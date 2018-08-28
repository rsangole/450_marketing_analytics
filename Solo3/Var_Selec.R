library(doParallel) 
library(tidyverse)
library(janitor)
library(lattice)
library(Amelia)
library(rpart)
library(mice)
library(parallel)
library(caret)
library(pROC)
library(missForest)
library(FSelector)
library(furrr)
library(future)

# cl <- makePSOCKcluster(40)
# clusterEvalQ(cl, library(foreach));
# registerDoParallel(cl)
# stopCluster(cl)

doMC::registerDoMC(38)

source('lib/data_utils.R')
source('utils.R')
trellis.par.set(caretTheme())

# ----------------
raw_data = data_read()
df = data_prep_A(raw_data) %>% 
  data_prep_B() %>% 
  remove_buyerstatus()
glimpse(df)

predictor <- 'Y'; pos <- 'RESPONSE'

training_df <- df %>% filter(LEARNING_TEST == 'LEARNING') %>% dplyr::select(-LEARNING_TEST)
test_df <-  df %>% filter(LEARNING_TEST != 'LEARNING') %>% dplyr::select(-LEARNING_TEST)
train_part <- caret::createDataPartition(y = training_df$Y, times = 1, p = 0.7, list = F)
cal_df <- training_df[-train_part,]
training_df <- training_df[train_part,]
training_df$Y %>% table %>% prop.table()
cal_df$Y %>% table %>% prop.table()

# Numerical and categorical one variable models ------
allCats <- df %>% dplyr::select(-LEARNING_TEST,-Y) %>% dplyr::select_if(is.factor) %>% names
cat_Preds_train <- allCats %>% purrr::map_dfc(~mkPredC(training_df[[predictor]], 
                                                 training_df[[.x]], 
                                                 training_df[[.x]]))
names(cat_Preds_train) <- allCats
cat_Preds_cal <- allCats %>% purrr::map_dfc(~mkPredC(training_df[[predictor]], 
                                                 training_df[[.x]], 
                                                 cal_df[[.x]]))
names(cat_Preds_cal) <- allCats

search_cats <- tibble(param = allCats)
for(v in allCats){
  aucTrain <- round(calcAUC(cat_Preds_train[[v]], training_df[[predictor]]),3)
  aucCal <- round(calcAUC(cat_Preds_cal[[v]], cal_df[[predictor]]),3)
  message(glue::glue('TrainAUC for variable {v} is \t\t\t\t\t{aucTrain}'))
  search_cats[search_cats$param==v,'aucTrain'] <- aucTrain
  search_cats[search_cats$param==v,'aucCal'] <- aucCal
}
search_cats %>% arrange(desc(aucCal))
# xyplot(aucCal~aucTrain, search_cats, asp=1, panel = function(...){panel.xyplot(...);panel.abline(a=0,b=1,col='red')},main='Categorical Vars')
# Top 25 categorical variables"
(selected_cat_variables <- c(search_cats %>% arrange(desc(aucCal)) %>% head(20) %>% pull(param),"OLD_RESPONSE_11","OLD_RESPONSE_12","OLD_RESPONSE_13","OLD_RESPONSE_14","OLD_RESPONSE_15",'LONG','LAT'))

# --
allNums <- training_df %>% select_if(is.numeric) %>% names
num_Preds_train <- allNums %>% purrr::map_dfc(~mkPredN(training_df[[predictor]], 
                                                       training_df[[.x]], 
                                                       training_df[[.x]]))
names(num_Preds_train) <- allNums
num_Preds_cal <- allNums %>% purrr::map_dfc(~mkPredN(training_df[[predictor]], 
                                                     training_df[[.x]], 
                                                     cal_df[[.x]]))
names(num_Preds_cal) <- allNums
search_nums <- tibble(param = allNums)
for (v in allNums) {
  aucTrain <- round(calcAUC(num_Preds_train[[v]], training_df[[predictor]]),3)
  aucCal <- round(calcAUC(num_Preds_cal[[v]], cal_df[[predictor]]),3)
  search_nums[search_nums$param==v,'aucTrain'] <- aucTrain
  search_nums[search_nums$param==v,'aucCal'] <- aucCal
}
search_nums %>% arrange(desc(aucCal))
# xyplot(aucCal~aucTrain, search_nums, asp=1, panel = function(...){panel.xyplot(...);panel.abline(a=0,b=1,col='red')},main='Numerical Vars')
# Top 25 num variables"
(selected_num_variables <- c(search_nums %>% arrange(desc(aucCal)) %>% head(25) %>% pull(param),"MAILERS_IN_14","MAILERS_IN_15"))

search_cats %>% write_csv('search_cats.csv',col_names = T)
search_nums %>% write_csv('search_nums.csv',col_names = T)

final_selected_vars <- c(selected_cat_variables,selected_num_variables)
manual_remove <- 'PIXEL'
(final_selected_vars <- final_selected_vars[final_selected_vars!=manual_remove])

# --
tr_df_X <- training_df[final_selected_vars]; tr_df_Y <- training_df$Y
cal_df_X <- cal_df[final_selected_vars]; cal_df_Y <- cal_df$Y
test_df_X <- test_df[final_selected_vars]; test_df_Y <- test_df$Y

#----------------------ONE VAR DONE-----------------------

# Imputing with the FULL dataset on selected variables ----------

tr_df_X %>% select_if(is.factor) %>% purrr::map_int( ~ length(levels(.x))) %>% dotplot(
  panel = function(...) {
    panel.dotplot(...)
    panel.abline(v = 53)
  }
)
tr_df_X$TYPE <- 'TRAIN'
cal_df_X$TYPE <- 'CAL'
test_df_X$TYPE <- 'TEST'
dim(tr_df_X)[1]; dim(cal_df_X)[1]; dim(test_df_X)[1]
full_df <- tr_df_X %>% bind_rows(cal_df_X) %>% bind_rows(test_df_X)

numdf <- full_df %>% dplyr::select_if(is.numeric)
catdf <- full_df %>% dplyr::select_if(is.factor)
# missmap(catdf, y.cex = 0); missmap(numdf, y.cex = 0)
# doMC::registerDoMC(38)
# registerDoParallel(cl = makeCluster(40))
# mice_num <- mice(numdf, method = 'rf',m = 5)
# stopCluster(cl); registerDoSEQ();
# saveRDS(mice_num,'mice_num_new_er.rdata')
mice_num <- read_rds('mice_num_new_er.rdata')
# densityplot(mice_num)
mice_num_complete <- as_tibble(complete(mice_num))

catdf %>% map_int(~sum(is.na(.x)))
catdf %>% map(~table(., useNA = 'always'))

make_u <- function(x){
  x <- as.character(x)
  new_level <- x %>% table() %>% sort() %>% tail(1) %>% names()
  x[is.na(x)] <- new_level
  x <- as.factor(x)
  x
}  

catdf <- catdf %>% purrr::map_df(~make_u(.x))

full_df <- mice_num_complete %>% bind_cols(catdf) %>% bind_cols(full_df[,'TYPE'])

tr_df_X      <- full_df %>% filter(TYPE %in% c('TRAIN','CAL')) %>% select(-TYPE)
test_df_X    <- full_df %>% filter(TYPE %in% c('TEST')) %>% select(-TYPE)

tr_df_Y <- factor(c(tr_df_Y, cal_df_Y)-1, levels = c(0, 1),  labels = c('NORESPONSE', 'RESPONSE'))
nrow(tr_df_X)==length(tr_df_Y)

# ------- Models -------------

## - RF -->
post_process_rf <- function(X){
  print(X)
  plot(X)
  try(varImpPlot(X$finalModel))
  X_CM <- confusionMatrix(
    data = X$pred$pred,
    reference = X$pred$obs,
    positive = 'RESPONSE'
  )
  print(X_CM)
  X_ROC <- roc(
    response = X$pred$obs,
    predictor = X$pred$RESPONSE,
    levels = levels(X$pred$obs)
  )
  ROCplot(X_ROC)
}

ctrl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = T,
  method = 'cv',
  number = 10,
  savePredictions = T,
  allowParallel = T,
  verboseIter = T
)
ntree = 100
rfFit.cs <-
  train(
    x = tr_df_X,
    y = tr_df_Y,
    method = 'rf',
    metric = 'ROC',
    trControl = ctrl,
    ntree = ntree,
    tuneGrid = data.frame(mtry = c(5,7,10,15)),
    preProc = c('center', 'scale'),
    verbose = T
  )
post_process_rf(rfFit.cs)
rfFit.nocs <-
  train(
    x = tr_df_X,
    y = tr_df_Y,
    method = 'rf',
    metric = 'ROC',
    trControl = ctrl,
    ntree = ntree,
    tuneGrid = data.frame(mtry = c(5,7,10,15)),
    # preProc = c('center', 'scale'),
    verbose = T
  )
post_process_rf(rfFit.nocs)
rfFit.csy <-
  train(
    x = tr_df_X,
    y = tr_df_Y,
    method = 'rf',
    metric = 'ROC',
    trControl = ctrl,
    ntree = ntree,
    tuneGrid = data.frame(mtry = c(5,7,10,15)),
    preProc = c('center', 'scale', 'YeoJohnson'),
    verbose = T
  )
post_process_rf(rfFit.csy)
eTFit.cs <-
  train(
    x = tr_df_X,
    y = tr_df_Y,
    method = 'extraTrees',
    metric = 'ROC',
    trControl = ctrl,
    ntree = ntree,
    tuneGrid = expand.grid(mtry = c(5,7,10,15), numRandomCuts = c(1,2,3)),
    preProc = c('center', 'scale')
  )


## - NB -->
ctrl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = T,
  method = 'cv',
  number = 10,
  savePredictions = T,
  allowParallel = T,
  verboseIter = T
)
nbFit.cs <-
  train(
    x = tr_df_X,
    y = tr_df_Y,
    method = 'nb',
    metric = 'ROC',
    trControl = ctrl,
    ntree = ntree,
    tuneGrid = expand.grid(fL = 0, usekernel = c(T,F), adjust = c(.1,.2,.4,.6)),
    preProc = c('center', 'scale'),
    verbose = T
  )
nbFit.cs
plot(nbFit.cs)


## - NNET -->
cl <- makeCluster(spec = 30)
clusterEvalQ(cl, library(foreach));
registerDoParallel(cl)
ctrl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = T,
  method = 'cv',
  number = 10,
  savePredictions = T,
  allowParallel = T,
  verboseIter = T
)
avnnetFit.cs <-
  train(
    x = tr_df_X,
    y = tr_df_Y,
    method = 'avNNet',
    metric = 'ROC',
    trControl = ctrl,
    ntree = ntree,
    tuneGrid = expand.grid(size = c(1,2), decay = c(0.1,0.3), bag = c(T,F)),
    preProc = c('center', 'scale'),
    verbose = T
  )
avnnet2Fit.cs <-
  train(
    x = tr_df_X,
    y = tr_df_Y,
    method = 'avNNet',
    metric = 'ROC',
    trControl = ctrl,
    ntree = ntree,
    tuneGrid = expand.grid(size = c(2,4,6), decay = c(0.3), bag = c(F)),
    preProc = c('center', 'scale'),
    verbose = T
  )
avnnet3Fit.cs <-
  train(
    x = tr_df_X,
    y = tr_df_Y,
    method = 'avNNet',
    metric = 'ROC',
    trControl = ctrl,
    ntree = ntree,
    tuneGrid = expand.grid(size = c(2), decay = c(0.4,.6), bag = c(F)),
    preProc = c('center', 'scale'),
    verbose = T
  )
# saveRDS(avnnetFit.cs,'avnnetFit.cs')
# saveRDS(avnnet2Fit.cs,'avnnet2Fit.cs')
avnnetFit.cs
plot(avnnetFit.cs)
avnnet2Fit.cs
plot(avnnet2Fit.cs)
post_process_rf(plot(avnnet2Fit.cs))
stopCluster(cl)


# Compare models

model_list <- list(
  'RF - Std' = rfFit.cs,
  'RF - Std + YJ' = rfFit.csy,
  'RF - Raw' = rfFit.nocs,
  'NB' = nbFit.cs,
  'Avg NNet' = avnnet2Fit.cs
)

res <- resamples(x = model_list)
dotplot(res)
bwplot(res)

get_probs <- function(result_obj, new_data){
  predict(result_obj, new_data, type = 'prob')
}
plot_hists <- function(df,id){
  name <- df[[id,'name']]
  X <- df[[id,'probs']]
  X %>% rownames_to_column('rowid') %>% reshape2::melt(data=., value.name='prob') %>% lattice::histogram(~prob|variable,.,main=name)
}

result_df <- tibble(
  name = c('RF - Std' ,'RF - Std + YJ',  'RF - Raw' , 'NB' , 'Avg NNet'),
  result_obj = list(avnnet2Fit.cs,nbFit.cs,rfFit.cs,rfFit.csy,rfFit.nocs)
)
result_df$probs <- get_probs(result_df$result_obj, test_df_X)

# map(1:5, ~plot_hists(result_df,.x))

# predProbs <- extractProb(models = model_list, testX = tr_df_X, testY = tr_df_Y)
# plotClassProbs(predProbs, useObjects = TRUE)

p = 0.5
result_df <- result_df %>% 
  mutate(class_hat = purrr::map(probs, ~factor(.x$RESPONSE > p, c(F,T),labels = c('NORESPONSE','RESPONSE'))))

result_df <- result_df %>% 
  mutate(cm = purrr::map(class_hat, ~confusionMatrix(data = .x, reference = test_df_Y, positive = 'RESPONSE')),
         test_accuracy = purrr::map_dbl(cm, ~.x$overall[['Accuracy']]),
         test_kappa =    purrr::map_dbl(cm, ~.x$overall[['Kappa']]),
         no_info_rate =  purrr::map_dbl(cm, ~.x$overall[['AccuracyNull']]))

result_df

ggplot(result_df,aes(x=test_kappa,y=test_accuracy))+ggrepel::geom_label_repel(aes(label=name))+geom_hline(aes(yintercept = no_info_rate),lty=2)+theme_bw()+labs(title='Test Set Performance Metrics',subtitle='Dotted line: No Information Rate')


p_seq <- seq(0.1, to = 0.9, by = 0.1)
model_num <- 1:5
grid = expand.grid('p_seq' = p_seq, 'model_num' = model_num)
f_ <- function(x,i,metric) {
    q = factor((result_df[[i, 'probs']][, 2]) > x, c(F, T), labels = c('NORESPONSE', 'RESPONSE'))
    confusionMatrix(data = q,
                    reference = test_df_Y,
                    positive = 'RESPONSE')$overall[[metric]]
  }
for (position in 1:nrow(grid)) {
  grid[position,'accuracy'] = (f_(grid$p_seq[position], grid$model_num[position],'Accuracy'))
  grid[position,'Kappa'] = (f_(grid$p_seq[position], grid$model_num[position],'Kappa'))
}
grid
xyplot(accuracy~p_seq,grid,type='b',groups=model_num,lwd=2,cex=1.5,auto.key=list(columns=5,text=result_df$name))
xyplot(Kappa~p_seq,grid,type='b',groups=model_num,lwd=2,cex=1.5,auto.key=list(columns=5,text=result_df$name))
