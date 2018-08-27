library(doParallel) 
library(drake)
library(tidyverse)
library(janitor)
library(lattice)
library(Amelia)
library(rpart)
# library(rpart.plot)
# library(tree)
library(mice)
library(parallel)
library(caret)
library(pROC)
library(missForest)
library(FSelector)
library(furrr)
library(future)

source('lib/data_utils.R')


# -- functions ----
ROCplot <- function(x,t=0.5) {
  plot(
    x,
    print.thres = t,
    type = "S",
    print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
    print.thres.cex = 1,
    legacy.axes = TRUE
  )
}
calcAUC <- function(predcol, outcol){
  perf <- ROCR::performance(ROCR::prediction(predcol, outcol==pos),'auc')
  as.numeric(perf@y.values)
}
mkPredC <- function(outCol,varCol,appCol){
  ppos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pposWNA <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol),varCol)
  pposWV <- (vTab[pos,]+1e-3*ppos)/(colSums(vTab)+1e-3)
  pred <- pposWV[appCol]
  pred[is.na(appCol)] <- pposWNA
  pred[is.na(pred)] <- ppos
  tibble(X = pred)
}
mkPredN <- function(outcol,varcol,appcol){
  cuts <- unique(as.numeric(quantile(varcol,probs = seq(0,1,0.1),na.rm=T)))
  varc <- cut(varcol,cuts)
  appc <- cut(appcol,cuts)
  mkPredC(outcol,varc,appc)
}
# ----------------
raw_data = data_read()
df = data_prep_A(raw_data)

glimpse(df)

char_to_factor_vars <- df %>% select_if(is.character) %>% names
df[char_to_factor_vars] <- df[char_to_factor_vars] %>% purrr::map_df( ~ as.factor(.x))
glimpse(df)

predictor <- 'Y'
pos <- 'RESPONSE'

df$Y <- factor(
  df$Y,  levels = c(0, 1),  labels = c('NORESPONSE', 'RESPONSE')
)

allCats <- head(df %>% select_if(is.factor) %>% names,-2)

training_df <- df %>% filter(LEARNING_TEST == 'LEARNING') %>% dplyr::select(-LEARNING_TEST)
test_df <-  df %>% filter(LEARNING_TEST != 'LEARNING') %>% dplyr::select(-LEARNING_TEST)

train_part <- caret::createDataPartition(y = training_df$Y, times = 1, p = 0.7, list = F)
cal_df <- training_df[-train_part,]
training_df <- training_df[train_part,]

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
xyplot(aucCal~aucTrain, search_cats, asp=1, panel = function(...){panel.xyplot(...);panel.abline(a=0,b=1,col='red')})
# Top 25 categorical variables"
(selected_cat_variables <- search_cats %>% arrange(desc(aucCal)) %>% head(20) %>% pull(param))


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
xyplot(aucCal~aucTrain, search_nums, asp=1, panel = function(...){panel.xyplot(...);panel.abline(a=0,b=1,col='red')})
# Top 25 num variables"
(selected_num_variables <- search_nums %>% arrange(desc(aucCal)) %>% head(20) %>% pull(param))

(final_selected_vars <- c(selected_cat_variables,selected_num_variables))

manual_remove <- 'PIXEL'
final_selected_vars <- final_selected_vars[final_selected_vars!=manual_remove]

# --

glimpse(tr_df_X <- training_df[final_selected_vars])
glimpse(tr_df_Y <- training_df$Y)
glimpse(cal_df_X <- cal_df[final_selected_vars])
glimpse(cal_df_Y <- cal_df$Y)
test_df_X <- test_df[final_selected_vars]
test_df_Y <- test_df$Y

map_int(tr_df_X, ~sum(is.na(.x)))
map_int(cal_df_X, ~sum(is.na(.x)))
map_int(test_df_X, ~sum(is.na(.x)))


# Imputing with the FULL dataset...

tr_df_X %>% select_if(is.factor) %>% purrr::map_int( ~ length(levels(.x))) %>% dotplot(
  panel = function(...) {
    panel.dotplot(...)
    panel.abline(v = 53)
  }
)

dim(tr_df_X)[1]; dim(cal_df_X)[1]; dim(test_df_X)[1]
full_df <- tr_df_X %>% bind_rows(cal_df_X) %>% bind_rows(test_df_X)

numdf <- full_df %>% dplyr::select_if(is.numeric)
catdf <- full_df %>% dplyr::select_if(is.factor)

# mice_num <- mice(numdf, method = 'cart',m = 5) 
# saveRDS(mice_num,'mice_num_varsel160.rdata')
mice_num <- read_rds('mice_num_varsel160.rdata')
# densityplot(mice_num)
mice_num_complete <- as_tibble(complete(mice_num))

# cl <- makePSOCKcluster(40); clusterEvalQ(cl, library(foreach)); 
# # registerDoParallel(cores = 40)
# missForFit <- missForest(
#     xmis = as.data.frame(catdf),
#     verbose = T,ntree = 100, mtry = 9
#     # parallelize = 'forests'
#   )
# stopCluster(cl); registerDoSEQ(); 
# saveRDS(mice_cat,'mice_cat_varsel160.rdata')
# mice_cat_complete <- as_tibble(complete(mice_cat))

catdf %>% map_int(~sum(is.na(.x)))
catdf %>% map(~table(., useNA = 'always'))
  
  catdf$LOAN_AMT[is.na(catdf$LOAN_AMT)] <- 'U'
  catdf$ESTMORTAMTRNG[is.na(catdf$ESTMORTAMTRNG)] <- '1L'
  catdf$SALES[is.na(catdf$SALES)] <- 'U'
  catdf$ESTMORTPAYRNG <- as.character(catdf$ESTMORTPAYRNG)
  catdf$ESTMORTPAYRNG[is.na(catdf$ESTMORTPAYRNG)] <- 'U'
  catdf$ESTMORTPAYRNG <- as.factor(catdf$ESTMORTPAYRNG)
  catdf$ESTAERNG <- as.character(catdf$ESTAERNG)
  catdf$ESTAERNG[is.na(catdf$ESTAERNG)] <- 'U'
  catdf$ESTAERNG <- as.factor(catdf$ESTAERNG)
  catdf$PIXELGEO <- as.character(catdf$PIXELGEO)
  catdf$PIXELGEO[is.na(catdf$PIXELGEO)] <- 'U'
  catdf$PIXELGEO <- as.factor(catdf$PIXELGEO)
  catdf$SALES[is.na(catdf$SALES)] <- 'U'
  catdf$ZHMDECOR[is.na(catdf$ZHMDECOR)] <- 'U'
  catdf$ZCELL[is.na(catdf$ZCELL)] <- 'U'
  catdf$ZCOMMCON[is.na(catdf$ZCOMMCON)] <- 'U'
  catdf$ZMOBFIN[is.na(catdf$ZMOBFIN)] <- 'U'
  catdf$ZCLOTHNG[is.na(catdf$ZCLOTHNG)] <- 'U'
  catdf$ZMUSROCK[is.na(catdf$ZMUSROCK)] <- 'U'
  catdf$ZGARDEN[is.na(catdf$ZGARDEN)] <- 'U'
  catdf$ZMOBCLTH[is.na(catdf$ZMOBCLTH)] <- 'U'

full_df <- mice_num_complete %>% bind_cols(catdf)
tr_df_X    <- full_df[1:14001,]
cal_df_X   <- full_df[14002:5999,]
test_df_X  <- full_df[6000:10799,]

tr_df_X <- tr_df_X %>% bind_rows(cal_df_X)
tr_df_Y <- c(tr_df_Y, cal_df_Y)-1
tr_df_Y <- factor(tr_df_Y, levels = c(0, 1),  labels = c('NORESPONSE', 'RESPONSE'))

glimpse(tr_df_X)

# -- Models

ctrl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = T,
  method = 'cv',
  number = 10,
  # repeats = 5,
  savePredictions = T,
  allowParallel = T
)
ntree = 100
rfFit.GR <-
  train(
    x = tr_df_X,
    y = tr_df_Y,
    method = 'rf',
    metric = 'ROC',
    trControl = ctrl,
    ntree = ntree,
    tuneGrid = data.frame(mtry = c(30,50,80)),
    preProc = c('center', 'scale'),
    verbose = T
  )
rfFit.GR
plot(rfFit.GR)
varImpPlot(rfFit.GR$finalModel)
rfFit_CM <- confusionMatrix(
  data = rfFit.GR$pred$pred,
  reference = rfFit.GR$pred$obs,
  positive = 'RESPONSE'
)
rfFit_CM
rfROC.GR <- roc(
  response = rfFit.GR$pred$obs,
  predictor = rfFit.GR$pred$RESPONSE,
  levels = levels(rfFit.GR$pred$obs)
)
ROCplot(rfROC.GR)
p_fun <- function(object, newdata){predict(object, newdata=newdata, type="prob")[,2]}
ytrain <- as.numeric(training_df$Y)-1
ytest <- as.numeric(test_df$Y)-1
train_explainer_classif_rf <- explain(model = crfFit.GR$finalModel,
                                      label = "Par_RF",
                                      data = training_df[-118],
                                      y = ytrain)
# predict_function = p_fun)
variable_importance(train_explainer_classif_rf, type = 'raw') %>% plot()
variable_importance(train_explainer_classif_rf, type = 'ratio') %>% plot()
variable_importance(train_explainer_classif_rf, type = 'difference') %>% plot()
variable_importance(train_explainer_classif_rf, loss_function = loss_root_mean_square) %>% plot()
vi_classif_rf <- variable_importance(explainer_classif_rf, loss_function = loss_root_mean_square)