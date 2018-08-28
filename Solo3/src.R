library(drake)
library(tidyverse)
library(janitor)
library(lattice)
library(Amelia)
library(rpart)
library(rpart.plot)
library(tree)
library(mice)
library(parallel)
library(caret)
library(pROC)
library('missForest')
library(FSelector)


# doMC::registerDoMC(36)

source('lib/data_utils.R')

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

subset_data <- read_rds(path = 'subsetdata_for_miceimpute.rdata')
imputed <- read_rds(path = 'df_imputed_miceobject.rdata')

imputed_subset <- mice::complete(imputed)
imputed_subset <- as_tibble(imputed_subset)
imputed_subset

imputed_subset$AD2AGE <- as.numeric(imputed_subset$AD2AGE)
imputed_subset$EXAGE <- as.numeric(imputed_subset$EXAGE)

still_has_na <- imputed_subset %>% map_dbl( ~ sum(is.na(.x)))
still_has_na[still_has_na != 0]

char_to_factor_vars <-
  imputed_subset %>% select_if(is.character) %>% names
imputed_subset[char_to_factor_vars] <-
  imputed_subset[char_to_factor_vars] %>% purrr::map_df( ~ as.factor(.x))

imputed_subset$CTINCIND <- NULL

imputed_subset %>% select_if(is.factor) %>% purrr::map_int( ~ length(levels(.x))) %>% dotplot(
  panel = function(...) {
    panel.dotplot(...)
    panel.abline(v = 53)
  }
)
# missForFit <-
#   missForest(
#     xmis = as.data.frame(imputed_subset),
#     verbose = T,
#     parallelize = 'forests'
#   )
# 
# saveRDS(missForFit,'missForFit.Rdata')
missForFit <- read_rds('missForFit.Rdata')
imputed_from_FF <- missForFit$ximp
imputed_from_FF <- as_tibble(imputed_from_FF)

raw_data = data_read()
df = data_prep_A(raw_data)
df_keep1 = data_prep_keep1(df)

imputed_from_FF <- imputed_from_FF %>% bind_cols(df_keep1[c('Y', 'LEARNING_TEST')])

# glimpse(imputed_subset)
# char_to_factor_vars <-
#   imputed_subset %>% select_if(is.character) %>% names
# imputed_subset[char_to_factor_vars] <-
#   imputed_subset[char_to_factor_vars] %>% purrr::map_df( ~ as.factor(.x))

imputed_from_FF$Y <- factor(
  imputed_from_FF$Y,
  levels = c(0, 1),
  labels = c('NORESPONSE', 'RESPONSE')
)

glimpse(imputed_from_FF)

training_df <-
  imputed_from_FF %>% filter(LEARNING_TEST == 'LEARNING') %>% dplyr::select(-LEARNING_TEST)
test_df <-
  imputed_from_FF %>% filter(LEARNING_TEST != 'LEARNING') %>% dplyr::select(-LEARNING_TEST)
dim(training_df)
dim(test_df)

# nearZeroVar(x = training_df, allowParallel = T, saveMetrics = T)

# modelmat <- model.matrix(Y ~ . - 1, training_df)
# 
# nzvAnalysis <- nearZeroVar(x = modelmat, allowParallel = T, saveMetrics = T)
# 
# nzvAnalysis[nzvAnalysis$zeroVar==T,] %>% rownames
# 
# modelmat <- as_tibble(modelmat) %>% 
#   select(-ESTMORTPAYRNG3B,-M_GRPTYPE_MEDIAND04)
# 
# corrmat <- cor(modelmat)
# corrmat <- as.matrix(corrmat)
# str(corrmat)
# corrplot::corrplot(corrmat,method = 'square',outline = F,addgrid.col = NA,tl.cex = .3, order = 'hclust',hclust.method = 'ward.D2')
# 
# # Variables with high pair-wise correlations
# tooHigh <-
#   (findCorrelation(
#     corrmat,
#     cutoff = .85,
#     verbose = T,
#     names = T
#   ))
# 
# modelmat[tooHigh] <- NULL

ctrl <- trainControl(
  # summaryFunction = twoClassSummary,
  classProbs = T,
  method = 'cv',
  number = 10,
  repeats = 5,
  # savePredictions = T,
  allowParallel = T
)
ntree = 100
## Grouped Raw Data
# rfFit.GR <-
#   train(
#     x = training_df %>% dplyr::select(-Y),
#     y = training_df$Y,
#     method = 'rf',
#     metric = 'ROC',
#     trControl = ctrl,
#     ntree = ntree,
#     tuneLength = 10,
#     # tuneGrid = data.frame(mtry = c(30,50,80)),
#     preProc = c('center', 'scale'),
#     verbose = T
#   )
# 
# rfFit.GR
# plot(rfFit.GR)
# plot(rfFit.GR$finalModel)
# legend(x = 'topright',legend = c('OOB','No Response','Response'),col = c('Black','Red','Green'),lty = c(1,1,1))
# varImpPlot(rfFit.GR$finalModel)
# rfCM.FR <- confusionMatrix(
#   data = rfFit.GR$pred$pred,
#   reference = rfFit.GR$pred$obs,
#   positive = 'RESPONSE'
# )
# rfCM.FR
# rfROC.GR <- roc(
#   response = rfFit.GR$pred$obs,
#   predictor = rfFit.GR$pred$RESPONSE,
#   levels = levels(rfFit.GR$pred$obs)
# )
# ROCplot(rfROC.GR)
# 
# library('DALEX')
# 
# p_fun <- function(object, newdata){predict(object, newdata=newdata, type="prob")[,2]}
# 
# yTest <- as.numeric(test_df$Y)-1
# 
# explainer_classif_rf <- DALEX::explain(rfFit.GR$finalModel, 
#                                        label = "rf",
#                                        data = test_df, 
#                                        y = yTest,
#                                        predict_function = p_fun)
# mp_classif_rf <- model_performance(explainer_classif_rf)
# plot(mp_classif_rf)
# plot(mp_classif_rf, geom = 'boxplot')
# vi_classif_rf <- variable_importance(explainer_classif_rf, loss_function = loss_root_mean_square)
# plot(vi_classif_rf)
# 

training_df %>% xtabs(~BUYER_STATUS+Y,.)
# Y
# BUYER_STATUS NORESPONSE RESPONSE
# ACTIVE         7163     1673
# INACTIVE       5960        0
# LAPSED         5204        0

# CRITICAL RULE!
# Removing all BUYER STATUS... since if its NOT "ACTIVE", we default to NORESPONSE
# 

training_df <- training_df %>% 
  dplyr::filter(BUYER_STATUS == 'ACTIVE') %>% 
  dplyr::select(-BUYER_STATUS)
test_df <- test_df %>% 
  dplyr::filter(BUYER_STATUS == 'ACTIVE') %>% 
  dplyr::select(-BUYER_STATUS)

# saveRDS(training_df,'start_from_here_train.rdata')
# saveRDS(test_df,'start_from_here_test.rdata')
training_df <- read_rds('start_from_here_train.rdata')
test_df <- read_rds('start_from_here_test.rdata')
# -- Random Forest -- 

## Grouped Raw Data
# ntree = 100
# rfFit.GR <-
#   train(
#     x = training_df %>% dplyr::select(-Y),
#     y = training_df$Y,
#     method = 'rf',
#     metric = 'ROC',
#     trControl = ctrl,
#     ntree = ntree,
#     tuneLength = 10,
#     # tuneGrid = data.frame(mtry = c(30,50,80)),
#     preProc = c('center', 'scale'),
#     verbose = T
#   )
# 
# rfFit.GR
# plot(rfFit.GR)
# plot(rfFit.GR$finalModel)
# legend(x = 'topright',legend = c('OOB','No Response','Response'),col = c('Black','Red','Green'),lty = c(1,1,1))
# varImpPlot(rfFit.GR$finalModel)
# rfCM.FR <- confusionMatrix(
#   data = rfFit.GR$pred$pred,
#   reference = rfFit.GR$pred$obs,
#   positive = 'RESPONSE'
# )
# rfCM.FR
# rfROC.GR <- roc(
#   response = rfFit.GR$pred$obs,
#   predictor = rfFit.GR$pred$RESPONSE,
#   levels = levels(rfFit.GR$pred$obs)
# )
# ROCplot(rfROC.GR)
# explainer_classif_rf <- DALEX::explain(rfFit.GR$finalModel, 
#                                        label = "rf",
#                                        data = test_df, 
#                                        y = yTest,
#                                        predict_function = p_fun)
# mp_classif_rf <- model_performance(explainer_classif_rf)
# plot(mp_classif_rf)
# plot(mp_classif_rf, geom = 'boxplot')
# vi_classif_rf <- variable_importance(explainer_classif_rf, loss_function = loss_root_mean_square)
# plot(vi_classif_rf)


# -- rf par --
library(doParallel) 
cl <- makePSOCKcluster(37)
clusterEvalQ(cl, library(foreach)); registerDoParallel(cl)
ctrl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = T,
  method = 'cv',
  number = 10,
  # repeats = 5,
  savePredictions = T,
  allowParallel = T
)
crfFit.GR <- train(
    y = training_df$Y,
    x = as.data.frame(training_df %>% dplyr::select(-Y)),
    method = 'parRF',
    metric = 'ROC',
    trControl = ctrl,
    tuneGrid = data.frame(mtry = c(30,50,80,100)),
    preProc = c('center', 'scale')
  )
stopCluster(cl); registerDoSEQ(); 
# saveRDS(crfFit.GR, 'crfFit.GR.rdata')
crfFit.GR <- read_rds('crfFit.GR.rdata')
crfFit.GR
plot(crfFit.GR)
# plot(crfFit.GR$finalModel)
# legend(x = 'topright',legend = c('OOB','No Response','Response'),col = c('Black','Red','Green'),lty = c(1,1,1))
varImpPlot(crfFit.GR$finalModel)
crfFit_CM <- confusionMatrix(
  data = crfFit.GR$pred$pred,
  reference = crfFit.GR$pred$obs,
  positive = 'RESPONSE'
)
crfFit_CM
crfROC.GR <- roc(
  response = crfFit.GR$pred$obs,
  predictor = crfFit.GR$pred$RESPONSE,
  levels = levels(crfFit.GR$pred$obs)
)
ROCplot(crfROC.GR)
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
# explainer_classif_rf <- explain(model = crfFit.GR$finalModel,
#                                 label = "Par_RF",
#                                 data = test_df[-118],
#                                 y = ytest,
#                                 predict_function = p_fun)
# mp_classif_rf <- model_performance(explainer_classif_rf)
# plot(mp_classif_rf)
# plot(mp_classif_rf, geom = 'boxplot')
# vi_classif_rf <- variable_importance(explainer_classif_rf, loss_function = loss_root_mean_square)
# plot(vi_classif_rf)

# # -- rf rules --
# ctrl <- trainControl(
#   # summaryFunction = twoClassSummary,
#   # classProbs = T,
#   method = 'cv',
#   number = 10,
#   repeats = 5,
#   # savePredictions = T,
#   allowParallel = T
# )
# rfrulesFit.GR <- train(
#   x = as.data.frame(training_df %>% dplyr::select(-Y)),
#   y = training_df$Y,
#   method = 'rfRules',
#   # metric = 'ROC',
#   trControl = ctrl,
#   tuneGrid = expand.grid(mtry = c(10,30,50,80),
#                          maxdepth = c(5,10,15,20)),
#   preProc = c('center', 'scale')
# )
# 
# crfFit.GR
# plot(crfFit.GR)





# -- deepboost --
library(deepboost) 
ctrl <- trainControl(
  # summaryFunction = twoClassSummary,
  # classProbs = T,
  method = 'cv',
  number = 10,
  # repeats = 5,
  savePredictions = T,
  allowParallel = F
)
tune <- expand.grid(
    num_iter = c(50,100,150), # (# Boosting Iterations)
    tree_depth = c(5,7,10), # (Tree Depth)
    beta = c(0.001,0.01), # (L1 Regularization)
    lambda = c(0.001,0.01), # (Tree Depth Regularization)
    loss_type = c('l') # (Loss)
)
library(doParallel)
deepboostFit <- list()
registerDoParallel(cores = 30)
deepboostFit <- foreach(i = 1:dim(tune)[1], .packages = 'deepboost', .combine = 'c', .inorder = T) %dopar% {
  deepboost(Y~.,data = training_df,
                               tree_depth = tune[i,'tree_depth'],
                               num_iter = tune[i,'num_iter'], 
                               beta = tune[i,'beta'], 
                               lambda = tune[i,'lambda'], 
                               loss_type = "l",
                               verbose = TRUE)
}
# for (i in 1:dim(tune)[1]) {
#   message('Processing: ',i)
#   deepboostFit[i] <- deepboost(Y~.,data = training_df,
#                                tree_depth = tune[i,'tree_depth'],
#                                num_iter = tune[i,'num_iter'], 
#                                beta = tune[i,'beta'], 
#                                lambda = tune[i,'lambda'], 
#                                loss_type = "l",
#                                verbose = TRUE)
# }
get_error <- function(obj){
  deepboost.evaluate(obj, obj@examples)[['error']]
}
tune$error <- deepboostFit %>% purrr::map_dbl(~get_error(.x))
tune %>% arrange(error)
lattice::xyplot(error~1:36, tune,type='b')
lattice::xyplot(error~num_iter|lambda+beta, groups=tree_depth,tune, type='p',auto.key=T)



# # # -- glmnet_h2o --
# ctrl <- trainControl(
#   summaryFunction = twoClassSummary,
#   classProbs = T,
#   method = 'cv',
#   number = 10,
#   # repeats = 5,
#   savePredictions = T,
#   allowParallel = T
# )
# glmnetFit.GR <- train(
#   x = as.data.frame(training_df %>% dplyr::select(-Y)),
#   y = training_df$Y,
#   method = 'glmnet_h2o',
#   metric = 'ROC',
#   trControl = ctrl,
#   tuneGrid = expand.grid(alpha = c(0,.25, 0.5, .75, 1),
#                          lambda = c(0.01, 0.1, 0.2)),
#   lambda_search = T,
#   early_stopping = T,
#   preProc = c('center', 'scale')
# )
# glmnetFit.GR
# 

# # -- Support Vector Machines --
ctrl <- trainControl(
  summaryFunction = twoClassSummary,
  method = 'cv',
  number = 10,
  savePredictions = T,
  allowParallel = T,
  classProbs = T
)
svmRadialFit.GR <- train(
  x = model.matrix(Y~.-1,training_df),
  y = training_df$Y,
  method = 'svmRadial',
  metric = 'ROC',
  trControl = ctrl,
  tuneGrid = expand.grid(.C = c(.25, .5, 1), .sigma = .05),
  preProc = c('center', 'scale')
)
svmRadialFit.GR



# # -- KNN --
ctrl <- trainControl(
  summaryFunction = twoClassSummary,
  method = 'cv',
  number = 10,
  savePredictions = T,
  allowParallel = T,
  classProbs = T
)
knnFit.GR <- train(
  # x = model.matrix(Y~.-1,training_df),
  # y = training_df$Y,
  x = as.data.frame(training_df %>% dplyr::select(-Y)),
  y = training_df$Y,
  method = 'knn',
  metric = 'ROC',
  trControl = ctrl,
  tuneGrid = expand.grid(.k = c(4*(0:5)+1,20*(1:5)+1,50*(2:5)+1)),
  preProc = c('center', 'scale')
)
svmRadialFit.GR


