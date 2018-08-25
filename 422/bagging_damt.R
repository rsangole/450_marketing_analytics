# library(randomForest)
#
# set.seed(1)
# ## BAGGING
# bag.damt <- randomForest(damt~.,
#                          data = damt_training[,groupedTransformedColumns.damt],
#                          mtry=length(groupedTransformedColumns.damt),
#                          importance=T)
# bag.damt
# plot(bag.damt)
# yhat.bag <- predict(bag.damt,newdata = damt_validating[,groupedTransformedColumns.damt])
# plot(yhat.bag,damt_validating$damt)
# abline(0,1)
# mean((yhat.bag-damt_validating$damt)^2)
#
# importance(bag.damt)
# varImpPlot(bag.damt)

library(randomForest)

## UnGrouped Transformed Data
ntree <- 200
mtryGrid <- data.frame(mtry=length(ungroupedTransformedColumns.damt))
set.seed(1)
bagFit.UT  <-
    train(
        x =damt_training[, ungroupedTransformedColumns.damt] %>%
            dplyr::select(-damt),
        y=damt_training$damt,
        method = 'rf',
        # trControl = ctrl_cv,
        ntree = ntree,
        tuneGrid = mtryGrid,
        preProc = c('center', 'scale')
    )
bagFit.UT
plot(bagFit.UT)
plot(bagFit.UT$finalModel)
varImpPlot(bagFit.UT$finalModel,main = 'Variable Importance - Bagging - Ungrouped Transformed Xs')


## Grouped transformed Data
ntree <- 200
mtryGrid <- data.frame(mtry=length(groupedTransformedColumns.damt))
set.seed(1)
bagFit.GT  <-
    train(
        x =damt_training[, groupedTransformedColumns.damt] %>% dplyr::select(-damt),
        y=damt_training$damt,
        method = 'rf',
        # trControl = ctrl_cv,
        ntree = ntree,
        tuneGrid = mtryGrid,
        preProc = c('center', 'scale')
    )
bagFit.GT
# plot(bagFit.GT,metric = 'RMSE')
# plot(bagFit.GT,metric = 'Rsquared')
plot(bagFit.GT$finalModel)
varImpPlot(bagFit.GT$finalModel,main = 'Variable Importance - Bagging - Grouped Transformed Xs')
