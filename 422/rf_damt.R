library(randomForest)

## UnGrouped Transformed Data
ntree <- 200
mtryGrid <- data.frame(mtry = 2:10)
set.seed(1)
rfFit.UT  <-
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
rfFit.UT
plot(rfFit.UT)
plot(rfFit.UT$finalModel)
varImpPlot(rfFit.UT$finalModel,main = 'Variable Importance - RF - Ungrouped Transformed Xs')

## Grouped transformed Data
ntree <- 200
mtryGrid <- data.frame(mtry = c(2:10))
set.seed(1)
rfFit.GT  <-
    train(
        x =damt_training[, groupedTransformedColumns.damt] %>% dplyr::select(-damt),
        y=damt_training$damt,
        method = 'rf',
        # trControl = ctrl_cv,
        ntree = ntree,
        tuneGrid = mtryGrid,
        preProc = c('center', 'scale')
    )
rfFit.GT
plot(rfFit.GT,metric = 'RMSE')
plot(rfFit.GT,metric = 'Rsquared')
plot(rfFit.GT$finalModel)
varImpPlot(rfFit.GT$finalModel,main = 'Variable Importance - RF - Grouped Transformed Xs')

## Grouped transformed Data - Using CV - NOT much different
# ntree <- 200
# mtryGrid <- data.frame(mtry = c(2:8))
# set.seed(1)
# rfFit.GT.cv  <-
#     train(
#         x =damt_training[, groupedTransformedColumns.damt] %>% dplyr::select(-damt),
#         y=damt_training$damt,
#         method = 'rf',
#         trControl = ctrl_cv,
#         ntree = ntree,
#         tuneGrid = mtryGrid,
#         preProc = c('center', 'scale')
#     )
# rfFit.GT.cv
# plot(rfFit.GT.cv,metric = 'RMSE')
# plot(rfFit.GT.cv,metric = 'Rsquared')
# plot(rfFit.GT.cv$finalModel)
# varImpPlot(rfFit.GT.cv$finalModel)
