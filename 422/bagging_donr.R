# bagging models
ntree.bag <- 500

## UnGrouped Raw Data
baggedFit.UR <-
    train(
        x = donr_training[, ungroupedColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl2,
        ntree = ntree.bag,
        tuneGrid = data.frame(mtry = length(ungroupedColumns.donr)),
        preProc = c('center', 'scale')
    )

baggedFit.UR
summary(baggedFit.UR)
plot(baggedFit.UR$finalModel)
varImpPlot(baggedFit.UR$finalModel)
head(baggedFit.UR$pred)
confusionMatrix(
    data = baggedFit.UR$pred$pred,
    reference = baggedFit.UR$pred$obs,
    positive = 'Donor'
)
baggedROC.UR <- roc(
    response = baggedFit.UR$pred$obs,
    predictor = baggedFit.UR$pred$Non.Donor,
    levels = levels(baggedFit.UR$pred$obs)
)

## Grouped Raw Data
baggedFit.GR <-
    train(
        x = donr_training[, groupedColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl,
        ntree = ntree.bag,
        tuneGrid = data.frame(mtry = length(groupedColumns.donr)),
        preProc = c('center', 'scale')
    )

baggedFit.GR
summary(baggedFit.GR)
plot(baggedFit.GR$finalModel)
varImpPlot(baggedFit.GR$finalModel)
head(baggedFit.GR$pred)
confusionMatrix(
    data = baggedFit.GR$pred$pred,
    reference = baggedFit.GR$pred$obs,
    positive = 'Donor'
)
baggedROC.GR <- roc(
    response = baggedFit.GR$pred$obs,
    predictor = baggedFit.GR$pred$Non.Donor,
    levels = levels(baggedFit.GR$pred$obs)
)

## UnGrouped Sq Data
baggedFit.USq <-
    train(
        x = donr_training[, ungroupedSquaredColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl2,
        ntree = ntree.bag,
        tuneGrid = data.frame(mtry = length(ungroupedSquaredColumns.donr)),
        preProc = c('center', 'scale')
    )

baggedFit.USq
summary(baggedFit.USq)
plot(baggedFit.USq$finalModel)
varImpPlot(baggedFit.USq$finalModel)
head(baggedFit.USq$pred)
confusionMatrix(
    data = baggedFit.USq$pred$pred,
    reference = baggedFit.USq$pred$obs,
    positive = 'Donor'
)
baggedROC.USq <- roc(
    response = baggedFit.USq$pred$obs,
    predictor = baggedFit.USq$pred$Non.Donor,
    levels = levels(baggedFit.USq$pred$obs)
)

## Grouped Sq Data
baggedFit.GSq <-
    train(
        x = donr_training[, groupedSquaredColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl2,
        ntree = ntree.bag,
        tuneGrid = data.frame(mtry = length(groupedSquaredColumns.donr)-1), #<<<<<<<<<<<<<
        preProc = c('center', 'scale')
    )

baggedFit.GSq
summary(baggedFit.GSq)
plot(baggedFit.GSq$finalModel,main = 'baggedFit - Training Set')
varImpPlot(baggedFit.GSq$finalModel,main = 'baggedFit - Training Set')
head(baggedFit.GSq$pred)
confusionMatrix(
    data = baggedFit.GSq$pred$pred,
    reference = baggedFit.GSq$pred$obs,
    positive = 'Donor'
)
baggedROC.GSq <- roc(
    response = baggedFit.GSq$pred$obs,
    predictor = baggedFit.GSq$pred$Non.Donor,
    levels = levels(baggedFit.GSq$pred$obs)
)

## Ungrouped Log Data
baggedFit.UL <-
    train(
        x = donr_training[, ungroupedTransformedColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl2,
        ntree = ntree.bag,
        tuneGrid = data.frame(mtry = length(ungroupedTransformedColumns.donr)-1),
        preProc = c('center', 'scale')
    )

baggedFit.UL
summary(baggedFit.UL)
plot(baggedFit.UL$finalModel)
varImpPlot(baggedFit.UL$finalModel)
head(baggedFit.UL$pred)
confusionMatrix(
    data = baggedFit.UL$pred$pred,
    reference = baggedFit.UL$pred$obs,
    positive = 'Donor'
)
baggedROC.UL <- roc(
    response = baggedFit.UL$pred$obs,
    predictor = baggedFit.UL$pred$Non.Donor,
    levels = levels(baggedFit.UL$pred$obs)
)
#
# ## Grouped Log Data
# baggedFit.GL <-
#     train(
#         x = donr_training[, groupedTransformedColumns.donr] %>%
#             dplyr::select(-donr),
#         y = donr_training$donr,
#         method = 'rf',
#         metric = 'ROC',
#         trControl = ctrl2,
#         ntree = ntree.bag,
#         tuneGrid = data.frame(mtry = length(groupedTransformedColumns.donr)-1),
#         preProc = c('center', 'scale')
#     )
#
# baggedFit.GL
# summary(baggedFit.GL)
# plot(baggedFit.GL$finalModel)
# varImpPlot(baggedFit.GL$finalModel)
# head(baggedFit.GL$pred)
# confusionMatrix(
#     data = baggedFit.GL$pred$pred,
#     reference = baggedFit.GL$pred$obs,
#     positive = 'Donor'
# )
# baggedROC.GL <- roc(
#     response = baggedFit.GL$pred$obs,
#     predictor = baggedFit.GL$pred$Non.Donor,
#     levels = levels(baggedFit.GL$pred$obs)
# )

## Grouped Boxcox Data
set.seed(1)
baggedFit.GL <-
    train(
        x = donr_training[, groupedTransSqColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl2,
        ntree = ntree.bag,
        tuneGrid = data.frame(mtry = length(groupedTransformedColumns.donr)-1),
        preProc = c('center', 'scale')
    )

baggedFit.GL
summary(baggedFit.GL)
plot(baggedFit.GL$finalModel,main = 'baggedFit - Training Set')
varImpPlot(baggedFit.GL$finalModel,main = 'baggedFit - Training Set')
head(baggedFit.GL$pred)
confusionMatrix(
    data = baggedFit.GL$pred$pred,
    reference = baggedFit.GL$pred$obs,
    positive = 'Donor'
)
baggedROC.GL <- roc(
    response = baggedFit.GL$pred$obs,
    predictor = baggedFit.GL$pred$Non.Donor,
    levels = levels(baggedFit.GL$pred$obs)
)

# ------------ COMPARE -----------------
plot(baggedROC.UR, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,lwd=1.0)
plot(baggedROC.GR, type = "s", col = 'forestgreen', legacy.axes = T,add=T,lwd=1.0)
plot(baggedROC.UL, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,add=T,lwd=1.0)
plot(baggedROC.GL, type = "s", col = 'red', legacy.axes = T,add=T,lwd=1.0)
plot(baggedROC.USq, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,add=T,lwd=1.0)
plot(baggedROC.GSq, type = "s", col = 'gray', legacy.axes = T,add=T,lwd=1.0)

