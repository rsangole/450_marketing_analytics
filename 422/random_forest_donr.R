ntree <- 200

## UnGrouped Raw Data
rfFit.UR <-
    train(
        x = donr_training[, ungroupedColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl2,
        ntree = ntree,
        tuneGrid = data.frame(mtry = sqrt(length(ungroupedColumns.donr))),
        preProc = c('center', 'scale')
    )

rfFit.UR
plot(rfFit.UR$finalModel)
legend(x = 'topright',legend = c('OOB','Non.Donor','Donor'),
       col = c('Black','Red','Green'),lty = c(1,1,1))
varImpPlot(rfFit.UR$finalModel)
dim(rfFit.UR$pred)
rfCM.UR <- confusionMatrix(
    data = rfFit.UR$pred$pred,
    reference = rfFit.UR$pred$obs,
    positive = 'Donor'
)
rfROC.UR <- roc(
    response = rfFit.UR$pred$obs,
    predictor = rfFit.UR$pred$Non.Donor,
    levels = levels(rfFit.UR$pred$obs)
)

## Grouped Raw Data
rfFit.GR <-
    train(
        x = donr_training[, groupedColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl2,
        ntree = ntree,
        tuneGrid = data.frame(mtry = sqrt(length(groupedColumns.donr))),
        preProc = c('center', 'scale')
    )

rfFit.GR
plot(rfFit.GR$finalModel)
legend(x = 'topright',legend = c('OOB','Non.Donor','Donor'),col = c('Black','Red','Green'),lty = c(1,1,1))
varImpPlot(rfFit.GR$finalModel)
rfCM.FR <- confusionMatrix(
    data = rfFit.GR$pred$pred,
    reference = rfFit.GR$pred$obs,
    positive = 'Donor'
)
rfROC.GR <- roc(
    response = rfFit.GR$pred$obs,
    predictor = rfFit.GR$pred$Non.Donor,
    levels = levels(rfFit.GR$pred$obs)
)

## UnGrouped Sq Data
rfFit.USq <-
    train(
        x = donr_training[, ungroupedSquaredColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl2,
        ntree = ntree,
        tuneGrid = data.frame(mtry = sqrt(length(ungroupedSquaredColumns.donr))),
        preProc = c('center', 'scale')
    )
plot(rfFit.USq$finalModel,
     main = 'random forest Fit - Train Errors')
legend(x = 'topright',legend = c('OOB','Non.Donor','Donor'),
       col = c('Black','Red','Green'),lty = c(1,1,1))
varImpPlot(rfFit.USq$finalModel,
           main = 'random forest Fit - UnGrouped')
rfCM.USq <- confusionMatrix(
    data = rfFit.USq$pred$pred,
    reference = rfFit.USq$pred$obs,
    positive = 'Donor'
)
rfROC.USq <- roc(
    response = rfFit.USq$pred$obs,
    predictor = rfFit.USq$pred$Non.Donor,
    levels = levels(rfFit.USq$pred$obs)
)

## Grouped Sq Data
rfFit.GSq <-
    train(
        x = donr_training[, groupedSquaredColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl2,
        ntree = ntree,
        tuneGrid = data.frame(mtry = sqrt(length(groupedSquaredColumns.donr))),
        preProc = c('center', 'scale')
    )

rfFit.GSq
plot(rfFit.GSq$finalModel)
legend(x = 'topright',legend = c('OOB','Non.Donor','Donor'),
       col = c('Black','Red','Green'),lty = c(1,1,1))
varImpPlot(rfFit.GSq$finalModel)
rfCM.GSq <- confusionMatrix(
    data = rfFit.GSq$pred$pred,
    reference = rfFit.GSq$pred$obs,
    positive = 'Donor'
)
rfROC.GSq <- roc(
    response = rfFit.GSq$pred$obs,
    predictor = rfFit.GSq$pred$Non.Donor,
    levels = levels(rfFit.GSq$pred$obs)
)

## Ungrouped Log Data
rfFit.UL <-
    train(
        x = donr_training[, ungroupedTransformedColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl2,
        ntree = ntree,
        tuneGrid = data.frame(mtry = sqrt(length(ungroupedTransformedColumns.donr))),
        preProc = c('center', 'scale')
    )

rfFit.UL
plot(rfFit.UL$finalModel)
legend(x = 'topright',legend = c('OOB','Non.Donor','Donor'),col = c('Black','Red','Green'),lty = c(1,1,1))
varImpPlot(rfFit.UL$finalModel)
rfCM.UL <- confusionMatrix(
    data = rfFit.UL$pred$pred,
    reference = rfFit.UL$pred$obs,
    positive = 'Donor'
)
rfROC.UL <- roc(
    response = rfFit.UL$pred$obs,
    predictor = rfFit.UL$pred$Non.Donor,
    levels = levels(rfFit.UL$pred$obs)
)

## Grouped Log Data
rfFit.GL <-
    train(
        x = donr_training[, groupedTransformedColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'rf',
        metric = 'ROC',
        trControl = ctrl2,
        ntree = ntree,
        tuneGrid = data.frame(mtry = sqrt(length(groupedTransformedColumns.donr))),
        preProc = c('center', 'scale')
    )

rfFit.GL
plot(rfFit.GL$finalModel)
legend(x = 'topright',legend = c('OOB','Non.Donor','Donor'),col = c('Black','Red','Green'),lty = c(1,1,1))
varImpPlot(rfFit.GL$finalModel)
rfCM.GL <- confusionMatrix(
    data = rfFit.GL$pred$pred,
    reference = rfFit.GL$pred$obs,
    positive = 'Donor'
)
rfROC.GL <- roc(
    response = rfFit.GL$pred$obs,
    predictor = rfFit.GL$pred$Non.Donor,
    levels = levels(rfFit.GL$pred$obs)
)

# ------------ COMPARE -----------------
plot(rfROC.UR, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,lwd=1)
plot(rfROC.GR, type = "s", col = 'forestgreen', legacy.axes = T,add=T,lwd=1)
plot(rfROC.UL, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,add=T,lwd=1)
plot(rfROC.GL, type = "s", col = 'darkblue', legacy.axes = T,add=T,lwd=1)
plot(rfROC.USq, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,add=T,lwd=1)
plot(rfROC.GSq, type = "s", col = 'red', legacy.axes = T,add=T,lwd=1)
