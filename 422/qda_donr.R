# QDA Models
## UnGrouped Raw Data
qdaFit.UR <-
    train(
        x = donr_training[, ungroupedColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'qda',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )

qdaFit.UR
summary(qdaFit.UR)
head(qdaFit.UR$pred)
confusionMatrix(data = qdaFit.UR$pred$pred,
                reference = qdaFit.UR$pred$obs,
                positive = 'Donor')
qdaROC.UR <- roc(
    response = qdaFit.UR$pred$obs,
    predictor = qdaFit.UR$pred$Non.Donor,
    levels = levels(qdaFit.UR$pred$obs)
)

# Log Data - Ungrouped
qdaFit.UL <-
    train(
        x = donr_training[, ungroupedTransformedColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'qda',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
confusionMatrix(data = qdaFit.UL$pred$pred,
                reference = qdaFit.UL$pred$obs,
                positive = 'Donor')
qdaROC.UL <- roc(
    response = qdaFit.UL$pred$obs,
    predictor = qdaFit.UL$pred$Non.Donor,
    levels = levels(qdaFit.UL$pred$obs)
)

# Squared Data - Ungrouped
qdaFit.USq <-
    train(
        x = donr_training[, ungroupedSquaredColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'qda',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
confusionMatrix(data = qdaFit.USq$pred$pred,
                reference = qdaFit.USq$pred$obs,
                positive = 'Donor')
qdaROC.USq <- roc(
    response = qdaFit.USq$pred$obs,
    predictor = qdaFit.USq$pred$Non.Donor,
    levels = levels(qdaFit.USq$pred$obs)
)

# ------------ COMPARE -----------------
plot(qdaROC.UR, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,lwd=.5)
plot(qdaROC.UL, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,add=T,lwd=.5)
plot(qdaROC.USq, type = "s", col = 'red', legacy.axes = T,add=T,lwd=.5)
