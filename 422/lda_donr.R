# LDA Models
## UnGrouped Raw Data
ldaFit.UR <-
    train(
        x = donr_training[, ungroupedColumns.donr] %>% dplyr::select(-donr),
        y = donr_training$donr,
        method = 'lda',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
ldaFit.UR
confusionMatrix(data = ldaFit.UR$pred$pred,
                reference = ldaFit.UR$pred$obs,
                positive = 'Donor')
ldaROC.UR <- roc(
    response = ldaFit.UR$pred$obs,
    predictor = ldaFit.UR$pred$Non.Donor,
    levels = levels(ldaFit.UR$pred$obs)
)

# Log Data - Ungrouped
ldaFit.UL <-
    train(
        x = donr_training[, ungroupedTransformedColumns.donr] %>% dplyr::select(-donr),
        y = donr_training$donr,
        method = 'lda',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
confusionMatrix(data = ldaFit.UL$pred$pred,
                reference = ldaFit.UL$pred$obs,
                positive = 'Donor')
ldaROC.UL <- roc(
    response = ldaFit.UL$pred$obs,
    predictor = ldaFit.UL$pred$Non.Donor,
    levels = levels(ldaFit.UL$pred$obs)
)

# Sq Data - Ungrouped
ldaFit.USq <-
    train(
        x = donr_training[, ungroupedSquaredColumns.donr] %>% dplyr::select(-donr),
        y = donr_training$donr,
        method = 'lda',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
confusionMatrix(data = ldaFit.USq$pred$pred,
                reference = ldaFit.USq$pred$obs,
                positive = 'Donor')
ldaROC.USq <- roc(
    response = ldaFit.USq$pred$obs,
    predictor = ldaFit.USq$pred$Non.Donor,
    levels = levels(ldaFit.USq$pred$obs)
)

# Ungrouped Trans Sq Data
ldaFit.UTS <-
    train(
        x = donr_training[, ungroupedTransSqColumns.donr] %>% dplyr::select(-donr),
        y = donr_training$donr,
        method = 'lda',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
confusionMatrix(data = ldaFit.UTS$pred$pred,
                reference = ldaFit.UTS$pred$obs,
                positive = 'Donor')
ldaROC.UTS<- roc(
    response = ldaFit.UTS$pred$obs,
    predictor = ldaFit.UTS$pred$Non.Donor,
    levels = levels(ldaFit.UTS$pred$obs)
)

# ------------ COMPARE -----------------
plot(ldaROC.UR, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,lwd=1)
plot(ldaROC.UL, type = "s", col = 'green', legacy.axes = T,add=T,lwd=1)
plot(ldaROC.USq, type = "s", col = 'blue', legacy.axes = T,add=T,lwd=1)
plot(ldaROC.UTS, type = "s", col = 'red', legacy.axes = T,add=T,lwd=1)
