#LOGIT MODEL
## Raw Standardized data
set.seed(1)
levels(donr_training$donr) #glm treats 2nd factor as event of interest

logitFit.GR <-
    train(
        x = donr_training[, groupedColumns.donr] %>%
            dplyr::select(-donr,-avhv,-genf,-inca,-tgif,-lgif,-rgif,-agif),
        y = donr_training$donr,
        method = 'glm',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
logitFit.GR #.9665ROC, .9014Accuracy
summary(logitFit.GR)
confusionMatrix(data = logitFit.GR$pred$pred,
                reference = logitFit.GR$pred$obs,
                positive = 'Donor')
logitROC.GR <- roc(
    response = logitFit.GR$pred$obs,
    predictor = logitFit.GR$pred$Non.Donor,
    levels = levels(logitFit.GR$pred$obs)
)
ROCplot(logitROC.GR)

## LOG Standardized Data
set.seed(1)
logitFit.GL <-
    train(
        x = donr_training[, groupedTransformedColumns.donr] %>%
            dplyr::select(-donr,-log.avhv,-genf,-log.inca,-log.lgif,-log.rgif,-log.agif,-log.plow),
        y = donr_training$donr,
        method = 'glm',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
logitFit.GL
summary(logitFit.GL)
confusionMatrix(data = logitFit.GL$pred$pred,
                reference = logitFit.GL$pred$obs,
                positive = 'Donor')
logitROC.GL <- roc(
    response = logitFit.GL$pred$obs,
    predictor = logitFit.GL$pred$Non.Donor,
    levels = levels(logitFit.GL$pred$obs)
)

## TRANSFORMED SQ Data
set.seed(1)
logitFit.GTS <-
    train(
        x = donr_training[, groupedTransSqColumns.donr] %>%
            dplyr::select(-donr,-genf,-trans.avhv,
                          -trans.incm,-trans.inca,
                          -trans.plow,-trans.tgif,
                          -trans.lgif,-trans.rgif,
                          -trans.agif,-sq.trans.agif,
                          -sq.trans.avhv,-sq.trans.inca,
                          -sq.trans.npro,-sq.trans.lgif,
                          -sq.trans.rgif),
        y = donr_training$donr,
        method = 'glm',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
logitFit.GTS
summary(logitFit.GTS)
confusionMatrix(data = logitFit.GTS$pred$pred,
                reference = logitFit.GTS$pred$obs,
                positive = 'Donor')
logitROC.GTS <- roc(
    response  = logitFit.GTS$pred$obs,
    predictor = logitFit.GTS$pred$Non.Donor,
    levels = levels(logitFit.GTS$pred$obs)
)

## Squared terms
logitFit.GSq <-
    train(
        x = donr_training[, groupedSquaredColumns.donr] %>%
            dplyr::select(-donr,-genf,-avhv,-plow,-lgif,-sq.avhv,-sq.plow,
                          -sq.tgif,-sq.lgif,-sq.rgif,-sq.agif,
                          -inca,-rgif,-agif),
        y = donr_training$donr,
        method = 'glm',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
logitFit.GSq2 <-
    train(
        x = donr_training[, groupedSquaredColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'glm',
        metric = 'ROC',
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
logitFit.GSq
summary(logitFit.GSq)
confusionMatrix(data = logitFit.GSq2$pred$pred,
                reference = logitFit.GSq2$pred$obs,
                positive = 'Donor')
logitROC.GSq <- roc(
    response = logitFit.GSq2$pred$obs,
    predictor = logitFit.GSq2$pred$Non.Donor,
    levels = levels(logitFit.GSq2$pred$obs)
)


# ------------ COMPARE -----------------
plot(logitROC.GL, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,lwd=.5)
plot(logitROC.GR, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,add=T,lwd=.5)
plot(logitROC.GSq, type = "s", col = 'red', legacy.axes = T,add=T,lwd=.5)
plot(logitROC.GTS, type = "s", col = 'blue', legacy.axes = T,add=T,lwd=.5)


#----------------
