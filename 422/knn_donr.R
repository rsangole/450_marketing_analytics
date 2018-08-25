#KNN MODEL
## Raw Standardized data
set.seed(1)
levels(donr_training$donr) #glm treats 2nd factor as event of interest

knnFit.UR <-
    train(
        x = donr_training[, ungroupedColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'knn',
        metric = 'ROC',
        tuneGrid = data.frame(k=1:15),
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
knnFit.UR
plot(knnFit.UR)
summary(knnFit.UR)
confusionMatrix(data = knnFit.UR$pred$pred,
                reference = knnFit.UR$pred$obs,
                positive = 'Donor')
knnROC.UR <- roc(
    response = knnFit.UR$pred$obs,
    predictor = knnFit.UR$pred$Non.Donor,
    levels = levels(knnFit.UR$pred$obs)
)
ROCplot(knnROC.UR)

## LOG Standardized Data
set.seed(1)
knnFit.UL <-
    train(
        x = donr_training[, ungroupedTransSqColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'knn',
        metric = 'ROC',
        tuneGrid = data.frame(k=1:12),
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
knnFit.UL
plot(knnFit.UL)
summary(knnFit.UL)
confusionMatrix(data = knnFit.UL$pred$pred,
                reference = knnFit.UL$pred$obs,
                positive = 'Donor')
knnROC.UL <- roc(
    response = knnFit.UL$pred$obs,
    predictor = knnFit.UL$pred$Non.Donor,
    levels = levels(knnFit.UL$pred$obs)
)
ROCplot(knnROC.UL)

## Squared terms
knnFit.USq <-
    train(
        x = donr_training[, ungroupedSquaredColumns.donr] %>%
            dplyr::select(-donr),
        y = donr_training$donr,
        method = 'knn',
        metric = 'ROC',
        tuneGrid = data.frame(k=1:12),
        trControl = ctrl2,
        preProc = c('center', 'scale')
    )
knnFit.USq
summary(knnFit.USq)
confusionMatrix(data = knnFit.USq$pred$pred,
                reference = knnFit.USq$pred$obs,
                positive = 'Donor')
knnROC.USq <- roc(
    response = knnFit.USq$pred$obs,
    predictor = knnFit.USq$pred$Non.Donor,
    levels = levels(knnFit.USq$pred$obs)
)


# ------------ COMPARE -----------------
plot(knnROC.UL, type = "s", col = 'gray', legacy.axes = T,lwd=1)
plot(knnROC.UR, type = "s", col = 'gray', legacy.axes = T,add=T,lwd=1)
plot(knnROC.USq, type = "s", col = 'red', legacy.axes = T,add=T,lwd=1)
