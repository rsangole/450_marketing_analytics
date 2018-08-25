ntree.boost <- 5000
int.depth.boost <- 1
shrinkage.boost <- 0.001

gbmGrid <- expand.grid(n.trees = c(2000,4000),
                       interaction.depth = c(1,2),
                       shrinkage = c(0.1,0.01),
                       n.minobsinnode = 10)

# ## UnGrouped Raw Data
# d <- data.frame(donr_training[, ungroupedColumns.donr] %>%
#     dplyr::select(-donr))
# boostFit.UR <-
#     train(
#         x = d,
#         y = donr_training$donr,
#         method = 'gbm',
#         metric = 'ROC',
#         trControl = ctrl2,
#         tuneGrid = data.frame(n.trees = ntree.boost,
#                               interaction.depth = int.depth.boost,
#                               shrinkage = shrinkage.boost,
#                               n.minobsinnode = 10),
#         preProc = c('center', 'scale')
#     )
# rm(d)
# boostFit.UR
# summary(boostFit.UR$finalModel)
# dim(boostFit.UR$pred)
# boostCM.UR <- confusionMatrix(
#     data = boostFit.UR$pred$pred,
#     reference = boostFit.UR$pred$obs,
#     positive = 'Donor'
# )
# boostROC.UR <- roc(
#     response = boostFit.UR$pred$obs,
#     predictor = boostFit.UR$pred$Non.Donor,
#     levels = levels(boostFit.UR$pred$obs)
# )
#
# ## UnGrouped Log Data
# d <- data.frame(donr_training[, ungroupedTransformedColumns.donr] %>%
#                     dplyr::select(-donr))
# boostFit.UL <-
#     train(
#         x = d,
#         y = donr_training$donr,
#         method = 'gbm',
#         metric = 'ROC',
#         trControl = ctrl,
#         tuneGrid = data.frame(n.trees = ntree.boost,
#                               interaction.depth = int.depth.boost,
#                               shrinkage = shrinkage.boost,
#                               n.minobsinnode = 10)
#         # ,preProc = c('center', 'scale')
#     )
# rm(d)
# boostFit.UL
# summary(boostFit.UL$finalModel)
# dim(boostFit.UL$pred)
# boostCM.UL <- confusionMatrix(
#     data = boostFit.UL$pred$pred,
#     reference = boostFit.UL$pred$obs,
#     positive = 'Donor'
# )
# boostROC.UL <- roc(
#     response = boostFit.UL$pred$obs,
#     predictor = boostFit.UL$pred$Non.Donor,
#     levels = levels(boostFit.UL$pred$obs)
# )
#
## UnGrouped Sq Data
X <- data.frame(donr_training[, ungroupedSquaredColumns.donr] %>%
                    dplyr::select(-donr))
Y <- donr_training$donr
set.seed(1)
boostFit.USq_ <-
    train(
        x=X,
        y=Y,
        method = 'gbm',
        metric = 'ROC',
        trControl = ctrl3,
        tuneGrid = gbmGrid,
        verbose = F,
        # tuneGrid = data.frame(n.trees = 5000,
        #                       interaction.depth = 2,
        #                       shrinkage = 0.01,
        #                       n.minobsinnode = 10),
        preProc = c('center', 'scale')
    )
# trellis.par.set(caretTheme())
# boostFit.GSq %>% plot()
plot(boostFit.USq_)
summary(boostFit.USq$finalModel)
dim(boostFit.USq$pred)
boostCM.USq <- confusionMatrix(
    data = boostFit.USq$pred$pred,
    reference = boostFit.USq$pred$obs,
    positive = 'Donor'
)
boostROC.USq <- roc(
    response = boostFit.USq$pred$obs,
    predictor = boostFit.USq$pred$Non.Donor,
    levels = levels(boostFit.USq$pred$obs)
)
ROCplot(boostROC.USq)

## Grouped Sq Data
X <- data.frame(donr_training[, groupedSquaredColumns.donr] %>%
                    dplyr::select(-donr))
Y <- donr_training$donr
# gbmGrid <- expand.grid(n.trees = c(2000,5000,7000),
#            interaction.depth = c(1,2),
#            shrinkage = .01,
#            n.minobsinnode = 10)

boostFit.GSq <-
    train(
        x=X,
        y=Y,
        method = 'gbm',
        metric = 'ROC',
        trControl = ctrl2,
        tuneGrid = data.frame(n.trees = 5000,
                              interaction.depth = 2,
                              shrinkage = 0.01,
                              n.minobsinnode = 10),
        preProc = c('center', 'scale')
    )
# trellis.par.set(caretTheme())
# boostFit.GSq %>% plot()
summary(boostFit.GSq$finalModel)
dim(boostFit.GSq$pred)
boostCM.GSq <- confusionMatrix(
    data = boostFit.GSq$pred$pred,
    reference = boostFit.GSq$pred$obs,
    positive = 'Donor'
)
boostROC.GSq <- roc(
    response = boostFit.GSq$pred$obs,
    predictor = boostFit.GSq$pred$Non.Donor,
    levels = levels(boostFit.GSq$pred$obs)
)
ROCplot(boostROC.GSq)

#####_----------------ISLR METHOD -------------------------
# d <- donr_training[, groupedSquaredColumns.donr]
# d$donr <- d$donr %>% unclass() - 1
# boostFit.GSqISLR <- gbm(
#     donr~.,
#     data = d,
#     n.trees = 5000,
#     shrinkage = 0.01,
#     interaction.depth = 2,
#     cv.folds = 10
# )
# boostPred <- predict(object = boostFit.GSqISLR,
#                      newdata = donr_validating[,groupedSquaredColumns.donr],
#                      n.trees = 5000)
# boostClass <- rep(0,length(boostPred))
# boostClass[boostPred>.5] <- 1
# table(boostClass,donr_validating$donr %>% unclass()-1) %>% confusionMatrix(positive='1')

# ------------ COMPARE -----------------
plot(boostROC.UR, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = T,lwd=1)
# plot(ldaROC.GR, type = "s", col = 'forestgreen', legacy.axes = T,add=T,lwd=1)
plot(boostROC.UL, type = "s", col = 'green', legacy.axes = T,add=T,lwd=1)
# plot(ldaROC.GL, type = "s", col = 'darkblue', legacy.axes = T,add=T,lwd=1)
plot(boostROC.USq, type = "s", col = 'blue', legacy.axes = T,lwd=1)
plot(boostROC.GSq, type = "s", col = 'red', legacy.axes = T,add=T,lwd=1)

summary(boostFit.GSq$finalModel,
        cBars=15,
        las=2,
        main='Top 15 var contributing to Grouped-Square Boosting Model')



# -----------reboost with smaller var set
## Grouped Sq Data
X <- data.frame(donr_training[, groupedSquaredColumns.donr] %>%
                    dplyr::select(chld,hinc,reg,wrat,home,tdon,tgif,incm,
                                  tlag,npro,inca,plow,avhv,agif))
Y <- donr_training$donr
# gbmGrid <- expand.grid(n.trees = c(2000,5000,7000),
#            interaction.depth = c(1,2),
#            shrinkage = .01,
#            n.minobsinnode = 10)

boostFit.GSqRed <-
    train(
        x=X,
        y=Y,
        method = 'gbm',
        metric = 'ROC',
        trControl = ctrl2,
        tuneGrid = data.frame(n.trees = 4000,
                              interaction.depth = 2,
                              shrinkage = 0.01,
                              n.minobsinnode = 10),
        preProc = c('center', 'scale')
    )
# trellis.par.set(caretTheme())
# boostFit.GSqRed %>% plot()
summary(boostFit.GSqRed$finalModel,cBars=15,las=2,main='Top 15 var contributing to Reduced Grouped-Square Boosting Model')
dim(boostFit.GSqRed$pred)
boostCM.GSqRed <- confusionMatrix(
    data = boostFit.GSqRed$pred$pred,
    reference = boostFit.GSqRed$pred$obs,
    positive = 'Donor'
)
boostROC.GSqRed <- roc(
    response = boostFit.GSqRed$pred$obs,
    predictor = boostFit.GSqRed$pred$Non.Donor,
    levels = levels(boostFit.GSqRed$pred$obs)
)
ROCplot(boostROC.GSqRed)



X <- data.frame(donr_training[, trans_columns])
Y <- donr_training$donr

boostFit.GTS <-
    train(
        x=X,
        y=Y,
        method = 'gbm',
        metric = 'ROC',
        trControl = ctrl2,
        tuneGrid = data.frame(n.trees = 4000,
                              interaction.depth = 2,
                              shrinkage = 0.01,
                              n.minobsinnode = 10),
        preProc = c('center', 'scale')
    )
