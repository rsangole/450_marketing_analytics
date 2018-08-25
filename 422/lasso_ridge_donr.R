glmnGrid <- expand.grid(alpha = c(0,  .2, .4, .6, .8, 1),
                        lambda = seq(.001, .2, length = 15))
set.seed(1)
glmnFit <- train(x = donr_training[,ungroupedTransformedColumns.donr] %>% dplyr::select(-donr),
                 y = donr_training$donr,
                 method = "glmnet",
                 tuneGrid = glmnGrid,
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = ctrl)
glmnFit

glmnResult <- merge(glmnFit$pred,  glmnFit$bestTune)
glmnetCM <- confusionMatrix(glmnFit, norm = "none")
glmnetCM

glmnetRoc <- roc(response = glmnResult$obs,
                 predictor = glmnResult$Non.Donor,
                 levels = levels(glmnResult$obs))

glmnFit0 <- glmnFit
glmnFit0$results$lambda <- format(round(glmnFit0$results$lambda, 3))

glmnPlot <- plot(glmnFit0,
                 plotType = "level",
                 cuts = 15,
                 scales = list(x = list(rot = 90, cex = .65)))

update(glmnPlot,
       ylab = "Mixing Percentage\nRidge <---------> Lasso",
       sub = "",
       main = "Area Under the ROC Curve",
       xlab = "Amount of Regularization")

plot(glmnetRoc, type = "s", legacy.axes = TRUE)

glmnFit$bestTune

glmBestFit <- train(x = donr_training[,ungroupedTransformedColumns.donr] %>%
                        dplyr::select(-donr),
                 y = donr_training$donr,
                 method = "glmnet",
                 tuneGrid = data.frame(alpha=1, lambda=0.001),
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = ctrl)
