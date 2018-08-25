glmnGrid <- expand.grid(alpha = c(0,.05,0.1,.15, .2, .4, .6, .8, 1),
                        lambda = seq(.00001,.1, length = 50))
set.seed(1)
glmnFit <- train(x = damt_training[,ungroupedTransformedColumns.damt] %>%
                     dplyr::select(-damt) %>%
                     dplyr::select(-one_of(tooHigh)),
                 y = damt_training$damt,
                 method = "glmnet",
                 tuneGrid = glmnGrid,
                 preProc = c("center", "scale")
                 # trControl = ctrl_lgocv
                 )
glmnFit

plot(glmnFit)

glmnFit0 <- glmnFit
glmnFit0$results$lambda <- format(round(glmnFit0$results$lambda, 3))

glmnPlot <- plot(glmnFit0,
                 plotType = "level",
                 metric = 'RMSE',
                 cuts = 30,
                 contour=T,
                 pretty = T,
                 scales = list(x = list(rot = 90, cex = .65)),
                 col.regions=cm.colors(100)
                 )
update(glmnPlot,
       ylab = "Mixing Percentage\nRidge <---------> Lasso",
       sub = "Ungrouped Transformed Variables w/o High Corr Variables",
       main = "RMSE",
       xlab = "Amount of Regularization")

glmnFit$bestTune

glmnFit$finalModel %>% plot(xvar='lambda')

# glmBestFit <- train(x = damt_training[,ungroupedTransformedColumns.damt] %>%
#                         dplyr::select(-damt) %>%
#                         dplyr::select(-one_of(tooHigh)),
#                     y = damt_training$damt,
#                     method = "glmnet",
#                     tuneGrid = data.frame(alpha=.2, lambda=0.0001),
#                     preProc = c("center", "scale"),
#                     trControl = ctrl_lgocv)
#
#
# plot(predict(glmBestFit),damt_training$damt)
#
#
#
# lassoFit <- train(x = damt_training[,ungroupedTransformedColumns.damt] %>%
#                       dplyr::select(-damt) %>%
#                       dplyr::select(-one_of(tooHigh)),
#                   y = damt_training$damt,
#                   method = "glmnet",
#                   tuneGrid = data.frame(alpha=1,lambda = 10^seq(-5,-15,length=20)),
#                   preProc = c("center", "scale"),
#                   trControl = ctrl_rcv)
#
# lassoFit %>% plot()
#
# lassoFit$finalModel$tuneValue %>% plot()



# # LASSO -------------------------------------------------------------------
# # KISS!
# grid <- 10^seq(10,-2,length=100)
# lasso.mod <- glmnet(x = as.matrix(damt_training[,ungroupedTransformedColumns.damt] %>%
#                         dplyr::select(-damt) %>%
#                         dplyr::select(-one_of(tooHigh))),
#                     y = damt_training$damt,
#                     alpha = 1,
#                     lambda = grid)
# plot(lasso.mod,xvar = 'lambda')
#
# cv.output <- glmnet::cv.glmnet(x = as.matrix(damt_training[,ungroupedTransformedColumns.damt] %>%
#                                                  dplyr::select(-damt) %>%
#                                                  dplyr::select(-one_of(tooHigh))),
#                                y = damt_training$damt,
#                                alpha=1)
# plot(cv.output)
# bestlam <- cv.output$lambda.min
# bestlam
# lam.1se <- cv.output$lambda.1se
# lam.1se
#
# lasso.coef <- predict(lasso.mod,type = 'coefficients',s=lam.1se)
# barchart(lasso.coef[,1] %>% sort())
#
# #KISS!
#
#
# # RIDGE -------------------------------------------------------------------
# # KISS!
# grid <- 10^seq(4,-4,length=100)
# ridge.mod <- glmnet(x = as.matrix(damt_training[,ungroupedTransformedColumns.damt] %>%
#                                       dplyr::select(-damt) %>%
#                                       dplyr::select(-one_of(tooHigh))),
#                     y = damt_training$damt,
#                     alpha = 0,
#                     lambda = grid)
# plot(ridge.mod,xvar = 'lambda')
#
# cv.output <- glmnet::cv.glmnet(x = as.matrix(damt_training[,ungroupedTransformedColumns.damt] %>%
#                                                  dplyr::select(-damt) %>%
#                                                  dplyr::select(-one_of(tooHigh))),
#                                y = damt_training$damt,
#                                alpha=0,
#                                lambda = grid)
# plot(cv.output)
# bestlam <- cv.output$lambda.min
# bestlam
# lam.1se <- cv.output$lambda.1se
# lam.1se
#
# ridge.coef <- predict(ridge.mod,type = 'coefficients',s=lam.1se)
# barchart(ridge.coef[,1] %>% sort())
# #KISS!
