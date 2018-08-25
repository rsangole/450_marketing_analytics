valid_fits_donr <- data.frame(obs = donr_validating$donr)

#Logit Models
valid_fits_donr$logitGR <- predict(logitFit.GR, donr_validating[, groupedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$logitGL <- predict(logitFit.GL, donr_validating[, groupedTransformedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$logitGSq <- predict(logitFit.GSq, donr_validating[, groupedSquaredColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$logitGSq2 <- predict(logitFit.GSq2, donr_validating[, groupedSquaredColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$logitGTS <- predict(logitFit.GTS, donr_validating[, groupedTransSqColumns.donr] %>% dplyr::select(-donr))


#LDA
valid_fits_donr$ldaUR  <- predict(ldaFit.UR,  donr_validating[, ungroupedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$ldaUL  <- predict(ldaFit.UL,  donr_validating[, ungroupedTransformedColumns.donr] %>%dplyr::select(-donr))
valid_fits_donr$ldaUSq <- predict(ldaFit.USq, donr_validating[, ungroupedSquaredColumns.donr] %>%dplyr::select(-donr))

#QDA
valid_fits_donr$qdaUR  <- predict(qdaFit.UR,  donr_validating[, ungroupedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$qdaUL  <- predict(qdaFit.UL,  donr_validating[, ungroupedTransformedColumns.donr] %>%dplyr::select(-donr))
valid_fits_donr$qdaUSq <- predict(qdaFit.USq, donr_validating[, ungroupedSquaredColumns.donr] %>%dplyr::select(-donr))

#KNN Models
valid_fits_donr$knnUR <- predict(knnFit.UR, donr_validating[, ungroupedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$knnUL <- predict(knnFit.UL, donr_validating[, ungroupedTransformedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$knnUSq <- predict(knnFit.USq, donr_validating[, ungroupedSquaredColumns.donr] %>% dplyr::select(-donr))

# #LASSO
# valid_fits_donr$lassoUL <- predict(glmBestFit,donr_validating[, ungroupedTransformedColumns.donr] %>% dplyr::select(-donr))

#RF
valid_fits_donr$rfUR <-  predict(rfFit.UR, donr_validating[, ungroupedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$rfGR <-  predict(rfFit.GR, donr_validating[, groupedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$rfUL <-  predict(rfFit.UL, donr_validating[, ungroupedTransformedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$rfGL <-  predict(rfFit.GL, donr_validating[, groupedTransformedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$rfUSq <- predict(rfFit.USq, donr_validating[, ungroupedSquaredColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$rfGSq <- predict(rfFit.GSq, donr_validating[, groupedSquaredColumns.donr] %>% dplyr::select(-donr))

# #BAGGING
valid_fits_donr$baggedUR <- predict(baggedFit.UR,donr_validating[, ungroupedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$baggedGR <- predict(baggedFit.GR,donr_validating[, groupedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$baggedUL <- predict(baggedFit.UL,donr_validating[, ungroupedTransformedColumns.donr] %>%dplyr::select(-donr))
valid_fits_donr$baggedGL <- predict(baggedFit.GL,donr_validating[, groupedTransformedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$baggedUSq <- predict(baggedFit.USq,donr_validating[, ungroupedSquaredColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$baggedGSq <- predict(baggedFit.GSq,donr_validating[, groupedSquaredColumns.donr] %>% dplyr::select(-donr))

#BOOSTING --- modify to include data.frame like line 42
# valid_fits_donr$boostUR <- predict(boostFit.UR,donr_validating[, ungroupedColumns.donr])
# valid_fits_donr$boostGR <- predict(boostFit.GR,donr_validating[, groupedColumns.donr] %>% dplyr::select(-donr))
# valid_fits_donr$boostUL <- predict(boostFit.UL,donr_validating[, ungroupedTransformedColumns.donr])
# valid_fits_donr$boostGL <- predict(boostFit.GL,donr_validating[, groupedTransformedColumns.donr] %>% dplyr::select(-donr))
valid_fits_donr$boostUSq <- predict(boostFit.USq,n.trees = 5000,
                                    newdata = data.frame(donr_validating[, ungroupedSquaredColumns.donr] %>%
                                                             dplyr::select(-donr)))
valid_fits_donr$boostGSq <- predict(boostFit.GSq,n.trees = 5000,
                                    newdata = data.frame(donr_validating[,groupedSquaredColumns.donr] %>%
                                                   dplyr::select(-donr)))
valid_fits_donr$boostGSqRed <- predict(boostFit.GSqRed,n.trees = 5000,
                                    newdata = data.frame(donr_validating[,groupedSquaredColumns.donr] %>%
                                                             dplyr::select(chld,hinc,reg,wrat,home,tdon,tgif,incm,tlag,npro,inca,plow,avhv,
                                                                           agif)))

overallAccuracies <-
    map_dbl(
        .x = valid_fits_donr %>% dplyr::select(-obs),
        ~ confusionMatrix(
            data = .x,
            reference = valid_fits_donr$obs,
            positive = 'Donor'
        )$overall[[1]]
    )
overallAccuracies
dotplot(1 - overallAccuracies %>% sort(),
        xlab = 'Misclassification Rate',
        main = 'Comparison of misclassification rates - Validation Set')


#
# MPE_Comparison<-
#     map_dbl(.x = valid_fits_donr[,-1],.f = ~mean((obs-.x)^2))
# MPE_ComparisonSD<-
#     map_dbl(.x = valid_fits_donr[,-1],.f = ~sd((obs-.x)^2)/sqrt(nrow(valid_fits_donr)))
#
# dotplot(sort(MPE_Comparison,decreasing = T),
#         xlab = 'Mean Prediction Error - Validation Set',
#         main = '`damt` Model Comparison - Mean Prediction Errors : Validation Data')
#
# MPEdf <- data.frame(MPE_Comparison,MPE_ComparisonSD,Model=names(MPE_Comparison))
#
# ul=MPEdf$MPE_Comparison+MPEdf$MPE_ComparisonSD
# ll=MPEdf$MPE_Comparison-MPEdf$MPE_ComparisonSD
# dotplot(reorder(Model,MPE_Comparison)~MPE_Comparison,
#         data = MPEdf,
#         xlab = 'Mean Prediction Error - Validation Set',
#         main = '`damt` Model Comparison - Mean Prediction Errors : Validation Data',
#         col='red',
#         ul=MPEdf$MPE_Comparison+MPEdf$MPE_ComparisonSD,
#         ll=MPEdf$MPE_Comparison-MPEdf$MPE_ComparisonSD,
#         xlim=c((min(ll)-0.5*min(ll)),(max(ul)+0.5*min(ul))),
#         panel=
#             function(x,y,ul,ll,...){
#                 panel.dotplot(x,y,...)
#                 panel.arrows(
#                     x0=ul,
#                     y0=as.numeric(y),
#                     x1=ll,
#                     y1=as.numeric(y),
#                     code=3,
#                     angle=90, length=0.05,
#                     col='black',
#                     alpha=.7
#                 )
#             })
# DonationEstimations <-
#     map_dbl(valid_fits_donr[,-1],sum)
#
# barchart(sort(DonationEstimations,decreasing = F),
#          xlab = 'Sum of Estimated Donations',
#          main = 'Total Estimated Donations Compared Across Models: Validation Data',
#          horizontal=T)
#
# forPlot <- data.frame(MPE_Comparison,
#                       DonationEstimations) %>%
#     rownames_to_column()
#
# xyplot(
#     DonationEstimations ~ MPE_Comparison,
#     forPlot,
#     main = 'Estimated Total $ vs Estimated Mean Prediction Error - Validation Data',
#     xlab = 'Mean Prediction Error',
#     ylab = 'Total Estimated Donations ($)',
#     panel = function(x, y, ...) {
#         panel.xyplot(x, y, ...)
#         ltext(
#             x = x,
#             y = y,
#             labels = forPlot$rowname,
#             pos = 4,
#             offset = 1,
#             cex = 0.8
#         )
#     },
#     xlim = c(1.28,1.92)
# )
#
# forPlot %>%
#     ggplot(aes(x=MPE_Comparison,y=DonationEstimations,label=rowname))+
#     geom_text(size=3,angle = 40,check_overlap = T)+
#     # xlim(1,1.85)+
#     labs(x='Mean Prediction Error',y='Total Estimated Donations($)')
#
#
# #GAM Plot
# xyplot(GAM.CustRed~obs,
#        valid_fits_donr,
#        aspect = 1,
#        xlab = 'Observations (Y)',
#        ylab = 'GAM.CustReduced (Yhat)',
#        xlim=c(7,22),
#        ylim=c(7,22),
#        panel = function(x, y) {
#            panel.xyplot(x, y)
#            panel.abline(coef = c(0,1),
#                         col = 'black')
#        }
# )
