validating_fits <- data.frame(obs = damt_validating$damt)

#Datasets
obs = damt_validating$damt
damt_validating.groupedTrans <- data.frame(damt_validating[,groupedTransformedColumns.damt] %>%
    dplyr::select(-damt))
damt_validating.groupedTransSq <- data.frame(damt_validating[,groupedTransSqColumns.damt] %>%
                                               dplyr::select(-damt))
damt_validating.ungroupedTrans <- data.frame(damt_validating[,ungroupedTransformedColumns.damt] %>%
    dplyr::select(-damt))

#PCR
validating_fits$PCR.UnGrp <-  predict(pcrFit.UT, damt_validating.ungroupedTrans)
validating_fits$PCR.Grp <-  predict(pcrFit.GT, damt_validating.groupedTrans)

#PLS
validating_fits$PLS.UnGrp <-  predict(plsFit.UT, damt_validating.ungroupedTrans)
validating_fits$PLS.Grp <-  predict(plsFit.GT, damt_validating.groupedTrans)

#Boosted Model
validating_fits$Boost.Grp <-  predict(boostFit.GT, damt_validating.groupedTrans)
validating_fits$Boost.GrpSq <-  predict(boostFit.GS, damt_validating.groupedTransSq)
validating_fits$Boost.UnGrp <-  predict(boostFit.UT, damt_validating.ungroupedTrans)

#Bagged Model
validating_fits$Bagged.Grp <-  predict(bagFit.GT, damt_validating.groupedTrans)
validating_fits$Bagged.UngGrp <-  predict(bagFit.UT, damt_validating.ungroupedTrans)

#RF
validating_fits$RF.Grp <-  predict(rfFit.GT, damt_validating.groupedTrans)
validating_fits$RF.Grp.CV <-  predict(rfFit.GT.cv, damt_validating.groupedTrans)
validating_fits$RF.UnGrp <-  predict(rfFit.UT, damt_validating.ungroupedTrans)
validating_fits$RF.GrpRaw <-  predict(rfFit.GT.noCenterScale, damt_validating.groupedTrans)

#Nnet
validating_fits$NeuralNet.Grp <-  predict(nnetFit, damt_validating.ungroupedTrans)

#GAM
validating_fits$GAM.Grp <-  predict(gamFit.GT, damt_validating.groupedTrans)
validating_fits$GAM.GrpSq <-  predict(gamFit.GS, damt_validating.groupedTransSq)
validating_fits$GAM.Cust <-  predict(gamFit, damt_validating.groupedTransSq)
validating_fits$GAM.CustRed <-  predict(gamFit2, damt_validating.groupedTransSq)

MPE_Comparison<-
    map_dbl(.x = validating_fits[,-1],.f = ~mean((obs-.x)^2))
MPE_ComparisonSD<-
    map_dbl(.x = validating_fits[,-1],.f = ~sd((obs-.x)^2)/sqrt(nrow(validating_fits)))

dotplot(sort(MPE_Comparison,decreasing = T),
        xlab = 'Mean Prediction Error - Validation Set',
        main = '`damt` Model Comparison - Mean Prediction Errors : Validation Data')

MPEdf <- data.frame(MPE_Comparison,MPE_ComparisonSD,Model=names(MPE_Comparison))

ul=MPEdf$MPE_Comparison+MPEdf$MPE_ComparisonSD
ll=MPEdf$MPE_Comparison-MPEdf$MPE_ComparisonSD
dotplot(reorder(Model,MPE_Comparison)~MPE_Comparison,
        data = MPEdf,
        xlab = 'Mean Prediction Error - Validation Set',
        main = '`damt` Model Comparison - Mean Prediction Errors : Validation Data',
        col='red',
        ul=MPEdf$MPE_Comparison+MPEdf$MPE_ComparisonSD,
        ll=MPEdf$MPE_Comparison-MPEdf$MPE_ComparisonSD,
        xlim=c((min(ll)-0.5*min(ll)),(max(ul)+0.5*min(ul))),
        panel=
            function(x,y,ul,ll,...){
                panel.dotplot(x,y,...)
                panel.arrows(
                    x0=ul,
                    y0=as.numeric(y),
                    x1=ll,
                    y1=as.numeric(y),
                    code=3,
                    angle=90, length=0.05,
                    col='black',
                    alpha=.7
                )
            })
DonationEstimations <-
    map_dbl(validating_fits[,-1],sum)

barchart(sort(DonationEstimations,decreasing = F),
        xlab = 'Sum of Estimated Donations',
        main = 'Total Estimated Donations Compared Across Models: Validation Data',
        horizontal=T)

forPlot <- data.frame(MPE_Comparison,
                      DonationEstimations) %>%
    rownames_to_column()

xyplot(
    DonationEstimations ~ MPE_Comparison,
    forPlot,
    main = 'Estimated Total $ vs Estimated Mean Prediction Error - Validation Data',
    xlab = 'Mean Prediction Error',
    ylab = 'Total Estimated Donations ($)',
    panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        ltext(
            x = x,
            y = y,
            labels = forPlot$rowname,
            pos = 4,
            offset = 1,
            cex = 0.8
        )
    },
    xlim = c(1.28,1.92)
)

forPlot %>%
    ggplot(aes(x=MPE_Comparison,y=DonationEstimations,label=rowname))+
    geom_text(size=3,angle = 40,check_overlap = T)+
    # xlim(1,1.85)+
    labs(x='Mean Prediction Error',y='Total Estimated Donations($)')


#GAM Plot
xyplot(GAM.CustRed~obs,
       validating_fits,
       aspect = 1,
       xlab = 'Observations (Y)',
       ylab = 'GAM.CustReduced (Yhat)',
       xlim=c(7,22),
       ylim=c(7,22),
       panel = function(x, y) {
           panel.xyplot(x, y)
           panel.abline(coef = c(0,1),
                        col = 'black')
       }
       )
