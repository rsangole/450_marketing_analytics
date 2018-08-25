validating_profits <- data.frame(logitGR=matrix(nrow = nrow(donr_validating)))

#Logit Models
validating_profits$logitGR <- predict(logitFit.GR, donr_validating[, groupedColumns.donr],            type = 'prob')[[2]]
validating_profits$logitGL <- predict(logitFit.GL, donr_validating[, groupedTransformedColumns.donr] ,type = 'prob')[[2]]
validating_profits$logitGSq   <- predict(logitFit.GSq, donr_validating[, groupedSquaredColumns.donr]  ,    type = 'prob')[[2]]
validating_profits$logitGSq2   <- predict(logitFit.GSq2, donr_validating[, groupedSquaredColumns.donr]  ,    type = 'prob')[[2]]
validating_profits$logitGTS   <- predict(logitFit.GTS, donr_validating[, groupedTransSqColumns.donr]  ,    type = 'prob')[[2]]

# LDA
validating_profits$ldaUR  <- predict(ldaFit.UR,  donr_validating[, ungroupedColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$ldaUL  <- predict(ldaFit.UL,  donr_validating[, ungroupedTransformedColumns.donr]%>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$ldaUSq <- predict(ldaFit.USq, donr_validating[, ungroupedSquaredColumns.donr]%>% dplyr::select(-donr) ,type = 'prob')[[2]]

# QDA
validating_profits$qdaUR  <- predict(qdaFit.UR,  donr_validating[, ungroupedColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$qdaUL  <- predict(qdaFit.UL,  donr_validating[, ungroupedTransformedColumns.donr]%>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$qdaUSq <- predict(qdaFit.USq, donr_validating[, ungroupedSquaredColumns.donr]%>% dplyr::select(-donr) ,type = 'prob')[[2]]

# KNN
validating_profits$knnUR  <- predict(knnFit.UR,  donr_validating[, ungroupedColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$knnUL  <- predict(knnFit.UL,  donr_validating[, ungroupedTransformedColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$knnUSq  <- predict(knnFit.USq,  donr_validating[, ungroupedSquaredColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]

# #LASSO
# validating_profits$lassoUL <- predict(glmBestFit,donr_validating[, ungroupedTransformedColumns.donr] %>% dplyr::select(-donr))

#RF
validating_profits$rfUR <-  predict(rfFit.UR, donr_validating[, ungroupedColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$rfGR <-  predict(rfFit.GR, donr_validating[, groupedColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$rfUL <-  predict(rfFit.UL, donr_validating[, ungroupedTransformedColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$rfGL <-  predict(rfFit.GL, donr_validating[, groupedTransformedColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$rfUSq <- predict(rfFit.USq, donr_validating[, ungroupedSquaredColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$rfGSq <- predict(rfFit.GSq, donr_validating[, groupedSquaredColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]

#BAGGING
validating_profits$baggedUR <- predict(baggedFit.UR,donr_validating[, ungroupedColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$baggedGR <- predict(baggedFit.GR,donr_validating[, groupedColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$baggedUL <- predict(baggedFit.UL,donr_validating[, ungroupedTransformedColumns.donr] %>%dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$baggedGL <- predict(baggedFit.GL,donr_validating[, groupedTransformedColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$baggedUSq <- predict(baggedFit.USq,donr_validating[, ungroupedSquaredColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]
validating_profits$baggedGSq <- predict(baggedFit.GSq,donr_validating[, groupedSquaredColumns.donr] %>% dplyr::select(-donr),type = 'prob')[[2]]

# #BOOSTING
validating_profits$boostUSq <- predict(boostFit.USq,n.trees = 5000,
                                       newdata = data.frame(donr_validating[, ungroupedSquaredColumns.donr]%>%
                                                                dplyr::select(-donr)),type = 'prob')[[2]]
validating_profits$boostGSq <- predict(boostFit.GSq,n.trees = 5000,
                                    newdata = data.frame(donr_validating[,groupedSquaredColumns.donr] %>%
                                                             dplyr::select(-donr)),type = 'prob')[[2]]
validating_profits$boostGSqRed <- predict(boostFit.GSqRed,n.trees = 5000,
                                       newdata = data.frame(donr_validating[,groupedSquaredColumns.donr]%>%
                                                                dplyr::select(chld,hinc,reg,wrat,home,tdon,tgif,incm,
                                                                              tlag,npro,inca,plow,avhv,agif)),
                                                            type = 'prob')[[2]]

profitResults <- map_df(validating_profits,profitCalculations)
profitResults$resulttype <- c('no_of_mailings','maxprofit','p_cutoff')
profitResults <- reshape2::melt(profitResults,id='resulttype') %>%
    arrange(value)
profitResults

dotplot(value~variable|resulttype,
        profitResults,auto.key=T,
        layout=c(1,3),
        scales=list(x=list(rot=90),
                                  relation="free"))

maxProfit <- profitResults %>%
    filter(resulttype=='maxprofit') %>%
    arrange(-value)

dotplot(reorder(variable,value)~value,maxProfit,pch=1,
        xlab='Estimated Total Profit ($)',
        main='Profit comparison across models - Validation Data')

dotplot(value~reorder(variable,-value),
        maxProfit,type=c('S','p'),
        xlab = 'Model',ylab = 'Profit ($)',
        scales=list(x=list(rot=90)))

profitResults %>%
    filter(resulttype!='p_cutoff') %>%
    reshape2::recast(variable~resulttype,measure.var = 'value') %>%
    ggplot(aes(x=no_of_mailings,y=maxprofit,label=variable))+
    geom_text(size=2,angle = 0,check_overlap = T)+
    labs(x='Number of Mailings',y='Predicted Average Profit ($)')

profitResults %>%
    filter(resulttype!='p_cutoff') %>%
    reshape2::recast(variable~resulttype,measure.var = 'value') -> forPlot
xyplot(
    maxprofit ~ no_of_mailings,
    forPlot,
    main = 'Estimated Total $ vs Estimated Mean Prediction Error - Validation Data',
    xlab = 'Mean Prediction Error',
    ylab = 'Total Estimated Donations ($)',
    panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        ltext(
            x = x,
            y = y,
            labels = forPlot$variable,
            pos = 4,
            offset = 1,
            cex = 0.8
        )
    }
    # xlim = c(1.28,1.92)
)
ggplot(forPlot, aes(no_of_mailings, maxprofit)) +
    geom_point(color = 'blue',shape=1) +
    scale_y_continuous(limits = c(10900,12200))+
    scale_x_continuous(limits = c(1100,1600))+
    geom_text_repel(aes(label = variable),
                    box.padding=unit(.5,'lines'),
                    max.iter=1e5,segment.size=.3,
                    force=1) +
    theme_bw()+
    labs(x='No. of mailings',y='Estimated Max Profit',
         title='Comparison of models on validation data')
# +
#     theme_classic()
