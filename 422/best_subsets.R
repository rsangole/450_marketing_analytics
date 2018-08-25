library(leaps)
olsFit <- lm(damt~.,
                  data = damt_training[,groupedTransformedColumns.damt])
summary(olsFit)

olsFit.full <- regsubsets(damt~.,
             data = damt_training[,groupedTransformedColumns.damt],
             nvmax = 40)
summary(olsFit.full) -> s

plot(s$rsq)
which.max(s$rsq)
plot(s$rss)
plot(s$adjr2)
plot(s$bic)
points(which.min(s$bic),s$bic[which.min(s$bic)],col='red',pch=20)

coef(olsFit.full,id = 25)


set.seed(1)
stepwiseFit.GT <-
    train(
        x = damt_training[, ungroupedTransformedColumns.damt] %>%
            dplyr::select(-damt),
        y = damt_training$damt,
        method = 'leapSeq',
        # trControl = ctrl_rcv,
        preProc = c('center', 'scale'),
        tuneGrid = data.frame(nvmax=10)
    )
stepwiseFit.GT
summary(stepwiseFit.GT)
s$bic %>% plot()
points(which.min(s$bic),s$bic[which.min(s$bic)],col='red',pch=20)


set.seed(1)
fwdFit.GT <-
    train(
        x = data.frame(damt_training[, ungroupedTransformedColumns.damt] %>%
            dplyr::select(-damt)),
        y = damt_training$damt,
        method = 'leapForward',
        # trControl = ctrl_rcv,
        preProc = c('center', 'scale'),
        tuneGrid = data.frame(nvmax=2:37)
    )
fwdFit.GT
fwdFit.GT %>% plot()
summary(fwdFit.GT) -> s
s$bic %>% plot()
points(which.min(s$bic),s$bic[which.min(s$bic)],col='red',pch=20)
