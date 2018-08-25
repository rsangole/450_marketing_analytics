nnet_training <- damt_training %>% dplyr::select(-one_of(tooHigh))
library(nnet)
nnetFit <- nnet(x = data.frame(damt_training[,ungroupedTransformedColumns.damt] %>% dplyr::select(-damt)),
                y= damt_training$damt,
                linout = T,
                trace = T,
                maxit = 500,
                size = 5,
                decay = .01)
print(nnetFit)

nnetYhat <- predict(nnetFit,damt_validating[,ungroupedTransformedColumns.damt])

mean((nnetYhat-damt_validating$damt)^2)


nnetGrid <- expand.grid(decay=c(0,0.1,0.01),
                        size=c(1:8))
nnetFit <- train(
    x = data.frame(damt_training[,ungroupedTransformedColumns.damt] %>%
                       dplyr::select(-damt)),
    y= damt_training$damt,
    preProc = c('center', 'scale'),
    method = 'nnet',
    linout = T,
    trace = F,
    maxit=500,
    tuneGrid = nnetGrid
)
plot(nnetFit)
