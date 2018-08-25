ctrl <- trainControl(
    summaryFunction = twoClassSummary,
    classProbs = T,
    method = 'LGOCV',
    savePredictions = T
)

ctrl1 <- trainControl(
    summaryFunction = twoClassSummary,
    classProbs = T,
    method = 'cv',
    number = 10,
    repeats = 1,
    savePredictions = T
)

ctrl2 <- trainControl(
    summaryFunction = twoClassSummary,
    classProbs = T,
    ## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated five times
    repeats = 5,
    savePredictions = T
)
ctrl3 <- trainControl(
    summaryFunction = twoClassSummary,
    classProbs = T,
    ## 10-fold CV
    # method = "repeatedcv",
    # number = 10,
    ## repeated five times
    # repeats = 5,
    savePredictions = T
)
ctrl_rcv <- trainControl(
    ## 10-fold CV
    method = "repeatedcv",
    number = 10,
    # ## repeated five times
    repeats = 5
)
ctrl_lgocv <- trainControl(
    method = 'lgocv',
    repeats = 5
)

ctrl_boot <- trainControl(
    method = 'boot'
)

ctrl_oob <- trainControl(
    method = 'oob'
)
ctrl_cv <- trainControl(
    method = 'cv'
)
