## UnGrouped Transformed Data
set.seed(1)
pcrFit.UT  <-
    train(
        x =damt_training[, ungroupedTransformedColumns.damt] %>%
            dplyr::select(-damt),
        y=damt_training$damt,
        method = 'pcr',
        # validation='CV',
        # trControl = ctrl_cv,
        tuneGrid = data.frame(ncomp=1:20),
        preProc = c('center', 'scale')
    )
pcrFit.UT
pcrFit.UT %>% plot()

set.seed(1)
pcrFit.GT  <-
    train(
        x =damt_training[, groupedTransformedColumns.damt] %>%
            dplyr::select(-damt),
        y=damt_training$damt,
        method = 'pcr',
        # validation='CV',
        # trControl = ctrl_cv,
        tuneGrid = data.frame(ncomp=1:20),
        preProc = c('center', 'scale')
    )
pcrFit.GT
pcrFit.GT %>% plot()

# PLS ---------------------------------------------------------------------

set.seed(1)
plsFit.UT  <-
    train(
        x =damt_training[, ungroupedTransformedColumns.damt] %>%
            dplyr::select(-damt),
        y=damt_training$damt,
        method = 'pls',
        # validation='CV',
        # trControl = ctrl_cv,
        tuneGrid = data.frame(ncomp=1:20),
        preProc = c('center', 'scale')
    )
plsFit.UT
plsFit.UT %>% plot()

plsFit.GT  <-
    train(
        x =damt_training[, groupedTransformedColumns.damt] %>%
            dplyr::select(-damt),
        y=damt_training$damt,
        method = 'pls',
        # validation='CV',
        # trControl = ctrl_cv,
        tuneGrid = data.frame(ncomp=1:20),
        preProc = c('center', 'scale')
    )
plsFit.GT
plsFit.GT %>% plot()
