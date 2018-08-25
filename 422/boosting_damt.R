gbmGrid <- expand.grid(n.trees = seq(2000,7000,by = 1000),
           interaction.depth = c(1,2),
           shrinkage = c(.1,0.01,0.001),
           n.minobsinnode = 10)

## Grouped Transformed w/ Squared Data
X <- data.frame(damt_training[, groupedTransSqColumns.damt] %>%
                    dplyr::select(-damt))
Y <- damt_training$damt
set.seed(1)
boostFit.GS <-
    train(
        x=X,
        y=Y,
        method = 'gbm',
        # trControl = ctrl2,
        tuneGrid = gbmGrid,
        preProc = c('center', 'scale')
    )
boostFit.GS
plot(boostFit.GS,metric = 'RMSE')

# The final values used for the model were n.trees = 4000, interaction.depth = 1,
# shrinkage = 0.01 and n.minobsinnode = 10


## Grouped Transformed Data
X <- data.frame(damt_training[, groupedTransformedColumns.damt] %>%
                    dplyr::select(-damt))
Y <- damt_training$damt
set.seed(1)
boostFit.GT <-
    train(
        x=X,
        y=Y,
        method = 'gbm',
        # trControl = ctrl2,
        tuneGrid = gbmGrid,
        preProc = c('center', 'scale')
    )
boostFit.GT
plot(boostFit.GT,metric = 'RMSE')

# The final values used for the model were n.trees = 4000, interaction.depth = 1,
# shrinkage = 0.01 and n.minobsinnode = 10

## Grouped Transformed Data - CV
X <- data.frame(damt_training[, groupedTransformedColumns.damt] %>%
                    dplyr::select(-damt))
Y <- damt_training$damt
set.seed(1)
boostFit.GTcv <-
    train(
        x=X,
        y=Y,
        method = 'gbm',
        trControl = ctrl_cv,
        tuneGrid = gbmGrid,
        preProc = c('center', 'scale')
    )
boostFit.GTcv
plot(boostFit.GTcv,metric = 'RMSE')

# Conclusion:
# No change in gbm parameters to select:
# The final values used for the model were n.trees = 5000, interaction.depth = 1,
# shrinkage = 0.01 and n.minobsinnode = 10.

## Ungrouped Transformed Data
X <- data.frame(damt_training[, ungroupedTransformedColumns.damt] %>%
                    dplyr::select(-damt))
Y <- damt_training$damt
set.seed(1)
boostFit.UT <-
    train(
        x=X,
        y=Y,
        method = 'gbm',
        # trControl = ctrl2,
        tuneGrid = gbmGrid,
        preProc = c('center', 'scale')
    )
boostFit.UT
plot(boostFit.UT,metric = 'RMSE')