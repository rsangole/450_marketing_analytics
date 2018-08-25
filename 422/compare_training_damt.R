# Comparison of models
r <- resamples(
    x = list(
        RandomForest.Grp=rfFit.GT,
        RandomForest.Ungrp=rfFit.UT,
        Bagged.Grp=bagFit.GT,
        Bagged.Ungrp=bagFit.UT,
        Boost.Grp=boostFit.GT,
        Boost.Grp.Sq=boostFit.GS,
        Boost.UnGrp=boostFit.UT,
        Elastinet=glmnFit,
        PCR.UnGrp=pcrFit.UT,
        PCR.Grp=pcrFit.GT,
        PLS.UnGrp=plsFit.UT,
        PLS.Grp=plsFit.GT,
        NeuralNet.Grp=nnetFit,
        GAM.Grp=gamFit.GT,
        GAM.Grp.Sq=gamFit.GS
        ),
    decreasing = T
)
print(r)
print(summary(r))

dotplot(r,layout=c(2,1),main='`damt` Model Comparison: Training Data')
bwplot(r, layout = c(2, 1))

summary(diff(r))
