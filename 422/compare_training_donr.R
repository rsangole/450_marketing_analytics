
# Comparison of models

r <- resamples(
    x = list(
        logit_Grp_Raw = logitFit.GR,
        logit_Grp_Log = logitFit.GL,
        logit_Grp_Sq = logitFit.GSq,
        logit_Grp_Trans = logitFit.GTS,
        lda_UnGrp_Raw = ldaFit.UR,
        lda_UnGrp_Log = ldaFit.UL,
        lda_UnGrp_Sq =  ldaFit.USq,
        lda_UnGrp_Log = ldaFit.UL,
        lda_Grp_Sq = ldaFit.GSq,
        qda_UnGrp_Raw = qdaFit.UR,
        qda_UnGrp_Log = qdaFit.UL,
        qda_UnGrp_Sq = qdaFit.USq,
        # lasso_UnGrp_Log = glmBestFit
        # tree_Grp_Raw = rpartFit.GR,
        # tree_Grp_Log = rpartFit.GL,
        # tree_Grp_Sq = rpartFit.GSq,
        # # rf_UnGrp_Raw = rfFit.UR,
        bag_UnGrp_Raw = baggedFit.UR,
        # bag_Grp_Raw = baggedFit.GR,
        bag_UnGrp_Log = baggedFit.UL,
        bag_Grp_Log = baggedFit.GL,
        bag_UnGrp_Sq = baggedFit.USq,
        bag_Grp_Sq = baggedFit.GSq,
        rf_UnGrp_Raw = rfFit.UR,
        rf_Grp_Raw = rfFit.GR,
        rf_UnGrp_Log = rfFit.UL,
        rf_Grp_Log = rfFit.GL,
        rf_UnGrp_Sq = rfFit.USq,
        rf_Grp_Sq = rfFit.GSq,
        boost_Grp_Sq = boostFit.GSq,
        boost_Grp_SqRed = boostFit.GSqRed,
        # boost_UnGrp_Log = boostFit.UL,
        # boost_UnGrp_Raw = boostFit.UR,
        boost_UnGrp_Sq = boostFit.USq
    ),
    metric = 'ROC',
    decreasing = T
)
print(r)
print(summary(r))

dotplot(r,layout=c(3,1))
dotplot(r,metric = 'ROC',
        main = 'Comparison of AUC across models')

summary(diff(r))

xyplot(r, what = "tTime", auto.key = T)

bwplot(r, metric = 'ROC',
       main = 'Comparison of AUC across models')

splom(r)
