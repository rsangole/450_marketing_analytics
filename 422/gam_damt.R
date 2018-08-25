map2(.x = damt_training[,groupedTransSqColumns.damt],
     .y = groupedTransSqColumns.damt,
     .f = ~xyplot(.x~damt_training$damt,main=.y))

gamPurr <- damt_training[,groupedTransSqColumns.damt] %>%
    dplyr::select(-damt,-reg,-home,-chld,-genf,-wrat,-hinc)

models=map(gamPurr,~gam(damt_training$damt~s(.x,df = 2)))


gam(damt~reg,damt_training)
xyplot(damt~reg,damt_training)
xyplot(smooth.spline(x=damt_training$damt,y=damt_training$reg,df=5)$y,col='red')

# smoothFits <- map(.x= damt_training[,trans_columns],
#                   .f=~smooth.spline(x = .x,
#                                     y = damt_training$damt,
#                                     df=20))
# Grouped Transformed
gamDF <- data.frame(damt_training[,groupedTransformedColumns.damt])
set.seed(1)
gamFit.GT <- train(
    damt~.,
    data = gamDF,
    method = 'gamSpline',
    # trControl = ctrl_cv,
    # df = 10,
    tuneGrid = data.frame(df=1:10),
    preProc = c('center', 'scale')
)
plot(gamFit.GT)

# Grouped Transformed Sq
gamDF <- data.frame(damt_training[,groupedTransSqColumns.damt])
set.seed(1)
gamFit.GS <- train(
    damt~.,
    data = gamDF,
    method = 'gamSpline',
    # trControl = ctrl_cv,
    # df = 10,
    tuneGrid = data.frame(df=1:10),
    preProc = c('center', 'scale')
)
plot(gamFit.GS)

#----------------------------
gam(formula = damt~
        reg+
        home+
        chld+
        hinc+
        genf+
        wrat+
        s(trans.avhv,    df=4)+
        s(trans.incm,    df=4)+
        s(trans.inca,    df=4)+
        s(trans.plow,    df=4)+
        s(trans.npro,    df=4)+
        s(trans.tgif,    df=4)+
        s(trans.lgif,    df=4)+
        s(trans.rgif,    df=4)+
        s(trans.tdon,    df=4)+
        s(trans.tlag,    df=4)+
        s(trans.agif,    df=4)+
        s(sq.trans.avhv, df=4)+
        s(sq.trans.incm, df=4)+
        s(sq.trans.inca, df=4)+
        s(sq.trans.plow, df=4)+
        s(sq.trans.npro, df=4)+
        s(sq.trans.tgif, df=4)+
        s(sq.trans.lgif, df=4)+
        s(sq.trans.rgif, df=4)+
        s(sq.trans.tdon, df=4)+
        s(sq.trans.tlag, df=4)+
        s(sq.trans.agif, df=4)
    ,
    data = data.frame(damt_training[,groupedTransSqColumns.damt])
) -> gamFit
gam(formula = damt~
        reg+
        home+
        chld+
        hinc+
        # genf+
        wrat+
        s(trans.avhv,    df=4)+
        s(trans.incm,    df=4)+
        # s(trans.inca,    df=4)+
        s(trans.plow,    df=4)+
        s(trans.npro,    df=4)+
        s(trans.tgif,    df=4)+
        s(trans.lgif,    df=4)+
        s(trans.rgif,    df=4)+
        # s(trans.tdon,    df=4)+
        # s(trans.tlag,    df=4)+
        s(trans.agif,    df=4)+
        s(sq.trans.avhv, df=4)+
        # s(sq.trans.incm, df=4)+
        # s(sq.trans.inca, df=4)+
        s(sq.trans.plow, df=4)+
        # s(sq.trans.npro, df=4)+
        # s(sq.trans.tgif, df=4)+
        s(sq.trans.lgif, df=4)+
        # s(sq.trans.rgif, df=4)+
        # s(sq.trans.tdon, df=4)+
        # s(sq.trans.tlag, df=4)+
        s(sq.trans.agif, df=4)
    ,
    data = data.frame(damt_training[,groupedTransSqColumns.damt])
) -> gamFit2

summary(gamFit)
summary(gamFit2)
