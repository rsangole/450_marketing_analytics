
# EDA
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)

par(mfrow=c(1,1))
map2(.x = damt_training[, c(5:9, 22)],
     .y = colnames(damt_training[, c(5:9, 22)]),
     .f = function(.x, .y) {
         beanplot(
             damt ~ genf * .x,
             damt_training,
             side = 'b',
             col = list("yellow", "orange"),
             border = c("yellow2", "darkorange"),
             # col = 'yellow2',
             # border = 'yellow3',
             ll = 0.005,
             boxwex = 1,
             log = '',
             main = paste0('Donation Amount vs ', .y),
             xlab = .y,
             ylab = 'Donation Amount ($)'
         )
         legend(
             "topright",
             bty = "n",
             c("Male", "Female"),
             fill = c("yellow", "orange")
         )
     })

# beanplot(
#     damt ~ genf * hinc,
#     damt_training,
#     side = 'b',
#     col = list("yellow", "orange"),
#     border = c("yellow2", "darkorange"),
#     ll = 0.01,
#     maxstripline = 0.4,
#     method = 'stack',
#     log = '',
#
# )

corrplot::corrplot(
    cor(damt_training[, c(conti_columns, 'damt')]),
    method = 'ellipse',
    hclust.method = 'ward.D2',
    order = 'hclust',
    addrect = 4
)
corrplot::corrplot(
    cor(damt_training[, c(trans_columns, 'damt')]),
    method = 'number',
    hclust.method = 'ward.D2',
    order = 'hclust',
    addrect = 4
)
tooHigh <- findCorrelation(cor(damt_training[, c(trans_columns, 'damt')]),
                           cutoff = .85,
                           verbose = T, names = T)
tooHigh

two_hist <- function(x, y) {
    gridExtra::grid.arrange(histogram(x, main = y),
                            histogram(log(x)))
}
# par(mfrow = c(1, 2))
map2(.x = damt_training[, conti_columns],
     .y = conti_columns,
     .f = ~ two_hist(.x, .y))

describe(x = damt_training)

#BEFORE
featurePlot(
    x = damt_training[, conti_columns],
    y = damt_training$damt,
    between = list(x = 1, y = 1),
    type = c('g', 'p', 'smooth')
)
#AFTER
featurePlot(
    x = damt_training[, trans_columns],
    y = damt_training$damt,
    between = list(x = 1, y = 1),
    type = c('g', 'p', 'smooth')
)


#Skewness checks
apply(damt_training[,conti_columns],2,skewness)
map_dbl(damt_training[,conti_columns],function(y) BoxCoxTrans(y,fudge = .3)$lambda)
    #zeros->log, pro and tlag benefit from diff transformation

# trans <- preProcess(x = data.frame(damt_training[,conti_columns]),
#            method = c('BoxCox'))
# transformed <- predict(trans,data.frame(damt_training[,conti_columns]))
# colnames(transformed) <- paste0('trans.',colnames(transformed))

map(.x = transformed,histogram)

