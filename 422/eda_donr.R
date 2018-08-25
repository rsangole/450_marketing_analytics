# EDA

map2(
    .x = data.train.std.c[,10:20],
    .y = colnames(data.train.std.c[,10:20]),
    ~beanplot(.x~donr,data.train.std.c,col='yellow2', border = 'yellow3',
              ll = 0.001,boxwex = 1,log = '',
              main=.y, ylab=.y,
              xlab='Donor/Not Donor')
)

par(mfrow=c(1,2))
map2(
    .x = donr_training[, conti_columns],
    .y = colnames(donr_training[, conti_columns]),
    ~ beanplot(
        .x~donr,
        donr_training,
        col = 'yellow2',
        border = 'yellow3',
        ll = 0.001,
        boxwex = 1,
        log = '',
        main = paste0('Donor Status vs ', .y),
        xlab = 'Donor Status',
        ylab = .y
    )
)

histogram(~wrat|donr,data.train.std.c)
histogram(~hinc|donr,data.train.std.c)
histogram(~chld|donr,data.train.std.c)
histogram(~home|donr,data.train.std.c)
histogram(~reg2|donr,data.train.std.c)

map2(
    .x = data.train.std.c,
    .y = colnames(data.train.std.c),
    ~histogram(x = ~.x,ylab = .y)
)

# Potential candidates for transformations:
# avhv, incm, inca, plow,

corrplot::corrplot(cor(donr_training[,groupedColumns.donr]),method = 'ellipse',
                   hclust.method = 'ward.D2',order = 'hclust')



two_hist <- function(x, y) {
    gridExtra::grid.arrange(histogram(x, xlab = y, main = paste0('Variable: ', y)),
                            histogram(log(x), xlab = paste0('log(', y, ')')))
}
map2(.x = donr_training[, conti_columns],
     .y = conti_columns,
     .f = ~ two_hist(.x, .y))
