log_transform <- function(x) {
    x.transformed <- map_df(
        x,
        .f = function(x)
            log(x + 1e-4)
    )
    colnames(x.transformed) <- paste0('log.', colnames(x))
    x.transformed
}

sq_transform <- function(x) {
    x.transformed <- map_df(
        x,
        .f = function(x)
            x ^ 2
    )
    colnames(x.transformed) <- paste0('sq.', colnames(x))
    x.transformed
}

ROCplot <- function(x,t=0.5) {
    plot(
        x,
        print.thres = t,
        type = "S",
        print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
        print.thres.cex = 1,
        legacy.axes = TRUE
    )
}


profitCalculations <- function(validationmodelPred) {
    # # calculate ordered profit function using average donation = $14.50 and mailing cost = $2
    donr <- donr_validating$donr %>% unclass() - 1
    profit <-
        cumsum(14.5 * donr[order(validationmodelPred, decreasing = T)] - 2)
    # plot(profit, type = 'l') # see how profits change as more mailings are made
    # abline(v = which.max(profit), col = 'red')
    no_of_mailings <-
        which.max(profit) # number of mailings that maximizes profits
    p_cutoff <-
        validationmodelPred[order(validationmodelPred, decreasing = T)][no_of_mailings]

    p <-
        validationmodelPred[order(validationmodelPred, decreasing = T)]

    plot(p,profit,type='l',xlab = 'threshold probability',ylab = 'Estimated profit ($)',
         main='Profit vs cutoff for P(Y="Donor")')
    abline(v=p_cutoff,col='red')

    c(no_of_mailings, maxprofit = max(profit), p_cutoff) # report number of mailings and maximum profit
}
