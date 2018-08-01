library(doMC)
doMC::registerDoMC(cores = 4)

main_mat <- read_tsv('data/stc-dc-task-cbc -v3(1).csv')
head(main_mat)
# xmat <- efcode.attmat.f(as.matrix(main_mat[,c("screen", "RAM", "processor", "price", "brand")]))
xdf <- convert_to_effectcodes(main_mat[,c("screen", "RAM", "processor", "price", "brand")])
head(xdf)

pricevec = main_mat$price - mean(main_mat$price)

brandsmat <- xdf %>% select(starts_with('brand'))
brands_pricemat <- brandsmat * pricevec
names(brands_pricemat) <- paste0(names(brands_pricemat),'price')

xdf <- cbind(xdf, brands_pricemat)
# xmat is the
# xmat has dimensions (108 x 14) == (36 x 3) x 14 == (#questions x #choices/q) x (#dummy variables)
head(xdf)
dim(xdf)
xmat <- as.matrix(xdf)

det(t(xmat)%*%xmat)

load('data/stc-cbc-respondents-v3(1).RData')
ydata <- as_tibble(resp.data.v3) %>%
  select(starts_with('DCM', ignore.case = F))
# ydata is the response from the discrete choice survey
# dimensions are (424 x 36) == (#resp x #ques)
ydata

table(complete.cases(ydata))
ydata <- na.omit(ydata)

ymat <- as.matrix(ydata)

zowner <- ifelse(is.na(resp.data.v3$vList3),0,1)

lgtdata <- NULL
for (i in 1:424) {
  lgtdata[[i]] <- list(y=ymat[i,],X=xmat)
}
lgtdata[[1]]


# lgtdata100 <- lgtdata[1:50]
lgtdata100 <- lgtdata
mcmctest <- list(R=250000,keep=10)
Data1 <- list(p=3,lgtdata=lgtdata100)
testrun1 <- rhierMnlDP(Data = Data1, Mcmc = mcmctest)

saveRDS(testrun1, file = 'cache/testrun1_250k.Rdata')

names(testrun1)
dim(testrun1$betadraw)
plot_runid_bwplots(testrun1, 1000)
plot_respondent_runcharts(testrun1, 300)

burnoff <- -1:-200000

plot(testrun1$loglike)

table(testrun1$Istardraw)

zownertest = matrix(scale(zowner,scale = F),ncol = 1)
Data2 <- list(p=3,lgtdata=lgtdata100,z=zownertest)
testrun2 <- rhierMnlDP(Data = Data2, Mcmc = mcmctest)

saveRDS(testrun1, file = 'cache/testrun2_250k.Rdata')

