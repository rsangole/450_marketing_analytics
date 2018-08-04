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
plot(testrun1$loglike)

table(testrun1$Istardraw)

zownertest = matrix(scale(zowner,scale = F),ncol = 1)
Data2 <- list(p=3,lgtdata=lgtdata100,Z=zownertest)
testrun2 <- rhierMnlDP(Data = Data2, Mcmc = mcmctest)

saveRDS(testrun2, file = 'cache/testrun2_250k.Rdata')

plot_runid_bwplots(testrun2, 25000)
plot_respondent_runcharts(testrun2, 300)

names(testrun2)

deltadraws_df <- unclass(testrun2$Deltadraw) %>% as_tibble()
names(deltadraws_df) <- names(xdf)
deltadraws_df %>%
  tibble::rownames_to_column(var = 'id') %>%
  reshape2::melt(id = 'id') %>%
  mutate(id = as.numeric(id)) %>%
  xyplot(value~id|variable,.,type='l',
       panel=function(...){panel.abline(h=0,col='gray',lty=1);panel.xyplot(...)},
       main = paste0('Deltadraw run chart'))
deltadraws_df %>%
  tibble::rownames_to_column(var = 'id') %>%
  reshape2::melt(id = 'id') %>%
  mutate(id = as.numeric(id)) %>%
  filter(id %in% 20000:25000) %>%
  lattice::bwplot(value~variable,.,panel=function(x,y,...){panel.bwplot(x,y,...);panel.abline(h = 0,col = 'darkgray',lty = 2)}, main = paste0('Beta Values for Deltadraws'), scales = list(x=list(rot=45)))

deltadraws_df %>%
  slice(20000:25000) %>%
  skimr::skim()

# Chapter 4
beta_means <- apply(testrun1$betadraw[,,20000:25000],c(1,2),mean)
dim(beta_means)

xbeta <- xmat %*% t(beta_means)
dim(xbeta)

xbeta2 <- matrix(xbeta, ncol = 3, byrow = T)
dim(xbeta2)
head(xbeta2)

expxbeta2 <- exp(xbeta2)
head(expxbeta2)

choice_df1 <- as_tibble(expxbeta2)
choice_df1$rsums <- rowSums(choice_df1)
choice_df1$V1 <- choice_df1$V1/choice_df1$rsums
choice_df1$V2 <- choice_df1$V2/choice_df1$rsums
choice_df1$V3 <- choice_df1$V3/choice_df1$rsums
choice_df1$rsums <- NULL
choice_df1$choicehat <- max.col(choice_df1[,1:3])
choice_df1$choice_actual <- as.vector(t(ydata))
choice_df1

xtabs(~choicehat+choice_actual, choice_df1)

rocTest <- pROC::roc(choice_df1$choice_actual, choice_df2$choicehat, plot=T)
pROC::auc(rocTest)

cat('Average LL',mean(testrun1$loglike))

apply(t(matrix(choice_df1$choicehat,ncol = 424)),2,function(x){tabulate(na.omit(x))})

# -- # -- # -- #

beta_means <- apply(testrun2$betadraw[,,20000:25000],c(1,2),mean)
dim(beta_means)

xbeta <- xmat %*% t(beta_means)
dim(xbeta)

xbeta2 <- matrix(xbeta, ncol = 3, byrow = T)
dim(xbeta2)
head(xbeta2)

expxbeta2 <- exp(xbeta2)
head(expxbeta2)

choice_df2 <- as_tibble(expxbeta2)
choice_df2$rsums <- rowSums(choice_df2)
choice_df2$V1 <- choice_df2$V1/choice_df2$rsums
choice_df2$V2 <- choice_df2$V2/choice_df2$rsums
choice_df2$V3 <- choice_df2$V3/choice_df2$rsums
choice_df2$rsums <- NULL
choice_df2$choicehat <- max.col(choice_df2[,1:3])
choice_df2$choice_actual <- as.vector(t(ydata))
choice_df2

xtabs(~choicehat+choice_actual, choice_df2)

rocTest <- pROC::roc(choice_df2$choice_actual, choice_df2$choicehat, plot=T)
pROC::auc(rocTest)

cat('Average LL',mean(testrun2$loglike))

apply(t(matrix(choice_df2$choicehat,ncol = 424)),2,function(x){tabulate(na.omit(x))})

# Ch 5

new_scenarious <- read_csv('data/extra-scenarios(1).csv')
new_scenarious.mat <- as.matrix(new_scenarious)
betameansoverall <- apply(testrun1$betadraw[,,20000:25000],c(2),mean)
betameansoverall
betavec <- matrix(betameansoverall,ncol=1,byrow = T)
(new_scenarious.mat %*% betavec) %>%
  matrix(.,ncol=3,byrow=T) %>%
  as_tibble() %>%
  mutate(V1 = exp(V1),
         V2 = exp(V2),
         V3 = exp(V3)) %>%
  rowwise() %>%
  mutate(rsum = V1+V2+V3,
         V1 = V1 / rsum,
         V2 = V2 / rsum,
         V3 = V3 / rsum) -> pchoice_df
pchoice_df$choice_hat <- max.col(pchoice_df[,1:3])
pchoice_df

(xmat %*% betavec) %>%
  matrix(.,ncol=3,byrow=T) %>%
  as_tibble() %>%
  mutate(V1 = exp(V1),
         V2 = exp(V2),
         V3 = exp(V3)) %>%
  rowwise() %>%
  mutate(rsum = V1+V2+V3,
         V1 = V1 / rsum,
         V2 = V2 / rsum,
         V3 = V3 / rsum) %>%
  ungroup() -> pchoice_df
pchoice_df$choice_hat <- max.col(pchoice_df[,1:3])
freq_resp <- round(pchoice_df[,1:3]*424,0)
names(freq_resp) <- c('Choice1Freq','Choice2Freq','Choice3Freq')
pchoice_df %<>% bind_cols(freq_resp)
pchoice_df







library(Rtsne)
tsneFit <- Rtsne(X = xmat, perplexity = 15, theta = 0.3)
plot(tsneFit$Y)
