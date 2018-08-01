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


lgtdata100 <- lgtdata[1:50]
mcmctest <- list(R=5000,keep=5)
Data1 <- list(p=3,lgtdata=lgtdata100)
testrun1 <- rhierMnlDP(Data = Data1, Mcmc = mcmctest)

names(testrun1)

dim(testrun1$betadraw)

run_id <- 1000
betas_from_draws <- as_tibble(testrun1$betadraw[,,run_id])
names(betas_from_draws) <- names(xdf)
betas_from_draws %>%
  tibble::rownames_to_column(var = 'id') %>%
  reshape2::melt(id = 'id') %>%
  lattice::bwplot(value~variable,.,panel=function(x,y,...){panel.bwplot(x,y,...);panel.abline(h = 0,col = 'darkgray',lty = 2)}, main = paste0('Beta Values for all respondents for simulation run #', run_id))

respondent_id <- 1
burnoff <- -1:-700
respondent_id_level_beta_runchart <- as_tibble(t(testrun1$betadraw[respondent_id,,]))
names(respondent_id_level_beta_runchart) <- names(xdf)
respondent_id_level_beta_runchart %<>%
  tibble::rownames_to_column(var = 'id') %>%
  reshape2::melt(id = 'id') %>%
  mutate(id = as.numeric(id))
xyplot(value~id|variable,respondent_id_level_beta_runchart,type='l',
       main = paste0('Beta estimation run chart for Respondent ',respondent_id))
densityplot(~value|variable,respondent_id_level_beta_runchart[burnoff,],plot.points=F,
            panel=function(...){panel.abline(v=0,col='gray',lty=1);panel.densityplot(...)},
            main = paste0('Density Plot for Respondent ',respondent_id))

