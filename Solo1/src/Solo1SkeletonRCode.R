load("apphappyData.RData")
ls()
## [1] "apphappy.2.labs.frame" "apphappy.2.num.frame" ##

#Library will load the existing loaded package.
#Require will install or update when the package is not in our repository

require(cluster)
require(useful)
require(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)

numdata <- apphappy.3.num.frame

dev.off()

### Following are EDA

str(numdata)
head(numdata)
tail(numdata)
summary(numdata)
a=table(numdata$q1)
a

a=table(numdata$q57)
a
names(a)
barplot(a)
library(plyr)
temp <- count(numdata, c('numdata$q1','numdata$q2r1'))
str(temp)

b=table(numdata$q1,numdata$q2r1)
b
barplot(b)
hist(numdata$q1)
hist(numdata$q2r1)

### Creating subsets ###

numsub <- subset(numdata, select=c("q24r1","q24r2","q24r3","q25r1","q25r2","q25r3",
                                   "q26r3","q26r4","q26r5"))

str(numsub)
summary(numsub)

a=table(numsub$q24r1)
a
barplot(a)

a=table(numsub$q25r1)
a
barplot(a)

b=table(numsub$q24r1,numsub$q25r1)
b
barplot(b)



#rcorr(as.matrix(numsub), type="pearson")

require(corrplot)
numsubcorrelation <- cor(numsub)
corrplot(numsubcorrelation)

mcor <- cor(numsub)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black")
#
corrplot(numsubcorrelation, method="shade", addCoef.col="black",
addCoefasPercent=TRUE ,type="lower", shade.col=NA, tl.col="black",
tl.srt=45, addcolorlabel="no", order="AOE",insig = "p-value")

###################################################
### Create a 'scree' plot to determine the num of clusters
#####################################################

dev.off()
wssplot <- function(numsub, nc=15, seed=1234) {
  wss <- (nrow(numsub)-1)*sum(apply(numsub,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(numsub, iter.max = 40, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(numsub)
######################################
### Create a Kmeans with 5 clusters
#########################################
clusterresults <- kmeans(numsub,5)
names(clusterresults)
clusterresults$withinss
clusterresults$tot.withinss
clusterresults$totss
clusterresults$betweenss
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

### Create a PC (Principal Componenet plot)

plot(clusterresults, data=numsub)

clusterresults$centers

head(clusterresults$cluster)

dev.off()
dissE <- daisy(numsub)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)
str(sk2)
plot(sk2)

newdf <- as.data.frame(clusterresults$cluster)

write.csv(newdf, file = "clusterresults.csv")

write.csv(numsub, file = "numsub.csv")

############################
### Create a dataset with the original data with the cluster info
### This will be useful for creating profiles for the clusters
#####################################33

newdf <- read.csv("clusterresults.csv")
combdata <- cbind(numsub,newdf,numdata$q1)
head(combdata)

require(reshape)
combdata <- rename(combdata, c(clusterresults.cluster="cluster"))
head(combdata)

aggregate(combdata,by=list(byvar=combdata$cluster), mean)
## Done with K Means, do the profile
########################################
## Hierarchical Clustering
########################################
numsub.dist = dist(numsub)
require(maptree)
hclustmodel <- hclust(dist(numsub), method = 'complete')
plot(hclustmodel)


cut.5 <- cutree(hclustmodel, k=5)
plot(silhouette(cut.5,numsub.dist))
head(cut.5)

write.csv(cut.5, file = "cut5results.csv")
########################################
##for hclust how to calculate BSS & TSS
######################################
require(proxy)
numsubmat <- as.matrix(numsub)
overallmean <- matrix(apply(numsubmat,2,mean),nrow=1)
overallmean
TSS <- sum(dist(numsubmat,overallmean)^2)
TSS
####################################
#Compute WSS based on 5 clusters
######################################
combcutdata <- cbind(numsub,cut.5)
head(combcutdata)

require(reshape)
combcutdata <- rename(combcutdata, c(cut.5="cluster"))
head(combcutdata)

clust1 <- subset(combcutdata, cluster == 1)
clust1 <- subset(clust1, select=c("q24r1","q24r2","q24r3","q25r1","q25r2","q25r3", "q26r3","q26r4","q26r5"))
clust1 <- as.matrix(clust1,rowby=T)
clust1mean <- matrix(apply(clust1,2,mean),nrow=1)
dis1 <- sum(dist(clust1mean,clust1)^2)

clust2 <- subset(combcutdata, cluster == 2)
clust2 <- subset(clust2, select=c("q24r1","q24r2","q24r3","q25r1","q25r2","q25r3",
                                  "q26r3","q26r4","q26r5"))
clust2 <- as.matrix(clust2,rowby=T)
clust2mean <- matrix(apply(clust2,2,mean),nrow=1)
dis2 <- sum(dist(clust2mean,clust2)^2)

clust3 <- subset(combcutdata, cluster == 3)
clust3 <- subset(clust3, select=c("q24r1","q24r2","q24r3","q25r1","q25r2","q25r3",
                                  "q26r3","q26r4","q26r5"))
clust3 <- as.matrix(clust3,rowby=T)
clust3mean <- matrix(apply(clust3,2,mean),nrow=1)
dis3 <- sum(dist(clust3mean,clust3)^2)

clust4 <- subset(combcutdata, cluster == 4)
clust4 <- subset(clust4, select=c("q24r1","q24r2","q24r3","q25r1","q25r2","q25r3",
                                  "q26r3","q26r4","q26r5"))
clust4 <- as.matrix(clust4,rowby=T)
clust4mean <- matrix(apply(clust4,2,mean),nrow=1)
dis4 <- sum(dist(clust4mean,clust4)^2)

clust5 <- subset(combcutdata, cluster == 5)
clust5 <- subset(clust5, select=c("q24r1","q24r2","q24r3","q25r1","q25r2","q25r3",
                                  "q26r3","q26r4","q26r5"))
clust5 <- as.matrix(clust5,rowby=T)
clust5mean <- matrix(apply(clust5,2,mean),nrow=1)
dis5 <- sum(dist(clust5mean,clust5)^2)

WSS <- sum(dis1,dis2,dis3,dis4,dis5)
WSS

BSS <- TSS - WSS
BSS
## calculating the % of Between SS/ Total SS
rsquare <- BSS/TSS
rsquare

dev.off()


#######################################################
### A little function to calculate the average silhouette width
### for a variety of choices of k for PAM method:
###########################################################
my.k.choices <- 2:8
avg.sil.width <- rep(0, times=length(my.k.choices))
for (ii in (1:length(my.k.choices)) ){
  avg.sil.width[ii] <- pam(numsub, k=my.k.choices[ii])$silinfo$avg.width
}
print( cbind(my.k.choices, avg.sil.width) )
#################################
# PAM method
###############################
clusterresultsPAM <-pam(numsub,5)
summary(clusterresultsPAM)
plot(clusterresultsPAM, which.plots=1)
plot(clusterresultsPAM, which.plots=2)


###################
## Model based clustering
##################
library(mclust)
fit <- Mclust(numsub,5)
plot(fit,data=numsub, what="density") # plot results
summary(fit) # display the best model

















