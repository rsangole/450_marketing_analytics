setwd("C:/Users/Syamala.srinivasan/Google Drive/NorthWestern/Predict450/RProjects")
##################
load("XYZ_complete_customer_data_frame.RData")
ls()
mydata <- complete.customer.data.frame
names(mydata)

table(mydata$RESPONSE16)
table(mydata$ANY_MAIL_16)
xtabs(~RESPONSE16 + ANY_MAIL_16, data = mydata)

mydata$PRE2009SALES = mydata$LTD_SALES - mydata$YTD_SALES_2009
mydata$PRE2009TRANSACTIONS = mydata$LTD_TRANSACTIONS - 
  mydata$YTD_TRANSACTIONS_2009
mydata$cum15QTY <- mydata$QTY0 + mydata$QTY1 + mydata$QTY2 + mydata$QTY3 +
  mydata$QTY4 + mydata$QTY5 + mydata$QTY6 + mydata$QTY7 + mydata$QTY8 +
  mydata$QTY9 + mydata$QTY10 + mydata$QTY11 + mydata$QTY12 + mydata$QTY13 +
  mydata$QTY14 + mydata$QTY15
mydata$cum15TOTAMT <- mydata$TOTAMT0 + mydata$TOTAMT1 + mydata$TOTAMT2 + mydata$TOTAMT3 +
  mydata$TOTAMT4 + mydata$TOTAMT5 + mydata$TOTAMT6 + mydata$TOTAMT7 + mydata$TOTAMT8 +
  mydata$TOTAMT9 + mydata$TOTAMT10 + mydata$TOTAMT11 + mydata$TOTAMT12 + mydata$TOTAMT13 +
  mydata$TOTAMT14 + mydata$TOTAMT15

#mydata$salepercust <- mydata$PRE2009SALES/mydata$CENSUS_FACT1
meansalepercust <- mean(mydata$PRE2009SALES)
minsalepercust <- min(mydata$PRE2009SALES)
maxsalepercust <- max(mydata$PRE2009SALES)
mydata$salepertrans <- mydata$PRE2009SALES/mydata$PRE2009TRANSACTIONS
mydata$salepercamp <- mydata$cum15TOTAMT/mydata$TOTAL_MAIL_15

mean(mydata$salepertrans, trim = 0.01, na.rm = TRUE)
mean(mydata$salepercamp, trim = 0.05, na.rm = TRUE)


require(rpart)
require(rpart.plot)
require(tree)
require(rattle)
require(caTools)
require(ROCR)
require(ResourceSelection)

library(corrgram)
library(MASS)
library(randomForest)
library(inTrees)
library(pROC)
library(caret)
library(dplyr)

subdat <- subset(mydata, select=c("PRE2009SALES","PRE2009TRANSACTIONS","MED_INC","cum15QTY", "QTY15",
                                  "cum15TOTAMT", "TOTAMT15","ESTHMVL","EXAGE", "INC_WOUTSCS_AMT_4","ZKITCHEN",
                                  "SUM_MAIL_16","TOTAL_MAIL_16","salepercamp",
                                  "ANY_MAIL_16","RESPONSE16","salepertrans"))

str(subdat)




subdat2 <- subset(subdat, ANY_MAIL_16 > 0)
str(subdat2)
#subdat2 <- subset(subdat, EXAGE > 0)
###subdat$ANY_MAIL_16 <- NULL 
##unitamtt <- mean(subdat2$TOTAMT)
subdat2$FRESPONSE16 <- factor(as.factor(subdat2$RESPONSE16))
head(subdat2)
subdat2$EXAGE[subdat2$EXAGE=="U"] <- NA
subdat3 <- na.omit(subdat2) 
head(subdat3)
str(subdat3)
subdat3$EXAGE <- as.numeric(subdat3$EXAGE)
mean(subdat3$EXAGE,na.rm=TRUE)
subdat3$logPRE2009TRANSACTIONS <- log(subdat3$PRE2009TRANSACTIONS)
head(subdat3)
str(subdat3)

subdat3 <- subset(subdat3, select=c("PRE2009SALES","PRE2009TRANSACTIONS","MED_INC",
                                  "ESTHMVL","EXAGE", "INC_WOUTSCS_AMT_4","TOTAMT15",
                                  "SUM_MAIL_16","TOTAL_MAIL_16", "cum15TOTAMT","cum15QTY",
          "RESPONSE16","FRESPONSE16", "salepertrans", "logPRE2009TRANSACTIONS"))

subdat3 <- na.omit(subdat3) 
str(subdat3)
table(subdat3$RESPONSE16)
### note: 8562/9712 = 88% did not respond####

######## Logistic Regression Model #############

mylogit <- glm(RESPONSE16 ~ PRE2009SALES+MED_INC+EXAGE+PRE2009TRANSACTIONS+TOTAMT15+
                 salepertrans + cum15QTY+cum15TOTAMT, data = subdat3, family = "binomial")

summary(mylogit)
pvalue <- 1 - pchisq(425, df=8)
pvalue
###7065-6640=425; 9711-9703=8; deviance significant##
###Another GOF tets - Hosmer & lemeshow ###
hoslem.test(subdat3$RESPONSE16, fitted(mylogit))
pred2 <- predict(mylogit,data=subdat3, type="response")
head(pred2)
str(pred2)

pred2round <- round(pred2,0)
xtabs(~RESPONSE16 + pred2round, data = subdat3)
#### accuracy = 8510+35/9712 = 88%####


### ROCR curve
ROCRpred <- prediction(pred2,subdat3$RESPONSE16)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
auc(subdat3$RESPONSE16,pred2)

hist(pred2)

pred2df <- as.data.frame(pred2)
data_all <- cbind(subdat3,pred2)
str(data_all)
mean(data_all$salepertrans)
mean(data_all$salepertrans [data_all$pred2>0.8])
###if the cutoff is 0.8 ###

########Random Forest model ###############

### putting '.' means includes all the variables
##rf1 <- randomForest(RESPONSE16 ~ .
##                    ,data=subdat3,importance=TRUE,ntree=100)
rf1 <- randomForest(RESPONSE16 ~ PRE2009SALES+MED_INC+ESTHMVL+
                      salepertrans+EXAGE
                    ,data=subdat3,importance=TRUE,ntree=100)
summary(rf1)

##getTree(rf1,1,labelVar=TRUE)
##?getTree
print(rf1)
plot(rf1)
importance(rf1)
varImpPlot(rf1)
##how much each variable improves the prediction of its tree##
##compared to the exact same tree without that variable##


#get the prediction probabilities##
rf1p  <- predict(rf1, newdata=subdat3,type="response")
head(rf1p)
hist(rf1p)
rf1pdf <- as.data.frame(rf1p)
rf1.pred = prediction(rf1p, subdat3$RESPONSE16)
rf1.perf = performance(rf1.pred,"tpr","fpr")
plot(rf1.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

auc(subdat3$RESPONSE16,rf1p)

#Make a confusion matrix and compute accuracy
##confusion.matrix <- table(rf1p, subdat3$RESPONSE16)
##confusion.matrix

rf1pround <- round(rf1p,0)
xtabs(~RESPONSE16 + rf1pround, data = subdat3)

data_alldf <- cbind(subdat3,rf1pdf)
str(data_alldf)
head(data_alldf)
mean(data_alldf$salepertrans)
mean(data_alldf$salepertrans [data_alldf$rf1p>0.7])
##############################
#####################  SVM Linear #############################

library("e1071")
library(caret)



####### repeated cross validation, 10-fold CV, 3 times repeated ##
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(RESPONSE16 ~., data = subdat3, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

####################### test the model on the test set #####################
test_pred <- predict(svm_Linear, newdata = subdat3)
test_pred
confusionMatrix(test_pred, subdat3$RESPONSE16)


####### tune the linear model for different values of C  ##########################

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5,7,10))
set.seed(3233)
svm_Linear_Grid <- train(RESPONSE16 ~., data = subdat3, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid, newdata = subdat3)
test_pred_grid
confusionMatrix(test_pred_grid, subdat3$RESPONSE16 )


#######################################################################################
########## Non linear classification with RBF kernel #################################
######################################################################################
set.seed(3233)
svm_Radial <- train(RESPONSE16 ~., data = subdat3, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Radial
plot(svm_Radial)

test_pred_Radial <- predict(svm_Radial, newdata = subdat3)
confusionMatrix(test_pred_Radial, subdat3$RESPONSE16)

svmnonlinearpred <- as.data.frame(test_pred_Radial)
colnames(svmnonlinearpred) <- "SVMNonLinPred"
####################################################################################

grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                     0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                           C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1, 1.5, 2,5))
set.seed(3233)
svm_Radial_Grid <- train(RESPONSE16 ~., data = subdat3, method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid_radial,
                         tuneLength = 10)

svm_Radial_Grid
plot(svm_Radial_Grid)

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = subdat3)
confusionMatrix(test_pred_Radial_Grid, subdat3$RESPONSE16)



#########################################################################################



###############################################################
##########################map ################################
###############################################################
library(ggmap)
library(gridExtra)
library(ggplot2)



# lets make it a little easier to type
cc <- mydata
ccMap <- subset(cc,select=c(LONG,LAT,RESPONSE16))
ccMap$lon <- -(as.numeric(substr(ccMap$LONG,2,9))/1000000)
ccMap$lat <- (as.numeric(substr(ccMap$LAT,2,9))/1000000)
m1 <- qmplot(lon, lat, data =ccMap[ccMap$RESPONSE16 == 0,], color=I('red'),size = I(0.5), darken = .0) + ggtitle("Response16 = 0")
m2 <- qmplot(lon, lat, data =ccMap[ccMap$RESPONSE16 == 1,], color=I('green'),size = I(0.5), darken = .0) + ggtitle("Response16 = 1")
grid.arrange(m1, m2, nrow=2)


 
 
  




