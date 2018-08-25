setwd("~/Documents/OneDrive/MSPA/422/Final_Project")

donr_flag <- TRUE # Create DONR specific dataset?
damt_flag <- TRUE  # Create DAMT specific dataset?

set.seed(1)

library(tidyverse)
library(lattice)
library(randomForest)
library(gbm)
library(beanplot)
library(MASS)
library(glmnet)
library(caret)
library(pROC)
library(Hmisc)
# library(doMC)
# registerDoMC(4)
library(e1071)
library(gam)

source('functions.R')
source('trainControls.R')

df_master <- readr::read_csv(file = 'charity.csv')

df_master %>% dplyr::select(-ID) -> df_master

# glimpse(df_master)
# map_int(df_master,~sum(is.na(.x)))

# New grouped variable 'reg' for region
df_master$reg <- 5
df_master$reg[df_master$reg1 == 1] <- 1
df_master$reg[df_master$reg2 == 1] <- 2
df_master$reg[df_master$reg3 == 1] <- 3
df_master$reg[df_master$reg4 == 1] <- 4

# Re-factoring categorical variables
df_master$reg <-
    factor(
        df_master$reg,
        levels = c(1, 2, 3, 4, 5),
        labels = c('Region1', 'Region2', 'Region3', 'Region4', 'Region5')
    )
df_master$home <-
    factor(
        df_master$home,
        levels = c(0, 1),
        labels = c('Non.H.Owner', 'H.Owner')
    )
df_master$chld <- factor(df_master$chld,
                         levels = seq(0, 5),
                         labels = paste0('CHLD', 0:5))
df_master$hinc <- factor(df_master$hinc,
                         levels = seq(1, 7),
                         labels = paste0('HINC', 1:7))
df_master$genf <-
    factor(df_master$genf,
           levels = c(0, 1),
           labels = c('M', 'F'))
df_master$wrat <- factor(df_master$wrat,
                         levels = seq(0, 9),
                         labels = paste0('W', 0:9))
df_master$donr <-
    factor(df_master$donr,
           levels = c(0, 1),
           labels = c('Non.Donor', 'Donor'))

mm <-
    model.matrix(reg ~ ., df_master %>% dplyr::select(-donr, -damt),)
ungroupedCols <- mm[, c(6:27)]

df_master <- as_tibble(data.frame(df_master, ungroupedCols))

# Continuous predictors
conti_columns <- c("avhv",
                   "incm",
                   "inca",
                   "plow",
                   "npro",
                   "tgif",
                   "lgif",
                   "rgif",
                   "tdon",
                   "tlag",
                   "agif")

log_columns <- c(
    'log.avhv',
    'log.incm',
    'log.inca',
    'log.plow',
    'log.npro',
    'log.tgif',
    'log.lgif',
    'log.rgif',
    'log.tdon',
    'log.tlag',
    'log.agif'
)

trans_columns <-
    c(
        'trans.avhv',
        'trans.incm',
        'trans.inca',
        'trans.plow',
        'trans.npro',
        'trans.tgif',
        'trans.lgif',
        'trans.rgif',
        'trans.tdon',
        'trans.tlag',
        'trans.agif'
    )

# Transformations
log.trans <- log_transform(df_master[, conti_columns])
squares <- sq_transform(df_master[, conti_columns])

trans <- preProcess(x = data.frame(df_master[,conti_columns]),
                    method = c('BoxCox'))
transformed <- predict(trans,data.frame(df_master[,conti_columns]))
colnames(transformed) <- paste0('trans.',colnames(transformed))
sq.transformed <- sq_transform(transformed)

pmap(.l = list(a=transformed,
     b=df_master[,conti_columns],
     c=colnames(transformed)),
     function(a,b,c){
         par(mfrow=c(2,1))
         hist(b,main=c)
         hist(a,main='transformed')
         }
     )


df_master <-
    df_master %>%
    bind_cols(log.trans) %>%
    bind_cols(squares) %>%
    bind_cols(transformed) %>%
    bind_cols(sq.transformed)

# Useful code:
# cat(paste0("\'",colnames(df_master), "\'",collapse = ','))
# cat(paste0("\'",colnames(sq.transformed), "\'",collapse = ','))

#Split the data
df_master.split <- df_master %>% split(f = df_master$part)

#DONR Datasets
if (donr_flag) {
    groupedColumns.donr <- c(
        "reg",
        "home",
        "chld",
        "hinc",
        "genf",
        "wrat",
        "avhv",
        "incm",
        "inca",
        "plow",
        "npro",
        "tgif",
        "lgif",
        "rgif",
        "tdon",
        "tlag",
        "agif",
        "donr"
    )

    groupedSquaredColumns.donr <- c(
        "reg",
        "home",
        "chld",
        "hinc",
        "genf",
        "wrat",
        "avhv",
        "incm",
        "inca",
        "plow",
        "npro",
        "tgif",
        "lgif",
        "rgif",
        "tdon",
        "tlag",
        "agif",
        'sq.avhv',
        'sq.incm',
        'sq.inca',
        'sq.plow',
        'sq.npro',
        'sq.tgif',
        'sq.lgif',
        'sq.rgif',
        'sq.tdon',
        'sq.tlag',
        'sq.agif',
        "donr"
    )

    groupedTransformedColumns.donr <- c(
        "reg",
        "home",
        "chld",
        "hinc",
        "genf",
        "wrat",
        'log.avhv',
        'log.incm',
        'log.inca',
        'log.plow',
        'log.npro',
        'log.tgif',
        'log.lgif',
        'log.rgif',
        'log.tdon',
        'log.tlag',
        'log.agif',
        "donr"
    )


    ungroupedColumns.donr <- c(
        "reg1",
        "reg2",
        "reg3",
        "reg4",
        "homeH.Owner",
        "chldCHLD1",
        "chldCHLD2",
        "chldCHLD3",
        "chldCHLD4",
        "chldCHLD5",
        "hincHINC2",
        "hincHINC3",
        "hincHINC4",
        "hincHINC5",
        "hincHINC6",
        "hincHINC7",
        "genfF",
        "wratW1",
        "wratW2",
        "wratW3",
        "wratW4",
        "wratW5",
        "wratW6",
        "wratW7",
        "wratW8",
        "wratW9",
        "avhv",
        "incm",
        "inca",
        "plow",
        "npro",
        "tgif",
        "lgif",
        "rgif",
        "tdon",
        "tlag",
        "agif",
        "donr"
    )
    ungroupedSquaredColumns.donr <- c(
        "reg1",
        "reg2",
        "reg3",
        "reg4",
        "homeH.Owner",
        "chldCHLD1",
        "chldCHLD2",
        "chldCHLD3",
        "chldCHLD4",
        "chldCHLD5",
        "hincHINC2",
        "hincHINC3",
        "hincHINC4",
        "hincHINC5",
        "hincHINC6",
        "hincHINC7",
        "genfF",
        "wratW1",
        "wratW2",
        "wratW3",
        "wratW4",
        "wratW5",
        "wratW6",
        "wratW7",
        "wratW8",
        "wratW9",
        "avhv",
        "incm",
        "inca",
        "plow",
        "npro",
        "tgif",
        "lgif",
        "rgif",
        "tdon",
        "tlag",
        "agif",
        'sq.avhv',
        'sq.incm',
        'sq.inca',
        'sq.plow',
        'sq.npro',
        'sq.tgif',
        'sq.lgif',
        'sq.rgif',
        'sq.tdon',
        'sq.tlag',
        'sq.agif',
        "donr"
    )
    ungroupedTransformedColumns.donr <- c(
        "reg1",
        "reg2",
        "reg3",
        "reg4",
        "homeH.Owner",
        "chldCHLD1",
        "chldCHLD2",
        "chldCHLD3",
        "chldCHLD4",
        "chldCHLD5",
        "hincHINC2",
        "hincHINC3",
        "hincHINC4",
        "hincHINC5",
        "hincHINC6",
        "hincHINC7",
        "genfF",
        "wratW1",
        "wratW2",
        "wratW3",
        "wratW4",
        "wratW5",
        "wratW6",
        "wratW7",
        "wratW8",
        "wratW9",
        'log.avhv',
        'log.incm',
        'log.inca',
        'log.plow',
        'log.npro',
        'log.tgif',
        'log.lgif',
        'log.rgif',
        'log.tdon',
        'log.tlag',
        'log.agif',
        "donr"
    )

    ungroupedTransSqColumns.donr <- c(
        "reg1",
        "reg2",
        "reg3",
        "reg4",
        "homeH.Owner",
        "chldCHLD1",
        "chldCHLD2",
        "chldCHLD3",
        "chldCHLD4",
        "chldCHLD5",
        "hincHINC2",
        "hincHINC3",
        "hincHINC4",
        "hincHINC5",
        "hincHINC6",
        "hincHINC7",
        "genfF",
        "wratW1",
        "wratW2",
        "wratW3",
        "wratW4",
        "wratW5",
        "wratW6",
        "wratW7",
        "wratW8",
        "wratW9",
        'trans.avhv',
        'trans.incm',
        'trans.inca',
        'trans.plow',
        'trans.npro',
        'trans.tgif',
        'trans.lgif',
        'trans.rgif',
        'trans.tdon',
        'trans.tlag',
        'trans.agif',
        'sq.trans.avhv',
        'sq.trans.incm',
        'sq.trans.inca',
        'sq.trans.plow',
        'sq.trans.npro',
        'sq.trans.tgif',
        'sq.trans.lgif',
        'sq.trans.rgif',
        'sq.trans.tdon',
        'sq.trans.tlag',
        'sq.trans.agif',
        "donr"
    )

    groupedTransSqColumns.donr <- c(
        "reg",
        "home",
        "chld",
        "hinc",
        "genf",
        "wrat",
        'trans.avhv',
        'trans.incm',
        'trans.inca',
        'trans.plow',
        'trans.npro',
        'trans.tgif',
        'trans.lgif',
        'trans.rgif',
        'trans.tdon',
        'trans.tlag',
        'trans.agif',
        'sq.trans.avhv',
        'sq.trans.incm',
        'sq.trans.inca',
        'sq.trans.plow',
        'sq.trans.npro',
        'sq.trans.tgif',
        'sq.trans.lgif',
        'sq.trans.rgif',
        'sq.trans.tdon',
        'sq.trans.tlag',
        'sq.trans.agif',
        "donr"
    )

    donr_training <-
        df_master.split$train %>% dplyr::select(-damt, -part)
    donr_validating <-
        df_master.split$valid %>% dplyr::select(-damt, -part)
    donr_testing <-
        df_master.split$test %>% dplyr::select(-damt, -part)
}

#DAMT Datasets
if (damt_flag) {
    groupedColumns.damt <- c(
        "reg",
        "home",
        "chld",
        "hinc",
        "genf",
        "wrat",
        "avhv",
        "incm",
        "inca",
        "plow",
        "npro",
        "tgif",
        "lgif",
        "rgif",
        "tdon",
        "tlag",
        "agif",
        "damt"
    )

    groupedSquaredColumns.damt <- c(
        "reg",
        "home",
        "chld",
        "hinc",
        "genf",
        "wrat",
        "avhv",
        "incm",
        "inca",
        "plow",
        "npro",
        "tgif",
        "lgif",
        "rgif",
        "tdon",
        "tlag",
        "agif",
        'sq.avhv',
        'sq.incm',
        'sq.inca',
        'sq.plow',
        'sq.npro',
        'sq.tgif',
        'sq.lgif',
        'sq.rgif',
        'sq.tdon',
        'sq.tlag',
        'sq.agif',
        "damt"
    )

    groupedTransformedColumns.damt <- c(
        "reg",
        "home",
        "chld",
        "hinc",
        "genf",
        "wrat",
        'trans.avhv',
        'trans.incm',
        'trans.inca',
        'trans.plow',
        'trans.npro',
        'trans.tgif',
        'trans.lgif',
        'trans.rgif',
        'trans.tdon',
        'trans.tlag',
        'trans.agif',
        "damt"
    )


    groupedTransSqColumns.damt <- c(
        "reg",
        "home",
        "chld",
        "hinc",
        "genf",
        "wrat",
        'trans.avhv',
        'trans.incm',
        'trans.inca',
        'trans.plow',
        'trans.npro',
        'trans.tgif',
        'trans.lgif',
        'trans.rgif',
        'trans.tdon',
        'trans.tlag',
        'trans.agif',
        'sq.trans.avhv',
        'sq.trans.incm',
        'sq.trans.inca',
        'sq.trans.plow',
        'sq.trans.npro',
        'sq.trans.tgif',
        'sq.trans.lgif',
        'sq.trans.rgif',
        'sq.trans.tdon',
        'sq.trans.tlag',
        'sq.trans.agif',
        "damt"
    )

    ungroupedColumns.damt <- c(
        "reg1",
        "reg2",
        "reg3",
        "reg4",
        "homeH.Owner",
        "chldCHLD1",
        "chldCHLD2",
        "chldCHLD3",
        "chldCHLD4",
        "chldCHLD5",
        "hincHINC2",
        "hincHINC3",
        "hincHINC4",
        "hincHINC5",
        "hincHINC6",
        "hincHINC7",
        "genfF",
        "wratW1",
        "wratW2",
        "wratW3",
        "wratW4",
        "wratW5",
        "wratW6",
        "wratW7",
        "wratW8",
        "wratW9",
        "avhv",
        "incm",
        "inca",
        "plow",
        "npro",
        "tgif",
        "lgif",
        "rgif",
        "tdon",
        "tlag",
        "agif",
        "damt"
    )
    ungroupedSquaredColumns.damt <- c(
        "reg1",
        "reg2",
        "reg3",
        "reg4",
        "homeH.Owner",
        "chldCHLD1",
        "chldCHLD2",
        "chldCHLD3",
        "chldCHLD4",
        "chldCHLD5",
        "hincHINC2",
        "hincHINC3",
        "hincHINC4",
        "hincHINC5",
        "hincHINC6",
        "hincHINC7",
        "genfF",
        "wratW1",
        "wratW2",
        "wratW3",
        "wratW4",
        "wratW5",
        "wratW6",
        "wratW7",
        "wratW8",
        "wratW9",
        "avhv",
        "incm",
        "inca",
        "plow",
        "npro",
        "tgif",
        "lgif",
        "rgif",
        "tdon",
        "tlag",
        "agif",
        'sq.avhv',
        'sq.incm',
        'sq.inca',
        'sq.plow',
        'sq.npro',
        'sq.tgif',
        'sq.lgif',
        'sq.rgif',
        'sq.tdon',
        'sq.tlag',
        'sq.agif',
        "damt"
    )
    ungroupedTransformedColumns.damt <- c(
        "reg1",
        "reg2",
        "reg3",
        "reg4",
        "homeH.Owner",
        "chldCHLD1",
        "chldCHLD2",
        "chldCHLD3",
        "chldCHLD4",
        "chldCHLD5",
        "hincHINC2",
        "hincHINC3",
        "hincHINC4",
        "hincHINC5",
        "hincHINC6",
        "hincHINC7",
        "genfF",
        "wratW1",
        "wratW2",
        "wratW3",
        "wratW4",
        "wratW5",
        "wratW6",
        "wratW7",
        "wratW8",
        "wratW9",
        'trans.avhv',
        'trans.incm',
        'trans.inca',
        'trans.plow',
        'trans.npro',
        'trans.tgif',
        'trans.lgif',
        'trans.rgif',
        'trans.tdon',
        'trans.tlag',
        'trans.agif',
        "damt"
    )

    damt_training <- df_master.split$train %>%
        filter(donr == 'Donor') %>%
        dplyr::select(-donr, -part)
    damt_validating <- df_master.split$valid %>%
        filter(donr == 'Donor') %>%
        dplyr::select(-donr, -part)
    damt_testing <- df_master.split$test %>%
        filter(donr == 'Donor') %>%
        dplyr::select(-donr, -part)
}

#Cleanup
rm(mm)
rm(df_master.split)
rm(ungroupedCols)
rm(donr_flag)
rm(damt_flag)
rm(squares)
rm(log.trans)
rm(transformed)

# Remove outliers from damt train set:
damt_training <- damt_training %>%
    filter(trans.tlag>0.5)

# Variables with high pair-wise correlations
tooHigh <- findCorrelation(cor(damt_training[, c(trans_columns, 'damt')]),
                           cutoff = .85,
                           verbose = T, names = T)
tooHigh

# If these are to be removed:
# damt_training %>% dplyr::select(-one_of(tooHigh))