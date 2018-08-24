model_get_mice <- function(input_data,...){
  subset_data <- input_data %>% dplyr::select(-Y,-LEARNING_TEST)
  parlmice(data = subset_data,...)
}
model_mice_impute <- function(mice_obj, df){
  as_tibble(complete(mice_obj)) %>% 
    bind_cols(df[c('Y','LEARNING_TEST')])
}


# train_df <- readd(train_df)
# 
# cormat <- cor(train_df),-
# 
# 
# rpartFit <- rpart(Y~., train_df, method = 'class')
# 
# 
# rf1 <- randomForest(RESPONSE16 ~ PRE2009SALES+MED_INC+ESTHMVL+
#                       salepertrans+EXAGE
#                     ,data=subdat3,importance=TRUE,ntree=100)
# summary(rf1)
# 
# 
# train_df %>% dplyr::select(Y, matches('^Z')) %>% na.omit() %>%  
#   train(Y~.,data=., method='rf')
# 
# rf2 <- train(Y~., 
#              data = train_df %>% na.omit(), 
#              method="rf", 
#              ntree = 100)
# 
