predictor <- 'Y'
allNums <- training_df %>% select_if(is.numeric) %>% names
allCats <- head(training_df %>% select_if(is.factor) %>% names,-1)
allNums; allCats
length(allNums)+length(allCats)+1 == dim(training_df)[2]

folds <- caret::createFolds(y = training_df$Y, k = 10, returnTrain = T)

folds <- caret::createFolds(y = training_df$Y, k = 10)
training_df[folds$Fold01,] %>% count(Y) %>% janitor::adorn_totals()

mkPredC <- function(outCol,varCol,appCol){
  ppos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pposWNA <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol),varCol)
  pposWV <- (vTab[pos,]+1e-3*ppos)/(colSums(vTab)+1e-3)
  pred <- pposWV[appCol]
  pred[is.na(appCol)] <- pposWNA
  pred[is.na(pred)] <- ppos
  tibble(X = pred)
}

pos <- 'RESPONSE'

cat_Preds <- allCats %>% 
  purrr::map_dfc(~mkPredC(training_df[[predictor]], training_df[[.x]], training_df[[.x]]))
names(cat_Preds) <- allCats
cat_Preds

library(ROCR)

calcAUC <- function(predcol, outcol){
  perf <- ROCR::performance(ROCR::prediction(predcol, outcol==pos),'auc')
  as.numeric(perf@y.values)
}
for(v in allCats){
  # pi <- glue::glue('pred{v}')
  aucTrain <- round(calcAUC(cat_Preds[[v]], training_df[[predictor]]),3)
  message(glue::glue('TrainAUC for variable {v} is \t\t\t\t\t{aucTrain}'))
}


# --- from scratch ---

char_to_factor_vars <- raw_data %>% select_if(is.character) %>% names
raw_data[char_to_factor_vars] <- raw_data[char_to_factor_vars] %>% purrr::map_df( ~ as.factor(.x))
predictor <- 'RESPONSE16'
raw_data$RESPONSE16 <- factor(
  raw_data$RESPONSE16,
  levels = c(0, 1),
  labels = c('NORESPONSE', 'RESPONSE')
)
allCats <- raw_data %>% select_if(is.factor) %>% names
cat_Preds <- allCats %>% purrr::map_dfc(~mkPredC(raw_data[[predictor]], raw_data[[.x]], raw_data[[.x]]))
names(cat_Preds) <- allCats
search_cats <- tibble(param = allCats)
for(v in allCats){
  aucTrain <- round(calcAUC(cat_Preds[[v]], raw_data[[predictor]]),3)
  message(glue::glue('TrainAUC for variable {v} is \t\t\t\t\t{aucTrain}'))
  search_cats[search_cats$param==v,'auc'] <- aucTrain
}
search_cats %>% arrange(auc)
