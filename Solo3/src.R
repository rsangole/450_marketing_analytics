imputed_subset <- complete(imputed)
imputed_subset <- as_tibble(imputed_subset)
imputed_subset

df <- readd(df_keep1)
imputed_subset <- imputed_subset %>% 
  bind_cols(df[c('Y', 'LEARNING_TEST')])

glimpse(imputed_subset)

training_df <- imputed_subset %>% filter(LEARNING_TEST == 'LEARNING') %>% select(-LEARNING_TEST)
test_df <- imputed_subset %>% filter(LEARNING_TEST != 'LEARNING') %>% select(-LEARNING_TEST)
dim(training_df)
dim(test_df)

