subset_data <- read_rds(path = 'subsetdata_for_miceimpute.rdata')
imputed <- read_rds(path = 'df_imputed_miceobject.rdata')

imputed_subset <- complete(imputed)
imputed_subset <- as_tibble(imputed_subset)
imputed_subset

imputed_subset$AD2AGE <- as.numeric(imputed_subset$AD2AGE)
imputed_subset$EXAGE <- as.numeric(imputed_subset$EXAGE)

df <- readd(df_keep1)
imputed_subset <- imputed_subset %>% 
  bind_cols(df[c('Y', 'LEARNING_TEST')])

glimpse(imputed_subset)

char_to_factor_vars <- imputed_subset %>% select_if(is.character) %>% names
imputed_subset[char_to_factor_vars] <- imputed_subset[char_to_factor_vars] %>% purrr::map_df(~as.factor(.x))

glimpse(imputed_subset)

training_df <- imputed_subset %>% filter(LEARNING_TEST == 'LEARNING') %>% dplyr::select(-LEARNING_TEST)
test_df <- imputed_subset %>% filter(LEARNING_TEST != 'LEARNING') %>% dplyr::select(-LEARNING_TEST)
dim(training_df)
dim(test_df)


corrmat <- cluster::daisy(training_df[-120], metric = 'gower')

corrmat <- cor(training_df)


# Variables with high pair-wise correlations
tooHigh <- (findCorrelation(,cutoff = .85,verbose = T, names = T))
tooHigh
