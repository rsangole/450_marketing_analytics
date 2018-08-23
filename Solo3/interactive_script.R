# --- # --- #
skim_df <- skimr::skim_to_list(readd(raw_data))
skim_df$character %>% View()
nzvResults <- caret::nearZeroVar(df, foreach = T, saveMetrics = T)
nzvResults
nzvResults$zeroVar %>% tabyl()
nzvResults %>% dplyr::filter(zeroVar)
# --- # --- #

dfa <- readd(df_A)
glimpse(dfa)


tc(DF$DISC_REG)
purrtc(DF,'MED')


train_df <- readd(train_df)
dim(train_df)

chunk <- function(x, n) split(x, sort(rank(x) %% n))
chunks <- chunk(1:ncol(train_df),10)
chunks %>% 
  purrr::map(~missmap(train_df[,.x],y.cex = 0.0001, x.cex = 0.8, col = c('black','yellow')))

#All the Zs have the same missingness
#All CON, MGZ,DM... have same missingness
#All ADULT_have similar missingness... LARGE amounts

train_df$LOAN_TRM %>% tabyl()

map_dbl(tdf, ~(sum(is.na(.x))/length(.x))) %>% sort(decreasing = T) %>% head(20)

