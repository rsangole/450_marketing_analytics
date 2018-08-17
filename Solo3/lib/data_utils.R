data_read <- function(){
  read_csv('input_data/complete_cust_data.csv', col_names = T)
}
prep_nzv <- function(df, remove_nzv=T){
  nzvResults <- caret::nearZeroVar(df, foreach = T, saveMetrics = T)
  zv_cols_to_remove <- rownames(nzvResults[nzvResults$zeroVar,])
  df[zv_cols_to_remove] <- NULL
  if(remove_nzv){
    nzv_cols_to_remove <- rownames(nzvResults[nzvResults$nzv,])
    df[nzv_cols_to_remove] <- NULL
  }
  df
}
prep_adultage <- function(df){
  # Rule: If ADxAGE == U, then ADx_ESTAGE == age, else it's 00
  # Both are NA at the same time
  age_cols <- colnames(df) %>% stringr::str_extract(pattern = 'AD[0-9A-Z_]*AGE') %>% na.omit()
  df$AD2AGE = ifelse(df$AD2AGE=='U', df$AD2_ESTAGE, df$AD2AGE)
  df$AD3AGE = ifelse(df$AD3AGE=='U', df$AD3_ESTAGE, df$AD3AGE)
  df$AD4AGE = ifelse(df$AD4AGE=='U', df$AD4_ESTAGE, df$AD4AGE)
  df$AD5AGE = ifelse(df$AD5AGE=='U', df$AD5_ESTAGE, df$AD5AGE)
  df$AD6AGE = ifelse(df$AD6AGE=='U', df$AD6_ESTAGE, df$AD6AGE)
  df$AD7AGE = ifelse(df$AD7AGE=='U', df$AD7_ESTAGE, df$AD7AGE)
  df$AD8AGE = ifelse(df$AD8AGE=='U', df$AD8_ESTAGE, df$AD8AGE)
  df$AD2_ESTAGE <- NULL
  df$AD3_ESTAGE <- NULL
  df$AD4_ESTAGE <- NULL
  df$AD5_ESTAGE <- NULL
  df$AD6_ESTAGE <- NULL
  df$AD7_ESTAGE <- NULL
  df$AD8_ESTAGE <- NULL
  df
}
prep_adult_g_r <- function(df){
  df$ADULT2_G <- factor(df$ADULT2_G, levels = c('M','F','U'))
  df$ADULT3_G <- factor(df$ADULT3_G, levels = c('M','F','U'))
  df$ADULT4_G <- factor(df$ADULT4_G, levels = c('M','F','U'))
  df$ADULT5_G <- factor(df$ADULT5_G, levels = c('M','F','U'))
  df$ADULT6_G <- factor(df$ADULT6_G, levels = c('M','F','U'))
  df$ADULT7_G <- factor(df$ADULT7_G, levels = c('M','F','U'))
  
  df$ADULT2_R <- factor(df$ADULT2_R, levels = c('H','P','U','W','Y'))
  df$ADULT3_R <- factor(df$ADULT3_R, levels = c('H','P','U','W','Y'))
  df$ADULT4_R <- factor(df$ADULT4_R, levels = c('H','P','U','W','Y'))
  df$ADULT5_R <- factor(df$ADULT5_R, levels = c('H','P','U','W','Y'))
  df$ADULT6_R <- factor(df$ADULT6_R, levels = c('H','P','U','W','Y'))
  df$ADULT7_R <- factor(df$ADULT7_R, levels = c('H','P','U','W','Y'))
  
  df$ADULT8_G <- NULL
  df$ADULT8_R <- NULL
  
  df
}
prep_addbackcols <- function(df, raw_data){
  df$YTD_SALES_2009 <- raw_data$YTD_SALES_2009
  df
}
prep_removecols <- function(df){
  df$ACCTNO <- NULL
  df$AMEX_REG <- NULL
  df$BLOCK <- NULL
  df$BLOCK_ID <- NULL
  df$DPBC <- NULL
  df$FILLER <- NULL
  df
}
prep_tonumeric <- function(df){
  df$ESTHMVL <- as.numeric(df$ESTHMVL)
  df$ESTLOANTOVALRNG <- as.numeric(df$ESTLOANTOVALRNG)
  df$LOAN_TRM <- as.numeric(df$LOAN_TRM)
  df$EXAGE <- as.numeric(df$EXAGE)
  df$IMPACT <- as.numeric(df$IMPACT)
  df$LOR1 <- as.numeric(df$LOR1)
  
  df
}
prep_refactor <- function(df){
  df$ETHNIC_DETAIL <- forcats::fct_lump(df$ETHNIC_DETAIL,prop = 0.01)
  df$ETHNIC_GROUP <- forcats::fct_lump(df$ETHNIC_GROUP,prop = 0.004)
  df$GEOPIXELCODE <- forcats::fct_lump(df$GEOPIXELCODE,prop = 0.004)
  
  df
}
prep_cleanups <- function(df){
  df %>% janitor::clean_names()
}
data_prep_A <- function(df,raw_data){
  df %>% 
    prep_adultage() %>% 
    prep_adult_g_r() %>% 
    prep_nzv() %>% 
    prep_removecols() %>% 
    prep_addbackcols(.,raw_data) %>% 
    prep_tonumeric() %>% 
    prep_refactor() %>% 
    prep_cleanups()
}