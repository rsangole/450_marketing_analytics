data_read <- function() {
  read_csv('input_data/complete_cust_data.csv', col_names = T)
}
prep_nzv <- function(df, remove_nzv = T) {
  nzvResults <- caret::nearZeroVar(df, foreach = T, saveMetrics = T)
  zv_cols_to_remove <- rownames(nzvResults[nzvResults$zeroVar, ])
  df[zv_cols_to_remove] <- NULL
  if (remove_nzv) {
    nzv_cols_to_remove <- rownames(nzvResults[nzvResults$nzv, ])
    message('Removing columns:', glue::glue_collapse(nzv_cols_to_remove,', '))
    df[nzv_cols_to_remove] <- NULL
  }
  df
}
prep_adultage <- function(df) {
  # Rule: If ADxAGE == U, then ADx_ESTAGE == age, else it's 00
  # Both are NA at the same time
  age_cols <-
    colnames(df) %>% stringr::str_extract(pattern = 'AD[0-9A-Z_]*AGE') %>% na.omit()
  df$AD2AGE = ifelse(df$AD2AGE == 'U', df$AD2_ESTAGE, df$AD2AGE)
  df$AD3AGE = ifelse(df$AD3AGE == 'U', df$AD3_ESTAGE, df$AD3AGE)
  df$AD4AGE = ifelse(df$AD4AGE == 'U', df$AD4_ESTAGE, df$AD4AGE)
  df$AD5AGE = ifelse(df$AD5AGE == 'U', df$AD5_ESTAGE, df$AD5AGE)
  df$AD6AGE = ifelse(df$AD6AGE == 'U', df$AD6_ESTAGE, df$AD6AGE)
  df$AD7AGE = ifelse(df$AD7AGE == 'U', df$AD7_ESTAGE, df$AD7AGE)
  df$AD8AGE = ifelse(df$AD8AGE == 'U', df$AD8_ESTAGE, df$AD8AGE)
  df$AD2_ESTAGE <- NULL
  df$AD3_ESTAGE <- NULL
  df$AD4_ESTAGE <- NULL
  df$AD5_ESTAGE <- NULL
  df$AD6_ESTAGE <- NULL
  df$AD7_ESTAGE <- NULL
  df$AD8_ESTAGE <- NULL
  df
}
prep_adult_g_r <- function(df) {
  df$ADULT2_G <- factor(df$ADULT2_G, levels = c('M', 'F', 'U'))
  df$ADULT3_G <- factor(df$ADULT3_G, levels = c('M', 'F', 'U'))
  df$ADULT4_G <- factor(df$ADULT4_G, levels = c('M', 'F', 'U'))
  df$ADULT5_G <- factor(df$ADULT5_G, levels = c('M', 'F', 'U'))
  df$ADULT6_G <- factor(df$ADULT6_G, levels = c('M', 'F', 'U'))
  df$ADULT7_G <- factor(df$ADULT7_G, levels = c('M', 'F', 'U'))
  
  df$ADULT2_R <-
    factor(df$ADULT2_R, levels = c('H', 'P', 'U', 'W', 'Y'))
  df$ADULT3_R <-
    factor(df$ADULT3_R, levels = c('H', 'P', 'U', 'W', 'Y'))
  df$ADULT4_R <-
    factor(df$ADULT4_R, levels = c('H', 'P', 'U', 'W', 'Y'))
  df$ADULT5_R <-
    factor(df$ADULT5_R, levels = c('H', 'P', 'U', 'W', 'Y'))
  df$ADULT6_R <-
    factor(df$ADULT6_R, levels = c('H', 'P', 'U', 'W', 'Y'))
  df$ADULT7_R <-
    factor(df$ADULT7_R, levels = c('H', 'P', 'U', 'W', 'Y'))
  
  df$ADULT8_G <- NULL
  df$ADULT8_R <- NULL
  
  df
}
prep_removecols <- function(df) {
  df %>%
    select(
      -ACCTNO,
      -AMEX_REG,
      -BLOCK,
      -BLOCK_ID,
      -DPBC,
      -FILLER,
      -CRRT,
      -DATEDEED,
      -DPBC,
      -VETERAN,
      -ZHMDECOP,
      -ZIP4,
      -ZIP9_Supercode,
      -TRACT,
      -MCD_CCD,
      -LONG,
      -LAT,
      -ZIP,
      -PHOMOWNR,
      -LOAN_KND,
      -PHONEMATCH,
      -TRANSTYP
    )
}
prep_to_factor <- function(){

}
prep_tonumeric <- function(df) {
  df$ESTHMVL <- as.numeric(df$ESTHMVL)
  df$ESTLOANTOVALRNG <- as.numeric(df$ESTLOANTOVALRNG)
  df$LOAN_TRM <- as.numeric(df$LOAN_TRM)
  df$EXAGE <- as.numeric(df$EXAGE)
  df$IMPACT <- as.numeric(df$IMPACT)
  df$LOR1 <- as.numeric(df$LOR1)
  
  df
}
prep_refactor <- function(df) {
  df$ETHNIC_DETAIL <- forcats::fct_lump(df$ETHNIC_DETAIL, prop = 0.01)
  df$ETHNIC_GROUP <- forcats::fct_lump(df$ETHNIC_GROUP, prop = 0.004)
  df$GEOPIXELCODE <- forcats::fct_lump(df$GEOPIXELCODE, prop = 0.004)
  df$M_GLOBAL_Z4 <- forcats::fct_lump(df$M_GLOBAL_Z4, prop = 0.004)
  
  df
}
prep_mutates <- function(df){
  df %>% 
    mutate(
      # How many times has the customer responded in the past, till campaigh 15?
      cusum_resp_till15 = RESPONSE0+RESPONSE1+RESPONSE2+RESPONSE3+RESPONSE4+RESPONSE5+RESPONSE6+RESPONSE7+RESPONSE8+RESPONSE9+RESPONSE10+RESPONSE11+RESPONSE12+RESPONSE13+RESPONSE14+RESPONSE15,
      # How many things were purchased till 15?
      cusum_qty_till15 = QTY+QTY0+QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8+QTY9+QTY10+QTY11+QTY12+QTY13+QTY14+QTY15,
      # What's the total $ purchased till 15?
      cusum_tot_usd_till15 = TOTAMT0+TOTAMT1+TOTAMT2+TOTAMT3+TOTAMT4+TOTAMT5+TOTAMT6+TOTAMT7+TOTAMT8+TOTAMT9+TOTAMT10+TOTAMT11+TOTAMT12+TOTAMT13+TOTAMT14+TOTAMT15,
      old_response_15 = RESPONSE15,
      old_response_14 = RESPONSE14,
      cusum_mailers_till15 = TOTAL_MAIL_15,
      mailers_in_15 = SUM_MAIL_15,
      PRE2009_SALES = LTD_SALES - YTD_SALES_2009,
      PRE2009_TRANSACTIONS = LTD_TRANSACTIONS-YTD_TRANSACTIONS_2009,
      Y = RESPONSE16
    ) %>% 
    dplyr::select(-LTD_SALES, -YTD_SALES_2009, -LTD_TRANSACTIONS, 
                  -YTD_TRANSACTIONS_2009) %>% 
    dplyr::select(-starts_with('ANY_MAIL')) %>% 
    dplyr::select(-starts_with('SUM_MAIL')) %>% 
    dplyr::select(-starts_with('TOTAL_MAIL')) %>% 
    dplyr::select(-starts_with('QTY')) %>% 
    dplyr::select(-starts_with('TOTAMT')) %>% 
    dplyr::select(-starts_with('RESPONSE')) %>% 
    dplyr::select(-ends_with('16'))
}
prep_cleanups <- function(df) {
  df %>% janitor::clean_names(case = 'all_caps')
}
data_prep_A <- function(df) {
  df %>%
    prep_adultage() %>%
    prep_adult_g_r() %>%
    prep_removecols() %>%
    prep_tonumeric() %>%
    prep_refactor() %>%
    prep_mutates() %>% 
    prep_nzv(remove_nzv = F) %>%
    prep_cleanups()
}

# Remove all Z variables
data_prep_removevars <- function(df){
  df %>% 
    select(-matches('^Z([A-Z]*)P$')) %>% 
    select(-LOAN_TRM, -LOAN_TYP) %>% 
    select(-matches('^AD[3-9]AGE$')) %>% 
    select(-matches('^AD[0-9]_ESTAGE')) %>% 
    select(-matches('ADULT[3-9]_[RG]'))
  # %>% 
    # select(-matches('^AD'))
}

data_prep_keep1 <- function(df){
  keep1 <- c(
    'AD_MAGAZINE',
    'AD_NEWSPAPER',
    'AD_RADIO',
    'AD_TV',
    'AD_WEB',
    'AD2AGE',
    'ADULT1_G',
    'ADULT1_R',
    'ADULT2_G',
    'ADULT2_R',
    'ALL_INTHE_NAME',
    'AVG_COMMUTETIM',
    'BUY_AMERICAN',
    'BUYER_STATUS',
    'C_00_03',
    'C_04_06',
    'C_07_09',
    'C_10_12',
    'C_13_18',
    'CHANNEL_ACQUISITION',
    'CHANNEL_DOMINANCE',
    'CTINCIND',
    'CUR_EST_MED_INC',
    'DISC_REG',
    'DM_BOOKS',
    'DM_CRAFT',
    'DM_FEM',
    'DM_FOODS',
    'DM_GEN',
    'DM_GIFTS',
    'DM_MALE',
    'DM_UP',
    'DOITSELF',
    'EMAIL_RECEPTIVE',
    'ESTLOANTOVALRNG',
    'ESTMORTAMTRNG',
    'ESTMORTPAYRNG',
    'ETHNIC_GROUP',
    'EXAGE',
    'LOAN_AMT',
    'LOAN_TRM',
    'LOAN_TYP',
    'LOOK_ATME_NOW',
    'M_GRPTYPE_MEDIAN',
    'M_HH_LEVEL',
    'MAILPREF',
    'MARRIED',
    'MC_REG',
    'MED_DWELL_AGE',
    'MED_FAMINCOM',
    'MED_HOME',
    'MED_INC',
    'MED_LOR',
    'MED_NOFAMINCOM',
    'MED_RENT',
    'MED_TRAV_TOWRK',
    'MEDIANAGE',
    'NEVER_EMPTY_HANDED',
    'NEWCAR',
    'NO_TIME_PRESENT',
    'OCCUPATION_GROUP',
    'P_CAPITA_INCOM',
    'P_FAMHHCHILD',
    'P_HHINCOLES10M',
    'P_HHINCOM10_14',
    'P_HHINCOM100_124',
    'P_HHINCOM125_149',
    'P_HHINCOM15_19',
    'P_HHINCOM150_199',
    'P_HHINCOM20_24',
    'P_HHINCOM20_UP',
    'P_HHINCOM25_29',
    'P_HHINCOM30_34',
    'P_HHINCOM35_39',
    'P_HHINCOM40_44',
    'P_HHINCOM45_49',
    'P_HHINCOM50_59',
    'P_HHINCOM60_74',
    'P_HHINCOM75_99',
    'P_MARRY',
    'P_NONHISP',
    'P_OWNOCC',
    'P_RENTER',
    'PCARPOOL',
    'PENNY_SAVED_EARNED',
    'PHHWHITE',
    'POP_UN18',
    'POP18_65',
    'POP25_34',
    'POP35_44',
    'POP45_54',
    'POP55_64',
    'POP65_P',
    'POP75_UP',
    'PRESCHLD',
    'RELIGION',
    'RRC',
    'SALES',
    'SHOW_ME_MONEY',
    'SPORTS_RELATED',
    'SSN',
    'STOCK_BOND_CURRENT',
    'STOP_SMELL_ROSES',
    'SWEEPS',
    'TRANSTYP',
    'UNIT_MORTG_1ST',
    'USEDCAR',
    'VISA_REG',
    'WORK_PLAY_HARD',
    'WRK_TRA_60_89',
    'WRK_TRA_90_UP',
    'ZCREDIT',
    'ZCREDPLP',
    'ZCREDPLT',
    'PRE2009_SALES',
    'PRE2009_TRANSACTIONS',
    'CUSUM_RESP_TILL15',
    'CUSUM_QTY_TILL15',
    'CUSUM_TOT_USD_TILL15',
    'OLD_RESPONSE_15',
    'OLD_RESPONSE_14',
    'CUSUM_MAILERS_TILL15',
    'MAILERS_IN_15',
    'Y',
    'LEARNING_TEST'
  )
  df %>% 
    dplyr::select(one_of(keep1)) %>% 
    mutate(
      OCCUPATION_GROUP = forcats::fct_lump(OCCUPATION_GROUP, prop = 0.03)
    )
}