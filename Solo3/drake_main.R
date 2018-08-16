library(drake)
library(tidyverse)
library(janitor)

pkgconfig::set_config("drake::strings_in_dots" = "literals") # New file API

source('lib/data_utils.R')

data_plan <- drake_plan(
  raw_data = data_read(),
  df_a = data_prep_A(raw_data)
)

# Run your work with make().

make(data_plan)

# See also loadd(), readd(), vis_drake_graph(), and drake_config(). # nolint

# --- # --- #

skim_df <- skimr::skim_to_list(readd(raw_data))
nzvResults <- caret::nearZeroVar(df, foreach = T, saveMetrics = T)
nzvResults
nzvResults$zeroVar %>% tabyl()
nzvResults %>% dplyr::filter(zeroVar)

# --- # --- #

prep_chr_to_factor <- function(){
  ADD_TYPE
  AMEX_REG
  
}