library(drake)
library(tidyverse)
library(janitor)
library(lattice)

pkgconfig::set_config("drake::strings_in_dots" = "literals") # New file API

source('lib/data_utils.R')
source('utils.R')

data_plan <- drake_plan(
  raw_data = data_read(),
  df_A = data_prep_A(raw_data)
)

# Run your work with make().
data_plan
outdated()
make(data_plan)

# See also loadd(), readd(), vis_drake_graph(), and drake_config(). # nolint

# --- # --- #

skim_df <- skimr::skim_to_list(readd(raw_data))
skim_df$character %>% View()
nzvResults <- caret::nearZeroVar(df, foreach = T, saveMetrics = T)
nzvResults
nzvResults$zeroVar %>% tabyl()
nzvResults %>% dplyr::filter(zeroVar)

# --- # --- #

prep_chr_to_factor <- function(){
  ADD_TYPE
  AMEX_REG
  
}


DF <- readd(df_A)
tc(DF$DISC_REG)
purrtc(DF,'HH')
