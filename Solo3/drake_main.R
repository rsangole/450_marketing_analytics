library(drake)
library(tidyverse)
library(janitor)
library(lattice)
library(Amelia)
library(rpart)
library(rpart.plot)
library(tree)
library(rattle)

pkgconfig::set_config("drake::strings_in_dots" = "literals") # New file API

source('lib/data_utils.R')
source('lib/model_utils.R')
source('utils.R')

data_plan <- drake_plan(
  raw_data = data_read(),
  df = data_prep_A(raw_data) %>% data_prep_removevars(),
  df_keep1 = df %>% data_prep_keep1()
)

model_plan <- drake_plan(
  mice_obj = model_get_mice(df, method = 'cart'),
  df_imp = model_impute(mice_obj, df),
  train_df = df_imp %>% dplyr::filter(LEARNING_TEST=='LEARNING') %>% dplyr::select(-LEARNING_TEST),
  test_df = df_imp %>% dplyr::filter(LEARNING_TEST=='TEST') %>% dplyr::select(-LEARNING_TEST)
)

full_plan <- bind_plans(data_plan, model_plan)

# Run your work with make().
full_plan
outdated()
make(full_plan)

# See also loadd(), readd(), vis_drake_graph(), and drake_config(). # nolint