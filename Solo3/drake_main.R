library(drake)
library(tidyverse)
pkgconfig::set_config("drake::strings_in_dots" = "literals") # New file API

data_read <- function(){
  df <- load('input_data/XYZ_complete_customer_data_frame.RData')
}
data_cleanse <- function(){
  
}
data_prep_A <- function(){
  prep_1()
  prep_2()
  #...
}
data_prep_B <- function(){
  prep_1()
  prep_2()
  #...
}
eda_plots <- function(){
  
}
eda_summaries <- function(){
  
}

create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram(binwidth = 0.25) +
    theme_gray(20)
}

# The workflow plan data frame outlines what you are going to do.

plan <- drake_plan(
  raw_data = readxl::read_excel(file_in("raw_data.xlsx")),
  data = raw_data %>%
    mutate(Species = forcats::fct_inorder(Species)) %>%
    select(-X__1),
  hist = create_plot(data),
  fit = lm(Sepal.Width ~ Petal.Width + Species, data),
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)

# Run your work with make().

make(plan)

# See also loadd(), readd(), vis_drake_graph(), and drake_config(). # nolint

# --- # --- #


