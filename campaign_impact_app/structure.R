





# REQUIREMENTS ----
# libraries requierements ----
library(tidyverse)
library(lubridate)
library(gridExtra)
library(arm)
library(broom)
library(DT)


# ggplot theme bw default with bigger font size
theme_set(theme_bw(base_size = 16))


# DATA ----
# reading data ----
tbl_data <- 
  read_csv("data/data.csv") %>% 
  # column name modifications to standard
  rename_all(function(col){
    col %>% 
      tolower %>% 
      str_replace_all(" ", "_") %>% 
      str_replace_all("\\(", "") %>% 
      str_replace_all("\\)", "") 
  }) %>% 
  # date_week to date type 
  mutate(date_week_fmt = dmy(date_week))



