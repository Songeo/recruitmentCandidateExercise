





# REQUIREMENTS ----
# libraries requierements ----
sapply(c(# manupulation & transformation libraries
  'tidyverse', 'lubridate',
  # plotting extras 
  'gridExtra',
  # modeling libraries
  'arm', 'broom', 
  # shiny libraries
  'shiny', 'DT'), 
  function(pkg){
    res <- require(pkg, character.only = T, quietly = T)
    
    if(!res){
      message('...downloading pckg...')
      install.packages(pkg)
      message('...installed pckg...')
      
      res <- require(pkg, character.only = T, quietly = T)
    }
    
    return(res)
  })

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



# FUNCTIONS ----
source("helper.R")
