



# libraries requierements ----
sapply(c(# manupulation & transformation libraries
         'here', 'tidyverse', 'lubridate',
         # plotting extras 
         'gridExtra',
         # modeling libraries
         'arm', 'forecast', 'sweep', 
         # shiny libraries
         'shiny'), 
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
theme_set(theme_bw(base_size = 14))


# project working directory ----
setwd(here::here())
getwd()

# reading data ----
tbl_data <- 
  read_csv("data.csv") %>% 
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

# eda ----
tbl_data %>% dim() # 144 x 5
tbl_data %>% summary() # jan 2014 to oct 2016

# The advertising has specifically intended to increase in Search volumes and over time three different (non-overlapping) advertising campaigns have been used.

gg.1 <- 
  tbl_data %>% 
  ggplot(aes(x = date_week_fmt, 
             y = search_volume)) + 
  geom_line(color = 'gray80') + 
  geom_point(aes(color = factor(media_campaign))) + 
  ylab('Search Volume') + 
  xlab('Date (Week)') + 
  guides(color = guide_legend("Media\nCampaign"))

gg.2 <- 
  tbl_data %>% 
  ggplot(aes(x = date_week_fmt, 
             y = media_spend_usd)) + 
  geom_line(color = 'gray80') + 
  geom_point(aes(color = factor(media_campaign))) + 
  ylab('Media Spend (USD)') + 
  xlab('Date (Week)') + 
  guides(color = guide_legend("Media\nCampaign"))

grid.arrange(gg.1, gg.2, ncol = 1)


# adstock ----


# (filter) Applies linear filtering to a univariate time series or to each series separately of a multivariate time series
# x: a univariate or multivariate time series.
# filter: a vector of filter coefficients in reverse time order (as for AR or MA coefficients).
# method: Either "convolution" or "recursive" (and can be abbreviated). If "convolution" a moving average is used: if "recursive" an autoregression is used.

fct_rf <- .5

# adstock function
fun_adstock_calc <- 
  function(spend, rf){
    # adstock (i) = Spend (i) + [ rf x Adstock (i-1) ]
    # Input: 
    # - spend: vector of media spend
    # - rf: retention factor [0, 1]
    # Output:
    # - adstock: adstock measure
    
    adstock <- stats::filter(spend, rf, method = 'recursive')
    return(adstock)
}


fun_adstock_calc(tbl_searches$media_spend_usd[1:10], fct_rf)




