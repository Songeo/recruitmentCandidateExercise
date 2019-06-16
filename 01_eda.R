


# REQUIREMENTS ----
# libraries requierements ----
sapply(c(# manupulation & transformation libraries
         'here', 'tidyverse', 'lubridate',
         # plotting extras 
         'gridExtra',
         # modeling libraries
         'arm', 'forecast', 'broom', 
         # shiny libraries
         'shiny', 'rmarkdown'), 
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


# DATA ----
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

# EDA ----
tbl_data %>% dim() # 144 x 5
tbl_data %>% summary() # jan 2014 to oct 2016

tbl_data$date_week

# The advertising has specifically intended to increase in Search volumes and over time three different (non-overlapping) advertising campaigns have been used.

# searh volume ----
gg.1 <- 
  tbl_data %>% 
  ggplot(aes(x = date_week_fmt, 
             y = search_volume)) + 
  geom_line(color = 'gray80') + 
  geom_point(aes(color = factor(media_campaign))) + 
  ylab('\nSearch Volume') + 
  xlab('Date (Week)') + 
  guides(color = guide_legend("Media\nCampaign"))

# media spend ----
gg.2 <- 
  tbl_data %>% 
  ggplot(aes(x = date_week_fmt, 
             y = media_spend_usd)) + 
  geom_line(color = 'gray80') + 
  geom_point(aes(color = factor(media_campaign))) + 
  ylab('\nMedia Spend (USD)') + 
  xlab('Date (Week)') + 
  guides(color = guide_legend("Media\nCampaign"))

# grid.arrange(gg.1, gg.2, ncol = 1)


# ADSTOCK ----

# (filter) Applies linear filtering to a univariate time series or to each series separately of a multivariate time series
# x: a univariate or multivariate time series.
# filter: a vector of filter coefficients in reverse time order (as for AR or MA coefficients).
# method: Either "convolution" or "recursive" (and can be abbreviated). If "convolution" a moving average is used: if "recursive" an autoregression is used.


# adstock function ----
fun_adstock_calc <- 
  function(spend, rf){
    # adstock (i) = Spend (i) + [ rf x Adstock (i-1) ]
    # Input: 
    # - spend: vector of media spend
    # - rf: retention factor [0, 1]
    # Output:
    # - adstock: adstock measure
    
    adstock <- stats::filter(spend, rf, method = 'recursive') %>% 
      as.numeric()
    return(adstock)
}

# adstock (reactive)  ----

# input variable shiny
fct_rf <- .5 
tbl_adstock <- 
  tbl_data %>% 
  mutate_at('media_campaign', factor) %>% 
  arrange(date_week_fmt) %>% 
  group_by(media_campaign) %>% 
  mutate(adstock = fun_adstock_calc(media_spend_usd, fct_rf)) %>% 
  ungroup()

gg.3 <- 
  tbl_adstock %>% 
  ggplot(aes(x = date_week_fmt, 
             y = adstock)) + 
  geom_line(aes(color = factor(media_campaign)) ) + 
  geom_line(aes(y = media_spend_usd), color = 'gray80' ) +
  ylab('Adstock\nMedia Spend (USD)') + 
  xlab('Date (Week)') + 
  guides(color = guide_legend("Media\nCampaign"))
gg.3

grid.arrange(gg.1, gg.2, gg.3, ncol = 1)



# MODELS ----

# Model 1 ----
tbl_mod <- tbl_adstock %>% 
  mutate(lag = lag(search_volume))

# (fun) model 1 ----
fun_model_lm <- 
  function(tbl_mod){
    # Input: 
    # - tbl_mod: data
    # Output: 
    # - list of elements: 
    #   * mod: model
    #   * tab_eff: model coeficients and intervals
    #   * tab_fit: model fit 
    mod <- lm(formula = "search_volume ~ adstock + media_campaign ", 
              data = tbl_mod)
    mod %>% glance
    mod %>% summary()
    
    tab_eff <- mod %>% tidy()
    
    tab_fit <- tbl_mod %>% 
      left_join(mod %>% 
                  augment %>% 
                  mutate(date_week_fmt = tbl_mod$date_week_fmt) %>% 
                  dplyr::select(date_week_fmt, search_volume, adstock, .fitted, .resid),
                by = c("search_volume", "date_week_fmt", "adstock")) %>% 
      dplyr::select(date_week_fmt, observed = search_volume, fitted = .fitted) %>% 
      gather(Type, value, -date_week_fmt) 
    
    return(list(mod = mod, 
                tab_eff = tab_eff, 
                tab_fit = tab_fit))  
  }

# (fun) gg chart fit ----
fun_gg_chart_fit <- 
  function(mod_list){
    # chart fit
    gg.1 <- mod_list$tab_fit %>% 
      ggplot(aes(x = date_week_fmt, 
                 y = value, 
                 color = Type)) + 
      geom_line() + 
      scale_color_manual(values = c('salmon', 'gray70')) + 
      ylab('Search Volume') + 
      xlab('Date (Week)') + 
      ggtitle("Fitted vs Observed Search Volume", 
              paste("Model:", as.character(mod_list$mod$call)[2]) )
    
    return(gg.1)
  }

# (fun) function table efficiencies ----
fun_tab_camp_eff <- 
  function(mod_list){
    # chart fit
    td <- 
      tibble(term = paste0("media_campaign", 
                        mod_list$mod$xlevels$media_campaign), 
             beta_0 = tidy(mod_list$mod) %>% 
               filter(term == '(Intercept)') %>% 
               .$estimate) %>% 
      left_join(tidy(mod_list$mod), 
                by = "term") %>% 
      mutate(efficiency = beta_0 + tidyr::replace_na(estimate, 0))
    
    tab_td <- 
      td %>% 
      mutate(term = str_replace_all(term, 'media_', '')) %>% 
      dplyr::select(term, efficiency) %>% 
      mutate_if(is.numeric, round, digits = 1) %>% 
      arrange(desc(efficiency))
    
    return(tab_td)
  }

# model ----
mod_list_1 <- fun_model_lm(tbl_adstock)
mod_list_1$mod %>% glance
mod_list_1$tab_eff

# chart fit ----
fun_gg_chart_fit(mod_list_1)

# campaign efficiencies ----
fun_tab_camp_eff(mod_list_1)





# model 2 ----

# (fun) model 2 function ----
fun_model_bayes <- 
  function(tbl_mod){
    # Input: 
    # - tbl_mod: data
    # Output: 
    # - list of elements: 
    #   * mod: model
    #   * tab_eff: model coeficients and intervals
    #   * tab_fit: model fit 
    mod <- arm::bayesglm(formula = "search_volume ~ adstock + media_campaign ", 
                    prior.mean = c(0, rep(30, 2)),
                    prior.scale = c(.0005),
                    data = tbl_mod)
    mod %>% summary()
    
    tab_eff <- mod %>% tidy()
    
    tab_fit <- tbl_mod %>% 
      left_join(mod %>% 
                  augment %>% 
                  mutate(date_week_fmt = tbl_mod$date_week_fmt) %>% 
                  dplyr::select(date_week_fmt, search_volume, adstock, .fitted, .resid),
                by = c("search_volume", "date_week_fmt", "adstock")) %>% 
      dplyr::select(date_week_fmt, observed = search_volume, fitted = .fitted) %>% 
      gather(Type, value, -date_week_fmt) 
    
    return(list(mod = mod, 
                tab_eff = tab_eff, 
                tab_fit = tab_fit))  
    }


# model ----
mod_list2 <- fun_model_bayes(tbl_adstock)
mod_list2$mod %>% glance
mod_list2$tab_eff

# simulations
sim(mod_list2$mod, 500)@coef %>% 
  as.tibble() %>% 
  mutate(campaign1 = `(Intercept)`, 
         campaign2 = `(Intercept)` + media_campaign2, 
         campaign3 = `(Intercept)` + media_campaign3) %>% 
  dplyr::select(starts_with('campaign')) %>% 
  rownames_to_column() %>% 
  gather(campaign, value, -rowname) %>% 
  group_by(campaign) %>% 
  summarise(median = median(value), 
            q25 = quantile(value, .25),
            q75 = quantile(value, .75),
            q05 = quantile(value, .05),
            q975 = quantile(value, .975)) %>% 
  ggplot(aes(x = median, y = campaign)) + 
  geom_errorbarh(aes(xmin = q25, xmax = q75), height = 0, size = 2) +
  geom_errorbarh(aes(xmin = q05, xmax = q975), height = .1) +
  geom_point(aes(color = campaign), size = 4) + 
  theme(legend.position = 'none') + 
  xlab('Estimate') + 
  ylab(NULL)
  

# chart fit ----
fun_gg_chart_fit(mod_list_2)

# campaign efficiencies ----
fun_tab_camp_eff(mod_list_2)
