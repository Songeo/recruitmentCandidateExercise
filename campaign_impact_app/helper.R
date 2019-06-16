
# Functions 


# (fun) adstock function ----
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



# (fun) model 1 function ----
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
                  dplyr::select(date_week_fmt, search_volume, .fitted, .resid),
                by = c("search_volume", "date_week_fmt")) %>% 
      dplyr::select(date_week_fmt, observed = search_volume, fitted = .fitted) %>% 
      gather(Type, value, -date_week_fmt) 
    
    return(list(mod = mod, 
                tab_eff = tab_eff, 
                tab_fit = tab_fit))  
  }

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
    mod <- bayesglm(formula = "search_volume ~ lag + adstock + media_campaign ",
                    family = 'gaussian',
                    data = tbl_mod)
    mod %>% glance
    mod %>% summary()
    
    tab_eff <- mod %>% tidy()
    
    tab_fit <- tbl_mod %>% 
      left_join(mod %>% 
                  augment %>% 
                  mutate(date_week_fmt = tbl_mod$date_week_fmt) %>% 
                  dplyr::select(date_week_fmt, search_volume, .fitted, .resid),
                by = c("search_volume", "date_week_fmt")) %>% 
      dplyr::select(date_week_fmt, observed = search_volume, fitted = .fitted) %>% 
      gather(Type, value, -date_week_fmt) 
    
    return(list(mod = mod, 
                tab_eff = tab_eff, 
                tab_fit = tab_fit))  
  }


# (fun) gg chart fit ----
fun_gg_chart_fit <- 
  function(mod_list){
    # Input: 
    # - mod_list: list of elements
    # Output: 
    # - gg.1: plot of fitted values vs observed values
    
    
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
    
    # Input: 
    # - mod_list: list of elements
    # Output: 
    # - tab_td: table of campaign effects to print
    
    # table of efficiencies
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