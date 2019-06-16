#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("structure.R")
source("helper.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Reactive data dependent of rf input ----
    
    TblStock <- reactive({
        fct_rf <- (input$rf_slider)
        tbl_adstock <- 
            tbl_data %>% 
            mutate_at('media_campaign', factor) %>% 
            arrange(date_week_fmt) %>% 
            group_by(media_campaign) %>% 
            mutate(adstock = fun_adstock_calc(media_spend_usd, fct_rf)) %>% 
            ungroup() %>% 
            mutate(lag = lag(search_volume)) %>% 
            na.omit
    })
    
    ResMod1 <- reactive({
        mod_list <- fun_model_lm(TblStock())
        mod_list
    })
    
    ResMod2 <- reactive({
        mod_list <- fun_model_bayes(TblStock())
        mod_list
    })
    
    
    
    # App ouput (results and plots) ----
    
    output$tx_summ <- renderText({ 
        tbl_adstock <- TblStock()
        txt <- paste0("Weekly search volume data from ", min(tbl_adstock$date_week_fmt), 
                     " to ", max(tbl_adstock$date_week_fmt), 
                     ", a total of ", n_distinct(tbl_adstock$date_week_fmt), 
                     " weeks. The following plots show search volume, media spend, ",
                     "and adstock transformation of media spend time series.")
        txt
    })
    
    output$gg_eda <- renderPlot({

        # search volume
        gg.1 <- 
            tbl_data %>% 
            ggplot(aes(x = date_week_fmt, 
                       y = search_volume)) + 
            geom_line(color = 'gray80') + 
            geom_point(aes(color = factor(media_campaign))) + 
            ylab('\nSearch Volume') + 
            xlab('Date (Week)') + 
            guides(color = guide_legend("Media\nCampaign")) + 
            ggtitle("Search Volume", 
                    "Searches realized in time by campaign")
        
        # media spend
        gg.2 <- 
            tbl_data %>% 
            ggplot(aes(x = date_week_fmt, 
                       y = media_spend_usd)) + 
            geom_line(color = 'gray80') + 
            geom_point(aes(color = factor(media_campaign))) + 
            ylab('\nMedia Spend (USD)') + 
            xlab('Date (Week)') + 
            guides(color = guide_legend("Media\nCampaign")) + 
            ggtitle("Media Spend", 
                    "Advertising spend by campaign")
        
        
        # adstock spend
        gg.3 <- 
            TblStock() %>% 
            ggplot(aes(x = date_week_fmt, 
                       y = adstock)) + 
            geom_line(aes(color = factor(media_campaign)), size = 1 ) + 
            geom_line(aes(y = media_spend_usd), color = 'gray80' ) +
            ylab('Adstock\nMedia Spend (USD)') + 
            xlab('Date (Week)') + 
            guides(color = guide_legend("Media\nCampaign")) + 
            ggtitle("Media Spend Adstock", 
                    paste0("Retention factor: ", round(100*(input$rf_slider)), '%'))
        
        
        grid.arrange(gg.1, gg.2, gg.3, ncol = 1)
    })
    
   
    # Results approach 1 ----
    output$gg_mod1_fit <- renderPlot({
        mod_list <- ResMod1()
        gg.1 <- fun_gg_chart_fit(mod_list)
        gg.1
    })
    
    output$tab_mod1_camp <- renderDataTable({
        mod_list <- ResMod1()
        
        # campaign efficiencies
        td <- fun_tab_camp_eff(mod_list)
        
        DT::datatable(td, 
                      rownames = FALSE, 
                      options = list(searching = FALSE, paging = FALSE))
    })
    
    # Results approach 2 ----
    output$gg_mod2_fit <- renderPlot({
        mod_list <- ResMod2()
        gg.1 <- fun_gg_chart_fit(mod_list)
        gg.1
    })
    
    output$tab_mod2_camp <- renderDataTable({
        mod_list <- ResMod2()
        
        # campaign efficiencies
        td <- fun_tab_camp_eff(mod_list)
        
        DT::datatable(td, 
                      rownames = FALSE, 
                      options = list(searching = FALSE, paging = FALSE))
    })
    
    output$gg_mod2_sim <- renderPlot({
        mod_list <- ResMod2()
        
        # campaign efficiencies
        gg <- sim(mod_list$mod, 500)@coef %>% 
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
        
        gg
    })
    
    
    
    

})

