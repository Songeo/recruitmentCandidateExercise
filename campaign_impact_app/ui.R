#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(rmarkdown)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    withMathJax(),
    
    # Application title
    titlePanel("Recruitment Candidate Exercise"),
    h4("Weekly Google search volumes influenced by brand’s advertising spend"),
    br(),
    br(),
    
    # Sidebar
    sidebarPanel(width = 4,
        h4('Retention factor slider'),
        helpText("Select a retention factor, from a range from 0 to 1, for the ad-stock function."),
        sliderInput("rf_slider", "Retention factor:", 
                    min = 0, max = 1, value = .8, step = .1)
        ), # sidebar Panel
    
    # Main panel with tabset
    mainPanel(
        tabsetPanel(
            # EDA
            tabPanel("EDA", 
                     h3("Exploratory analysis"),
                     br(),
                     textOutput("tx_summ"),
                     br(),
                     plotOutput('gg_eda', width = "650px", height = "650px")),
            
            # Modelo A
            tabPanel("Approach A", 
                     h3('Frequentist Approach'),
                     br(),
                     helpText(paste0('Linear model using function lm( ). Model ',
                                     '$$Search\\;Volume_{t} = \\beta_{0} + ',
                                     '\\beta_1 \\cdot Adstock_{t} + ',
                                     '\\sum_{i=1}^3 \\beta_{2,i} \\cdot campaign_i$$')),
                     wellPanel(h4('Fitted values'),
                               "The following figure presents the observed values (black) and fitted values (red).",
                               plotOutput('gg_mod1_fit', width = "700px", height = "300px")),
                     br(),
                     wellPanel(h4('Campaign efficiencies'),
                               "The table presents presents the efficiency or impact in search volume of each campaign in descending order.",
                               dataTableOutput('tab_mod1_camp', width = "400px")),
                     br()
                     ),
            
            # Modelo B
            tabPanel("Approach B", 
                     h3('Bayesian Approach'),
                     br(),
                     helpText(paste0('Linear model using function bayesglm( ). Model ',
                                     '$$Search\\;Volume_{t} = \\beta_0 + ',
                                     '\\beta_1 \\cdot Adstock_{t} + ',
                                     '\\sum_{i=1}^3 \\beta_{2,i } \\cdot campaign_i$$')),
                     wellPanel(h4('Fitted values'),
                               "The following figure presents the observed values (black) and fitted values (red).",
                               plotOutput('gg_mod2_fit', width = "700px", height = "300px")),
                     br(),
                     wellPanel(h4('Campaign efficiencies'),
                               "The table presents the efficiency or impact in search volume of each campaign in descending order.",
                               dataTableOutput('tab_mod2_camp', width = "400px")),
                     br(),
                     wellPanel(h4('Probability interval efficiencies'),
                               "The bayesian approach allows the creation of coefficient simulations from the posterior distribution. The following figure presents the impact in search volume of each campaign in descending order with intervals of 90% and 50% of probability.",
                               plotOutput('gg_mod2_sim', width = "400px", height = "200px")),
                     br()
                     ),
            
            # Referencias
            tabPanel("Documentation",
                     icon =  icon("info"),
                     includeMarkdown("documentation.Rmd"))
            
        ) # tabsetPanel
    ), # mainPanel
    
    
    hr(),
    HTML('<p style="text-align:center;color:gray">
       <b>Author: S. Mendizábal</b>
       <br>
       <b>Date: 2019-06-15</b>
       </p>')
    
) # fluid page
) # shinyui
