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
        h4('Retention factor'),
        helpText("Select a retention factor for the adstock function"),
        sliderInput("rf_slider", "Retention Factor:", 
                    min = 0, max = 1, value = .8, step = .1)
        # actionButton("action", "Run Model", class = "btn-primary")
        ), # sidebar Panel
    
    # Main panel with tabset
    mainPanel(
        tabsetPanel(
            # EDA
            tabPanel("EDA", 
                     h3("Exploratory analysis"),
                     "Quick plots of observed data and adstock transformation", 
                     br(),
                     br(),
                     plotOutput('gg_eda', width = "650px", height = "650px")),
            
            # Modelo A
            tabPanel("Approach A", 
                     h3('Frequentist Approach'),
                     br(),
                     helpText(paste0('$$Search\\;Volume_{t} = \\beta_{0} + ',
                                     '\\beta_1 * Adstock_{t} + ',
                                     '\\sum_{i=1}^3 \\beta_{2,i}*campaign_i$$')),
                     wellPanel(h4('Fitted values'),
                               plotOutput('gg_mod1_fit', width = "700px", height = "300px")),
                     br(),
                     wellPanel(h4('Campaign efficiencies'),
                               dataTableOutput('tab_mod1_camp', width = "400px")),
                     br()
                     ),
            
            # Modelo B
            tabPanel("Approach B", 
                     h3('Bayesian Autoregressive Approach'),
                     br(),
                     helpText(paste0('$$Search\\;Volume_{t} = \\beta_0 + ',
                                     'Search\\;Volume_{t-1} + \\beta_1 * Adstock_{t} + ',
                                     '\\sum_{i=1}^3 \\beta_{2,i }*campaign_i$$')),
                     wellPanel(h4('Fitted values'),
                               plotOutput('gg_mod2_fit', width = "700px", height = "300px")),
                     br(),
                     wellPanel(h4('Campaign efficiencies'),
                               dataTableOutput('tab_mod2_camp', width = "400px")),
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
