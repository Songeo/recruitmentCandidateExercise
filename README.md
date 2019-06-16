# Recruitment Candidate Exercise

**Exercise for Solutions Analytics Director role**


Link to (app)[https://songeo.shinyapps.io/campaign_impact_app/]


----

This exercise aims to gain an understanding of how a brand’s advertising spend has influenced the levels of weekly Google Search volumes that were made for the brand in a particular country.

In this case, the advertising has specifically intended to increase in Search volumes and over time three different (non-overlapping) advertising campaigns have been used.

Data for the weekly Search volumes; the advertising spend; and where the three different campaigns take place are available.

The task is to create two different modelling approaches for the data with Search Volume as the dependent. The aim is use the media spend to gain an understanding of which of the campaigns appear to have more effectively and efficiently generated additional Search volumes.

Because the media/advertising spend will have an impact in the week in which it takes place and a decaying effect in future weeks, it is necessary to represent the media spend in the model in the form of a 'recent advertising pressure' measure. This type of measure with media spend is called an Adstock; and it is in this form that the advertising should be used as an independent variable in the model. The Adstock calculation takes the form:

Adstock (in week n) = Media Spend (in week n) + [ RF x Adstock (in week n-1) ]

The RF is the Retention Factor [0,1] describing the proportion of the media pressure the is carried over from week to week.

Results for the two modelled approaches should be delivered as a Shiny App.

There should be a slider to allow the viewer to alter the value of the RF (in increments of 0.1); and as well as a chart showing the model fit, there should also be a table that reports the efficiencies for the three campaigns.

The models don’t need to be complicated and the Shiny app UI should be simple. Templated Shiny UI is enough. We expect that each model will use its own function with appropriate documentation and that the code will be pushed to your Github repository. We expect to see at least two commits. Along with pushing your code to Github, you should deploy your Shiny app on the Shiny server (using a free account https://www.shinyapps.io/).

The data for the exercise are available here https://github.com/schubertjan/recruitmentCandidateExercise. You are expected to fork the repository into your own Github account and make any code commits in this forked repository.
