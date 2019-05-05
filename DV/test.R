#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(ggplot2)
library(googleVis)

gdp_hdi <- read.csv('gdp_hdi.csv')
gdp_hdi <- gdp_hdi[, -1]
gdp_hdi$Country <- as.character(gdp_hdi$Country)
gdp_hdi$Region <- as.character(gdp_hdi$Region)

# User interface ----
ui <- fluidPage(
  titlePanel("Hdi_life"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from hdi_life data set."),
      
      sliderInput("range", 
                  label = "Range of time:",
                  min = 1990, max = 2017, value = 1990)
    ),
    
    mainPanel(htmlOutput("plot"))
  )
)

# Server logic ----
server <- function(input, output) {
  #datasetInput <- reactive({hdi_life[which(hdi_life$year %in% input$range), ]})
  #output$plot <- renderPlot({
  #ggplot(filter(hdi_life, year == input$range), 
  #aes(x = filter(hdi_life, year == 2009)$HDI, y = filter(hdi_life, year == 2009)$Life, color = Region)) +
  #geom_point() + xlim(0, 1) + ylim(0, 1)
  output$plot <- renderGvis({
    gvisMotionChart(gdp_hdi, idvar = 'Country', timevar= 'Year')
  })
}

# Run app ----
shinyApp(ui, server)