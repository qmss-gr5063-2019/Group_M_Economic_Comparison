#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

load("gdp.RData")
load("consumption_share.RData")
load("map_gdp_final.RData")
region <- unique(gdp$region)
segment <- unique(con_sh$Consumption.Segment)
c_area <- unique(con_sh$Area)

# Define UI for application that draws a histogram
ui <- navbarPage("Group M",
   tabPanel("Why GDP"),
   tabPanel("Global Situation",
     titlePanel("GDP by Country"),
     sidebarLayout(
       position = "right",
       sidebarPanel(
         sliderInput("year",
                     "Year:",
                     min = 1980,
                     max = 2017,
                     value = 38),
         selectInput("Country", 
                     label = "Country",
                     choices = region,
                     selected = region[2]),
         plotOutput("GDP_Rank", height = 250),
         plotOutput("GDP_growth_rate", height = 250),
         width = 3
         ),
     mainPanel(
       plotOutput("GDP_map", height = 750, width = "110%")
#        tabsetPanel(
#          tabPanel("plot", plotOutput("distPlot")),
#          tabPanel("map")
        )
         
      )),

   tabPanel("GDP Components",
            titlePanel("Comparison among BRICs"),
            actionButton("proportion", "%GDP"),
            actionButton("rate", "GDP Contribution Rate")),
   tabPanel("Consumption",
            titlePanel("Comparison among BRICs"),
            sidebarLayout(
              position = "right",
              sidebarPanel(
                radioButtons("chart_type", 
                             "Type of Chart:",
                             choices = list("Private vs Public",
                                            "Household Consumption by industry"),
                             selected = "Private vs Public"
                             ),
                selectInput("segment", 
                            label = "Household Consumption Segment:",
                            choices = segment,
                            selected = segment[3]),
                selectInput("c_area", 
                            label = "Household Consumption by Area:",
                            choices = c_area,
                            selected = c_area[1]),
                width = 2
              ),
              mainPanel(
                plotlyOutput("Pie_chart")
#                verbatimTextOutput("event")
              )
            )),
   tabPanel("Export",
            tabsetPanel(
              tabPanel("Scatter Plot"),
              tabPanel("Trade between BRICs",
                       plotlyOutput("network_4", height = "auto", width = "auto")),
              tabPanel("Ego Network")
            ))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
#   input_year <- reactive(input$year)
   output$GDP_Rank <- renderPlot({
     gdp <- gdp %>% filter(year <= input$year)
     ggplot(gdp[gdp$region == input$Country, ], aes(x = year, y = rank_rev)) + 
       geom_line(color = "skyblue4", size = 1.5) + theme_classic() + 
       geom_label(aes(label = rank), color = "darkgoldenrod3", vjust = 0, size = 2, 
                  fontface = "bold", lineheight = 2, check_overlap = TRUE) + 
       labs(x = "Year", y = NULL) + theme(axis.text.y = element_blank(), axis.ticks.x = element_line()) + 
       ggtitle("GDP Rank")
   })
   
   output$GDP_map <- renderPlot({
     mapgdp_randomyear <- map_gdp_final %>% filter(year == input$year)
     mapgdp_randomyear$GDP_level <- cut(mapgdp_randomyear$GDP,
                                        breaks=c(min(mapgdp_randomyear$GDP, na.rm = T),1e10,1e11,1e12, 1e13, max(mapgdp_randomyear$GDP, na.rm = T)), 
                                        labels=c("<=1e10","1e10-1e11","1e11-1e12","1e12-1e13",">=1e13"),include.lowest=TRUE,order=TRUE)
     worldmap <- map_data('world')
     ggplot() + geom_map(data = worldmap, map = worldmap, aes(x = long, y = lat, map_id=region), fill="white", colour="#7f7f7f", size=0.5) +
       geom_map(data = mapgdp_randomyear, map = worldmap, aes(fill = GDP_level, map_id = region)) + scale_fill_brewer() + theme_minimal() +
       theme(legend.position = "top", legend.margin = margin(4,6,4,6)) + labs(x = "Longitude", y = "Latitude")
   })
   
   output$Pie_chart <- renderPlotly({
     if(identical(input$chart_type, "Household Consumption by industry")){
       con_sha <- con_sh %>%
         filter(Consumption.Segment == input$segment) %>%
         filter(Area == input$c_area)
     
       p <- plot_ly() %>%
         add_pie(data = filter(con_sha, Country == "India"), labels = ~Sector, values = ~data, name = "India", domain = list(x = c(0, 0.4), y = c(0, 0.4)), marker = list(color = "cyan4")) %>%
         add_pie(data = filter(con_sha, Country == "China"), labels = ~Sector, values = ~data, name = "China", domain = list(x = c(0, 0.4), y = c(0.6, 1))) %>%
         add_pie(data = filter(con_sha, Country == "Russian Federation"), labels = ~Sector, values = ~data, name = "Russian Federation", domain = list(x = c(0.6, 1), y = c(0, 0.4))) %>%
         add_pie(data = filter(con_sha, Country == "Brazil"), labels = ~Sector, values = ~data, name = "Brazil", domain = list(x = c(0.6, 1), y = c(0.6, 1))) %>%
         layout(title = "World Bank Data by 2010", showlegend = T,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
       p
     } else{
       
     }
     
   })
   output$GDP_growth_rate <- renderPlot({
     gdp <- gdp %>% filter(year <= input$year)
     ggplot(gdp[gdp$region == input$Country, ], aes(x = year, y = GDP_growth)) + 
       geom_line(color = "skyblue4", size = 1.5) + theme_classic() + 
       geom_label(aes(label = paste(round(GDP_growth,2), "%", sep = "")), 
                  color = "darkgoldenrod3", vjust = 0, size = 2, fontface = "bold", 
                  lineheight = 2, na.rm = TRUE, check_overlap = TRUE, nudge_y = 0.3) + 
       labs(x = "Year", y = NULL) + theme(axis.text.y = element_blank(), axis.ticks.x = element_line()) + 
       ggtitle("GDP Growth Rate")
   })
   
   output$network_4 <- renderPlotly({
     p <- plot_geo(lat = c(-20, 60), lon = c(-50, 120)) %>%
       add_markers(
         lat = 39.9042, lon = 116.4074, text = "China", size = 20, hoverinfo = "text", color = I("red")) %>%  
       add_markers(
         lat = -15.8267, lon = -47.9218, text = "Brazil", size = 20, hoverinfo = "text", color = I("seagreen4")) %>%
       add_markers(
         lat = 28.6139, lon = 77.2090, text = "India", size = 20, hoverinfo = "text", color = I("orange4")) %>%
       add_markers(
         lat = 55.7558, lon = 37.6173, text = "Russia", size = 20, hoverinfo = "text", color = I("skyblue4")) %>%
       add_lines(lat = c(39.9042, -15.8267+2), lon = c(116.4074, -47.9218+2), color = I("red"), size = I(4.243524), text = "China -- Brazil") %>%
       add_lines(lat = c(39.9042, 28.6139+2), lon = c(116.4074, 77.2090+2), color = I("red"), size = I(5.067372), text = "China -- India") %>%
       add_lines(lat = c(39.9042, 55.7558+2), lon = c(116.4074, 37.6173+2), color = I("red"), size = I(4.956), text = "China -- Russia") %>%
       add_lines(lat = c(-15.8267, 39.9042+2), lon = c(-47.9218, 116.4074+2), color = I("seagreen4"), size = I(4.880709),text = "Brazil -- China") %>%
       add_lines(lat = c(-15.8267, 28.6139+2), lon = c(-47.9218, 77.2090+2), color = I("seagreen4"), size = I(2.085795),text = "Brazil -- India") %>%
       add_lines(lat = c(-15.8267, 55.7558+2), lon = c(-47.9218,37.6173+2), color = I("seagreen4"), size = I(1.227226),text = "Brazil -- Russia") %>%
       add_lines(lat = c(55.7558, -15.8267+2), lon = c(37.6173,-47.9218+2), color = I("skyblue4"), size = I(1.671890),text = "Russia -- Brazil") %>%
       add_lines(lat = c(55.7558, 39.9042+2), lon = c(37.6173,116.4074+2), color = I("skyblue4"), size = I(1.671890),text = "Russia -- China") %>%
       add_lines(lat = c(55.7558, 28.6139+2), lon = c(37.6173,77.2090+2), color = I("skyblue4"), size = I(1.671890),text = "Russia -- India") %>%
       add_lines(lat = c(28.6139, 39.9042+2), lon = c(77.2090,116.4074+2), color = I("orange4"), size = I(1.671890),text = "India -- China") %>%
       add_lines(lat = c(28.6139, -15.8267+2), lon = c(77.2090,-47.9218+2), color = I("orange4"), size = I(1.671890),text = "India -- Brazil") %>%
       add_lines(lat = c(28.6139, 55.7558+2), lon = c(77.2090,37.6173+2), color = I("orange4"), size = I(1.671890),text = "India -- Russia") %>%
       layout(
         title = 'Trade between BRICs',
         showlegend = FALSE,
         geo = list(
           resolution = 50,
           showland = TRUE,
           showlakes = TRUE,
           showcountries = TRUE,
           landcolor = toRGB("lightskyblue1"),
           #projection = list(type = "equirectangular"),
           coastlinewidth = 2,
           lataxis = list(
             range = c(-25, 90),
             showgrid = TRUE,
             tickmode = "linear",
             dtick = 10
           ),
           lonaxis = list(
             range = c(-60, 130),
             showgrid = TRUE,
             tickmode = "linear",
             dtick = 20
           )
         )
       )
     p
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

