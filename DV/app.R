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
library(googleVis)
library(ggthemes)
library(gganimate)
library(ggnetwork)
library(gifski)
library(tidyverse)
library(ggraph)
library(igraph)
library(intergraph)
library(ggrepel)

load("gdp.RData")
load("consumption_share.RData")
load("map_gdp_final.RData")
load("gdp_hdi.RData")
load("gdp_life.RData")
load("consumption_proportion.RData")
export_net_new1 <- read.csv("export_net_new1.csv")
load("GDP_construction.RData")
load('IMF_rate.RData')
imf_brazil <- read.csv("imf_brazil.csv")
imf_china <- read.csv("imf_china.csv")
imf_india <- read.csv("imf_india.csv")
imf_russia <- read.csv("imf_russia.csv")

region <- unique(gdp$region)
segment <- unique(con_sh$Consumption.Segment)
c_area <- unique(con_sh$Area)

# Define UI for application that draws a histogram
ui <- navbarPage("Group M",
   tabPanel("Why GDP",
            sidebarLayout(
              position = "right",
              sidebarPanel(
                radioButtons("indicator",
                             "Relationship between GDP and:",
                             choices = list("Human Development Index",
                                            "Life Expectancy"),
                             selected = "Human Development Index")),
              mainPanel(htmlOutput("gdp_indicator"))
              )
            ),
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
            sidebarLayout(
              position = "left",
              sidebarPanel(
                radioButtons("component_type", 
                             "Type of Plot:",
                             choices = list("Component/GDP %",
                                            "Contribution Rate of GDP Components"),
                             selected = "Component/GDP %"
                ),
                width = 2
              ),
              mainPanel(
                plotlyOutput("components", height = 600, width = "100%")
              )
            )),
   tabPanel("Consumption",
            titlePanel("Comparison among BRICs"),
            sidebarLayout(
              position = "left",
              sidebarPanel(
                radioButtons("chart_type", 
                             "Type of Chart:",
                             choices = list("Household vs Government",
                                            "Household Consumption by industry"),
                             selected = "Household vs Government"
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
                plotlyOutput("Pie_chart", height = 600, width = "110%")
#                verbatimTextOutput("event")
              )
            )),
   tabPanel("Export",
            tabsetPanel(
              tabPanel("Net Export of BRICs",
                       imageOutput("gganim")),
              tabPanel("Trade Network among BRICs",
                       plotlyOutput("network_4", height = "auto", width = "auto")),
              tabPanel("Ego Network of BRICs",
                plotOutput("ego_brazil", height = 600, width = "80%"),
                       plotOutput("ego_china", height = 600, width = "80%"),
                       plotOutput("ego_india", height = 600, width = "80%"),
                       plotOutput("ego_russia", height = 600, width = "80%"))
                       
            ))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
#   input_year <- reactive(input$year)
  output$gdp_indicator <- renderGvis({
    if (identical(input$indicator, "Human Development Index")) {
      gvisMotionChart(gdp_hdi, 
                      idvar="Country", 
                      timevar="Year")
      
    } else{
      gvisMotionChart(gdp_life, 
                      idvar="Country", 
                      timevar="Year")
    }
    
  })
  
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
   
   output$components <- renderPlot({
     if (identical(input$component_type, "Component/GDP %")) {
       constplot <- ggplot() +
       xlab("Year") + ylab("Component/GDP") + ggtitle("GDP Components") 
     
     constplot + 
       geom_bar(data = d, aes(x = year, y = amount, 
                              fill = factor(type, level = c("consumption", "investment", "export"))), 
                stat="identity", position=position_stack(0),width=1) + theme_classic()+
       theme(legend.title=element_blank(), strip.text.x = element_text(size = 14, colour = "black", face = "bold"))+
       facet_wrap(region~.) +
       scale_fill_manual(values = c("steelblue4", "Skyblue3", "skyblue1"))
       
     } else{
       Country <- c('China, P.R.: Mainland', 'India', 'Brazil', 'Russian Federation')
     imf_rate <- filter(IMF_clean_rate, region == Country[1] | region == Country[2] | region == Country[3] | region == Country[4], year >= 1980)
     ggplot(imf_rate, aes(x = year, y = contribution_rate, color = type)) + geom_line(size = 1) + facet_wrap(region ~ .)}
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
       ggplot() + geom_line(data = consumption_proportion, 
                            aes(x=year, y=proportion, color = region), size = 1) + 
         geom_point(data = consumption_proportion, 
                    aes(x=year, y=proportion, color = region), size=1, shape=20) +
         theme_classic() + theme(legend.position = "left") +
         scale_color_manual(values = c("olivedrab", "firebrick", "darkgoldenrod", "steelblue")) +
         labs(x = "Year", y = "Household Consumption/GDP %")
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
             range = c(-90, 90),
             showgrid = TRUE,
             tickmode = "linear",
             dtick = 10
           ),
           lonaxis = list(
             range = c(-180, 180),
             showgrid = TRUE,
             tickmode = "linear",
             dtick = 20
           )
         )
       )
     p
   })
   
   output$gganim <- renderImage({
     subset <- export_net_new1 %>%
       filter(shape == "Target")
     outfile <- tempfile(fileext='.gif')
     
     p1 <- ggplot(export_net_new1, aes(x = Country, y = Net, size = Export)) + 
       geom_point(aes(shape = shape, color = Region)) + geom_hline(aes(yintercept = 0)) + 
       transition_manual(Year) + geom_text(data = subset, aes(label = Country)) + 
       theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom")
     anim_save("outfile.gif", animate(p1)) # New
     
     # Return a list containing the filename
     list(src = "outfile.gif",
          contentType = 'image/gif',
           width = 1200,
           height = 800)
#     animate(p1, duration = 15, "p1.gif")
   })
   
   output$ego_brazil <- renderPlot({
     brazil <- graph_from_data_frame(imf_brazil, directed = FALSE)
     V(brazil)$export <- imf_brazil$Export
     dat4 <- ggnetwork(brazil, layout="fruchtermanreingold", 
                       arrow.gap=0, cell.jitter=0)
     dat4_label <- dat4 %>%
       filter(vertex.names != "Brazil")
     gg_brazil <- ggplot() + geom_edges(data=dat4, 
                                        aes(x=x, y=y, xend = xend, yend = yend, size = Export),color = "#0f5600",curvature=0.1,alpha=1/2, type = "closed", show.legend = FALSE) + 
       geom_nodes(data = dat4, aes(x=x, y=y), size = 3) + theme_blank() + geom_label_repel(data = dat4_label, aes(x=x,y=y, label = vertex.names)) + ggtitle("Ego Network of Brazil's Export") + theme(plot.title = element_text(hjust = 0.5))
     gg_brazil
   })
   
   output$ego_china <- renderPlot({
     china <- graph_from_data_frame(imf_china, directed = FALSE)
     V(china)$export <- imf_china$Export
     dat2 <- ggnetwork(china, layout="fruchtermanreingold", 
                       arrow.gap=0, cell.jitter=0)
     dat2_label <- dat2 %>%
       filter(vertex.names != "China")
     gg_china <- ggplot() + geom_edges(data=dat2, 
                                       aes(x=x, y=y, xend = xend, yend = yend, size = Export),color = "#a00808",curvature=0.1,alpha=1/2, type = "closed", show.legend = FALSE) + 
       geom_nodes(data = dat2, aes(x=x, y=y), size = 3) + theme_blank() + geom_label_repel(data = dat2_label, aes(x=x,y=y, label = vertex.names)) + ggtitle("Ego Network of China's Export") + theme(plot.title = element_text(hjust = 0.5))
     gg_china
   })
   
   output$ego_india <- renderPlot({
     india <- graph_from_data_frame(imf_india, directed = FALSE)
     V(india)$export <- imf_india$Export
     dat3 <- ggnetwork(india, layout="fruchtermanreingold", 
                       arrow.gap=0, cell.jitter=0)
     dat3
     dat3$color <- "steelblue"
     color_india <- c("steelblue") 
     dat3_label <- dat3 %>%
       filter(vertex.names != "India")
     gg_india <- ggplot() + geom_edges(data=dat3, 
                                       aes(x=x, y=y, xend = xend, yend = yend, size = Export),color = "#c48e05",curvature=0.1,alpha=1/2, type = "closed", show.legend = FALSE) + 
       geom_nodes(data = dat3, aes(x=x, y=y), size = 3) + theme_blank() + geom_label_repel(data = dat3_label, aes(x=x,y=y, label = vertex.names))+ ggtitle("Ego Network of Inida's Export") + theme(plot.title = element_text(hjust = 0.5))
     gg_india
   })
   
   output$ego_russia <- renderPlot({
     russia <- graph_from_data_frame(imf_russia, directed = FALSE)
     V(russia)$export <- imf_russia$Export
     dat5 <- ggnetwork(russia, layout="fruchtermanreingold", 
                       arrow.gap=0, cell.jitter=0)
     dat5_label <- dat5 %>%
       filter(vertex.names != "Russia")
     gg_russia <- ggplot() + geom_edges(data=dat5, 
                                        aes(x=x, y=y, xend = xend, yend = yend, size = Export),color = "#0893a0",curvature=0.1,alpha=1/2, type = "closed", show.legend = FALSE) + 
       geom_nodes(data = dat5, aes(x=x, y=y), size = 3) + theme_blank() + geom_label_repel(data = dat5_label, aes(x=x,y=y, label = vertex.names)) + ggtitle("Ego Network of Russia's Export") + theme(plot.title = element_text(hjust = 0.5))
     gg_russia
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

