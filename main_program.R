setwd("D:/datavis/projet final")
library(tidyverse)
library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
library(ggplot2)
library(tidyr)
library(leafpop)
library(stringr)
library(sp)
library(leaflegend)
library(shinycssloaders)
source("format_data.R")
source("graphs.R")
source("map.R")

# initialize data
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
plastics <- clean_plastics(plastics)
type_plastics <- type_plastics_data_init(plastics)
map_data <- map_data_init()
countries <- map_data[[1]]
countries_coordinates <- map_data[[2]]
countries_iso = countries@data %>% inner_join(countries_coordinates, by =c("ISO_A3" = "Alpha.3.code"))
map<-init_map(plastics,type_plastics,countries,countries_coordinates,countries_iso)
# initialize inputs
countries_input <- c("World") %>% rbind(plastics %>% distinct(country) %>% filter(!country=="Empty"))
top_company_input <- c("None") %>% rbind(plastics  %>%
                                          filter(
                                            parent_company != "null" &
                                              parent_company != "NULL" &
                                              parent_company != "Unbranded" &
                                              parent_company != "Grand Total" & parent_company != "Assorted" & 
                                              parent_company !="Null"
                                          ) %>%
                                          group_by(parent_company) %>%
                                          summarise(n = sum(grand_total, na.rm = TRUE)) %>% arrange(desc(n)) %>%
                                          head(5))
addResourcePath("img", "C:/Users/mefta/OneDrive/Pictures/Screenshots")
# Define UI for application that draws a histogram
shinyApp(
  
  ui <- fluidPage(
    
    tabsetPanel(
      tabPanel("Diapo",tags$img(src="img/diapo1.png", width = "100%", height = "850px"),
               tags$img(src="img/diapo2.png", width = "100%", height = "850px"),
               tags$img(src="img/diapo3.png", width = "100%", height = "850px"),
               tags$img(src="img/diapo4.png", width = "100%", height = "850px"),
               tags$img(src="img/diapo5.png", width = "100%", height = "850px"),
               
               tags$img(src="img/diapo7.png", width = "100%", height = "850px")),
      tabPanel("Les graphs", 
               withSpinner(plotOutput("graph0", width = "100%", height = "850px"  )),
               tags$img(src="img/diapo6.png", width = "100%", height = "850px"),
               withSpinner(plotOutput("graph1", width = "100%", height = "850px")),
               titlePanel("Les compagnies les plus polluantes en 2019 et 2020"),
               inputPanel(selectInput("pays", label = "Pays", choices = countries_input, selected = "World" )), 
               withSpinner(plotOutput("graph2", width = "100%", height = "800px"))
               ),
      tabPanel("Carte", titlePanel("Cartographie des dechets plastiques collectés par break free from plastic en 2019 et 2020"),
               radioButtons("company", "Compagnies:",
                            top_company_input$parent_company, inline=T),
               withSpinner(leafletOutput("map", width = "100%", height = 700)),
               tags$img(src="img/diapofin.png", width = "100%", height = "850px")
               )
    )
  ),
  server <- function(input, output) {
    output$graph0 <- renderPlot(top_plastics_by_countries(plastics))   
    output$graph1 <- renderPlot(top_plastics_by_volenteers(plastics))  
    output$graph2 <- renderPlot(f_company_pays_year(plastics,input$pays)) 
    output$map <- renderLeaflet({
      if(input$company=="None")
      {
        map
      }
      else  
      {
        map_markers <- map %>% clearMarkers()
        addMarkersMap(map_markers,plastics,countries_iso, input$company)
      }
      
    })
  })
