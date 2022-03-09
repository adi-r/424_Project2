#setwd("C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project2")
#setwd("C:/Users/Krishnan CS/424_Project2")
#print(getwd())

# LIBRARIES=======================================================================================================
library(lubridate)
library(DT)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tidyr)
library(scales)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(stringr)

options(scipen=999)


# READ DATA=======================================================================================================
df <- do.call(rbind, lapply(list.files(pattern = "*.csv"), read.csv))
df <- df[, -1]
# Set date column in Date format
df$date <- as.Date(df$date, "%Y-%m-%d")

#Extracting lat and long as a separate column for leaflet map
df$lat <- as.numeric(str_extract(df$Location, "\\d+.\\d+"))
df$long <- as.numeric(str_extract(df$Location, "-\\d+.\\d+"))

# Extracting ride data for Aug 23 2021
Aug_23_ridership$line_color <-  str_extract(Aug_23_ridership$line, "\\w+")

#Extracting line color data for Aug23 df


# UI==============================================================================================================
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "CS424 Project-2"),
                    dashboardSidebar(collapsed = FALSE, disable = FALSE,
                                     sidebarMenu(
                                       id = "menu_tabs",
                                       tags$div(style = "margin-top: 300px;"),
                                       menuItem("Dashboard", tabName = "map_dash", selected = TRUE, icon = icon("dashboard")),
                                       menuItem("About", tabName = "about", icon = icon("sunglasses", lib = "glyphicon"))
                                     )
                    ),
                    dashboardBody(
                      tags$head(tags$style(".sidebar-menu li { margin-bottom: 20px; }")),
                      tabItems(
                        tabItem(tabName = "map_dash", 
                                sidebarLayout(position = "left",
                                              sidebarPanel(style = "margin-top: 80%",
                                                           h4("Left"),
                                                           div(selectInput("station1_compare", "Station",
                                                                           choices = c("All", "UIC-Halsted", "O'Hare Airport", "Racine"),
                                                                           selected = c("UIC-Halsted")
                                                           )
                                                           ),
                                                           
                                                           fluidPage(
                                                             fluidRow(column(8,
                                                                           radioButtons("radio_single", "Dates",
                                                                                        c("Single Date" = "single",
                                                                                          "Date Range" = "range"),
                                                                                        inline = TRUE))),
                                                             fluidRow(column(4, textInput("text", "", value = "Disable me")))
                                                             ),
                                                           
                                                           fluidRow(column(8,
                                                                           div(selectInput("year1_compare", "Year",
                                                                                           choices = c("All", 2021:2001),
                                                                                           selected = c(2021)
                                                                           )
                                                                           )
                                                           )
                                                           ),
                                                           h4("Right"),
                                                           div(selectInput("station2_compare", "Station",
                                                                           choices = c("All", "UIC-Halsted", "O'Hare Airport", "Racine"),
                                                                           selected = c("O'Hare Airport")
                                                           )
                                                           ),
                                                           fluidRow(column(8,
                                                                           div(selectInput("year2_compare", "Year",
                                                                                           choices = c("All", 2021:2001),
                                                                                           selected = c(2021)
                                                                           )
                                                                           )
                                                           )
                                                           ),
                                                           width = 2
                                              ),
                                              mainPanel(
                                                fluidPage(
                                                  #splitLayout(cellWidths = c("75%", "75%"), uiOutput("compare_plots1"), uiOutput("compare_plots2"))),
                                                  #Leaflet Map UI
                                                  column(
                                                    width = 12,
                                                    leafletOutput("map_dash") 
                                                  )
                                                  
                                                  
                                                  )
                                                  
                                                )
                        )
                      ),
                        
                        tabItem(tabName = "about",
                                tags$div(style = "margin-top: 200px;"),
                                h1('About'),
                                h2('Created by Aditya Ranganathan on 02/07/2022'),
                                h3(""),
                                h3(""),
                                h3("The dashboard displays data reagrding CTA rides in a clear and intuitive manner.
                 Users can check ride data of 3 different CTA stations: O'Hare Airport, UIC-Halsted and Racine.
                 The data can be viewed from a yearly, monthly, weekly or daily basis. Users have the option of seeing the data either as plots or in a tabular form"),
                                h3("Users can get an idea about the number of passengers that travel through the 'L' and can also correlate certain major events that occurred in Chicago with respect to the number of riders during that time period."),
                                h3("Data was sourced from from the Chicago Data Portal at", tags$a(href="https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f/data", "this link")),
                                h3("The dataset consists of 1.1 million rows and has attributes 'stationname', 'station_id', 'date', 'rides' and 'daytype'")
                                
                        )
                      )
                    )
)



# SERVER=======================================================================================================
server <- function(input, output){
  output$map_dash <- renderLeaflet({
    map <- leaflet(options= leafletOptions()) %>%
      addTiles() %>% 
      addCircleMarkers(data = Aug_23_ridership, lat = ~lat, lng = ~long, 
                       #Taking the log and scaling the radius
                       radius = ~log(rides+10)*1.25,
                       color = ~line_color,
                       popup = paste("<center><strong>" ,Aug_23_ridership$stationname, "</strong>", "<br>",
                                     Aug_23_ridership$line, "<br>",
                                     "Rides: ", Aug_23_ridership$rides, "<br> </center>"
                                     )
                             
                #,icon = list(
                #iconUrl = 'https://icons.iconarchive.com/icons/icons8/ios7/32/Transport-Train-icon.png',
                #iconSize = c(25, 25))
                ) %>%
      setView( lat = 41.8781, lng = -87.6298, zoom = 13) %>% 
      #Different Backgrounds, have to select 3
      addProviderTiles("Stamen.TonerLite", group = "B/w") %>%
      addProviderTiles("OpenRailwayMap", group = "Railway") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "smooth") %>%
      addProviderTiles("CartoDB.Positron", group = "Minimalist") %>%
      #Resettable map
      addResetMapButton() %>%
      #Choice for background
      addLayersControl(
        baseGroups = c("B/w", "Railway", "smooth", "Minimalist"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    return(map)
  })
}
shinyApp(ui = ui, server = server)