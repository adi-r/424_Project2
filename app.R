#setwd("C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project2")
#setwd("C:/Users/Krishnan CS/424_Project2")
#print(getwd())

# LIBRARIES=======================================================================================================
library(lubridate)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tidyr)
library(scales)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(stringr)
library(shinyjs)

options(scipen=999)


# READ DATA=======================================================================================================
df <- do.call(rbind, lapply(list.files(pattern = "*.csv"), read.csv))
df <- df[, -1]
# Set date column in Date format
df$date <- as.Date(df$date, "%Y-%m-%d")

#Extracting lat and long as a separate column for leaflet map
df$lat <- as.numeric(str_extract(df$Location, "\\d+.\\d+"))
df$long <- as.numeric(str_extract(df$Location, "-\\d+.\\d+"))

#Storing line color as a separate column
df$line_color <-  str_extract(df$line, "\\w+")
stations <- unique(df$stationname)

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
                      #using shinyjs to disable/enable inputs
                      useShinyjs(),
                      tags$head(tags$style(".sidebar-menu li { margin-bottom: 20px; }")),
                      tabItems(
                        tabItem(tabName = "map_dash", 
                                sidebarLayout(position = "left",
                                              sidebarPanel(style = "margin-top: 70%",
                                                           width = 3,
                                                           fluidPage(
                                                             #Radio buttons
                                                             div(fluidRow(column(8,
                                                                             radioButtons("radio_single", "Select mode",
                                                                                          c("Single Date" = "single",
                                                                                            "Comparison" = "compare"),
                                                                                          inline = FALSE))),
                                                             #date picker for single date
                                                             fluidRow(
                                                               dateInput("date", label="Single Dates", value = "2021-08-23",
                                                                       min="2001-01-01", max="2021-11-30", format = "yyyy/mm/dd")
                                                             )),
                                                             #date range for comparison
                                                             fluidRow(dateRangeInput("date1", label="Compare Dates", min = NULL,
                                                                            max = NULL, format = "yyyy/mm/dd",
                                                                            separator = "and")),
                                                             fluidRow(
                                                               column(6,
                                                                      actionButton(inputId = "prevButton", label = "Prev")),
                                                               column(6,
                                                                      actionButton(inputId = "nextButton", label = "Next"))
                                                             ),
                                                             div(selectizeInput('select_station', "Station", choices = stations,
                                                                               selected = "UIC-Halsted", multiple = FALSE,
                                                                              options = NULL)
                                                             
                                                             ),
                                                             HTML("<br>"),
                                                             div(
                                                               fluidRow(
                                                                 selectInput("sortby", "Bar Plot View", choices = c("Alphabetical" = "alpha", "Ascending" = 'asc', "Descending" = "desc")),
                                                               )
                                                             )
                                                             )
                                                           
                                              ),
                                              mainPanel(
                                                fluidPage(
                                                  splitLayout(cellWidths = c("50%", "50%"), leafletOutput("map_dash"), uiOutput("plot_and_table")),
                                                  #Leaflet Map UI
                                                  # column(width = 12,
                                                  #        leafletOutput("map_dash"))
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
server <- function(input, output, session){
  updateSelectizeInput(session, 'select_station', choices = stations, server = TRUE)
  
 # MAP=========================================================================================================
  # Previous day button
   observeEvent(input$prevButton,{
       date <- singleDateReactive() - days(1)
       #decrementing the date input and updating it
       updateDateInput(session, "date", value = date)
  })
  
  #Next day Button
  observeEvent(input$nextButton,{
    date <- singleDateReactive() + days(1)
    #incrementing the date input and updating it
    updateDateInput(session, "date", value = date)
  })
  
  #Toggle single and compare date pickers
  observeEvent(input$radio_single,{
    if(input$radio_single == 'single'){
      shinyjs::enable("date")
      shinyjs::disable("date1")
    }
    else
    {
      shinyjs::enable("date1")
      shinyjs::disable("date")
    }
  })

  
  
  #Single Date reactive
  singleDateReactive <- reactive({ 
    if(input$radio_single == "single"){
      singleDate <- input$date
      return(singleDate)
    }
  
    })
  
  #Extracting subset of datframe for a single date
  dataframeReactive <- reactive({ 
    singleDate <- singleDateReactive()
    date_df <- subset(df, df$date == singleDate)})

  
  #rendering map
  output$map_dash <- renderLeaflet({
    df <- dataframeReactive()
    map <- leaflet(options= leafletOptions()) %>%
      addTiles() %>% 
      addCircleMarkers(data = df, lat = ~lat, lng = ~long, 
                       #Taking the log and scaling the radius
                       radius = ~log(rides+10)*1.25,
                       color = ~line_color,
                       popup = paste("<center><strong>" ,df$stationname, "</strong>", "<br>",
                                     df$line, "<br>",
                                     "Rides: ", df$rides, "<br> </center>")
                # Tried a custom icon            
                #,icon = list(
                #iconUrl = 'https://icons.iconarchive.com/icons/icons8/ios7/32/Transport-Train-icon.png',
                #iconSize = c(25, 25))
                ) %>%
      setView( lat = 41.8781, lng = -87.6298, zoom = 12) %>% 
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
  
 # BARPLOT=====================================================================================================
  
  # Dataframe for BAR TABLE
  bar_df <- function(start_date, sort_condn, end_date=NULL){
    date_frame <- df[df$date == start_date,][c("stationname", "rides")]
    
    if(sort_condn == 'alpha'){
      date_frame <- date_frame[order(date_frame$stationname),]
    }
    
    else if(sort_condn == 'asc'){
      date_frame <- date_frame[order(date_frame$rides),]
    }
    
    else{
      date_frame <- date_frame[order(-date_frame$rides),]  
    }
    
    return(date_frame)
  }
  
  # Pass dataframe to table layout function
  bar_table <- function(){
    table_frame <- bar_df(input$date, input$sortby)
    table_frame <- table_frame %>%
      rename(Station = stationname, Rides = rides)
    return(table_frame)
  }
  
  # reactive table layout
  output$bar_table <- renderUI({
    div(
      tags$style(
        HTML('.datatables {width: inherit !important;}')
      ),
      datatable(
        bar_table(),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          pageLength = 7,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      ))
    })
  
  #  reactive plotly function
  plot_1 <- reactive({
    data <- df[df$date == input$date,]
    p = plot_ly(data, y = data$rides, x = data$stationname, type = "bar", text=data$line) %>%
      layout(title = 'Ridership Data')
    return(p)
  })
  
  # Render Bar Plot and Table
  output$plot_and_table <- renderUI({
    fluidPage(
      fluidRow(column(8, div(renderPlotly({plot_1()})))),
      fluidRow(column(8, uiOutput("bar_table")))
      )
  })
}
shinyApp(ui = ui, server = server)