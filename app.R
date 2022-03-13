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

#setting initial leaflet map  
map <- leaflet(options= leafletOptions(preferCanvas = T)) %>%
  addTiles() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", group = "Default") %>%
  addProviderTiles("OpenRailwayMap", group = "CTA Lines") %>%
  addProviderTiles("CartoDB.Positron", group = "Minimal") %>%
  #Resettable map
  addResetMapButton() %>%
  #Choice for background
  addLayersControl(
    baseGroups = c("Default", "CTA Lines", "Minimal"),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomright"
  )

#Credits for below code snippet: https://stackoverflow.com/questions/70288989/programatically-trigger-marker-mouse-click-event-in-r-leaflet-for-shiny

# create js function that triggers a click on a marker selected by station name
jsCode <- 'shinyjs.markerClick = function(id) {
              map.eachLayer(function (layer) {
                if (layer.options.layerId == id) {
                  layer.fire("click");
                }
              })
           };'


# UI==============================================================================================================
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsCode, functions = c('markerClick')),
  titlePanel("CS424 Project-2"),
  fluidRow(
    #height = "100%",
    #column-1 for about and controls 
    column( width = 2,
      wellPanel(
        #About goes here
        
        style = "height:85vh; margin-top: 80%", 
        fluidRow(
          radioButtons("radio_single", "Select mode",
                       c("Single Date" = "single","Comparison" = "compare"),
                       inline = FALSE)
          ),
        HTML("<br>"),HTML("<br>"),
        #date picker for single date
        fluidRow(
          dateInput("date", label="Single Dates", value = "2021-08-23",
                    min="2001-01-01", max="2021-11-30", format = "yyyy/mm/dd")
        ),
        HTML("<br>"),HTML("<br>"),
        
        #date range for comparison
        fluidRow(dateRangeInput("date1", label="Compare Dates", start = "2021-08-23", end = "2015-08-23",  min="2001-01-01", max="2021-11-30", format = "yyyy/mm/dd",
                                separator = "and")),
        HTML("<br>"),HTML("<br>"),
        #next and prev buttons
        fluidRow(
          column(6,
                 actionButton(inputId = "prevButton", label = "Prev")),
          column(6,
                 actionButton(inputId = "nextButton", label = "Next"))
        ),
        fluidRow(
          selectInput("sortby", "Bar Plot View", choices = c("Alphabetical" = "alpha", "Ascending" = 'asc', "Descending" = "desc")),
        ),
        HTML("<br>"),HTML("<br>"),
        fluidRow(
          selectizeInput('select_station', "Select Station", choices = stations,
                         selected = "UIC-Halsted")
        ),
        HTML("<br>"),HTML("<br>"),
        fluidRow(
          selectInput("year", "Year",
                      choices = c("All", 2021:2001),
                      selected = c(2021)
                      )
          )
        )#Wellpanel1
      ), #Column-1
    
    #Rest of the stuff
    column(width = 10,
           
           #Bar plot column 
           column(width = 4,
           fluidRow( style = "height:85vh;", plotlyOutput(height = "100%", "bar_graph"))
           ),
           
           #Line data Table and map column
           column( width = 4,
             fluidRow(style = "height:15vh;", uiOutput(height = "85%", style ="width: 50%;","main_table")),
             fluidRow(style = "margin-top:300px; height:60vh;",leafletOutput(height = "90%","map_dash"))
           ),
           
           #Yearly graph column 
           column( width = 4,
                   
                   column(width = 12,
                          
                          #The yearly graph for station goes here 
                          fluidRow(uiOutput("plot_and_table"))
                   )
                   
             
           )
          
           )
    
    
    
    )
  )

# ui <- dashboardPage(skin = "black",
#                     dashboardHeader(title = "CS424 Project-2"),
#                     dashboardSidebar(collapsed = FALSE, disable = FALSE,
#                                      sidebarMenu(
#                                        id = "menu_tabs",
#                                        tags$div(style = "margin-top: 300px;"),
#                                        menuItem("Dashboard", tabName = "map_dash", selected = TRUE, icon = icon("dashboard")),
#                                        menuItem("About", tabName = "about", icon = icon("sunglasses", lib = "glyphicon"))
#                                        
#                                      )
#                     ),
#                     dashboardBody(
#                       #using shinyjs to disable/enable inputs
#                      
#                       #tags$head(tags$style(".sidebar-menu li { margin-bottom: 20px; }")),
#                       tabItems(
#                         tabItem(tabName = "map_dash", 
#                                 fluidPage(
#                                   sidebarLayout(position = "left",
#                                                 sidebarPanel(style = "margin-top: 70%",
#                                                              width = 2,
#                                                              #Radio buttons
#                                                              fluidRow(
#                                                                column(8,
#                                                                       radioButtons("radio_single", "Select mode",
#                                                                                    c("Single Date" = "single",
#                                                                                      "Comparison" = "compare"),
#                                                                                    inline = FALSE)
#                                                                       )
#                                                                ),
#                                                              #date picker for single date
#                                                              fluidRow(
#                                                                dateInput("date", label="Single Dates", value = "2021-08-23",
#                                                                        min="2001-01-01", max="2021-11-30", format = "yyyy/mm/dd")
#                                                              ),
#                                                              #date range for comparison
#                                                              fluidRow(dateRangeInput("date1", label="Compare Dates", start = "2021-08-23", end = "2001-08-23",  min="2001-01-01", max="2021-11-30", format = "yyyy/mm/dd",
#                                                                             separator = "and")),
#                                                              #next and prev buttons
#                                                              fluidRow(
#                                                                column(6,
#                                                                       actionButton(inputId = "prevButton", label = "Prev")),
#                                                                column(6,
#                                                                       actionButton(inputId = "nextButton", label = "Next"))
#                                                              ),
#                                                              HTML("<br>"),
#                                                              div(selectizeInput('select_station', "Select Station", choices = stations,
#                                                                                selected = "UIC-Halsted")
#                                                              
#                                                              ),
#                                                              HTML("<br>"),
#                                                              div(
#                                                                fluidRow(
#                                                                  selectInput("sortby", "Bar Plot View", choices = c("Alphabetical" = "alpha", "Ascending" = 'asc', "Descending" = "desc")),
#                                                                )
#                                                              ),
#                                                              fluidRow(column(8,
#                                                                              div(selectInput("year", "Year",
#                                                                                              choices = c("All", 2021:2001),
#                                                                                              selected = c(2021)
#                                                                              )
#                                                                              )
#                                                              )
#                                                              )
#                                                             
#                                                            
#                                               ),
#                                               
#                                               mainPanel(
#                                                 
#                                                 # fluidRow(
#                                                 #   column(12,
#                                                 #          leafletOutput("map_dash")),
#                                                 #   column(12,
#                                                 #          uiOutput("bar_graph"),),
#                                                 #   column(12,dddfd
#                                                 #          uiOutput("plot_and_table"))
#                                                 # )
#                                                 # leafletOutput("map_dash"),
#                                                 # uiOutput("bar_graph"),
#                                                 # uiOutput("plot_and_table")
#                                                  fluidPage(
#                                                   #splitLayout(cellWidths = c("100%", "100%", "400%"), leafletOutput("map_dash"), uiOutput("bar_graph"), uiOutput("plot_and_table")),
#                                                   splitLayout(
#                                                     cellWidths = 1000,
#                                                     cellArgs = list(style = "padding: 6px"),
#                                                     leafletOutput("map_dash"),
#                                                     uiOutput("bar_graph"),
#                                                     uiOutput("plot_and_table")
#                                                   )
#                                                   #Leaflet Map UI
#                                                   # column(width = 12,
#                                                   #        leafletOutput("map_dash"))
#                                                   )
#                                                 )
#                         )
#                       )),
#                         
#                         tabItem(tabName = "about",
#                                 tags$div(style = "margin-top: 200px;"),
#                                 h1('About'),
#                                 h2('Created by Aditya Ranganathan on 02/07/2022'),
#                                 h3(""),
#                                 h3(""),
#                                 h3("The dashboard displays data reagrding CTA rides in a clear and intuitive manner.
#                  Users can check ride data of 3 different CTA stations: O'Hare Airport, UIC-Halsted and Racine.
#                  The data can be viewed from a yearly, monthly, weekly or daily basis. Users have the option of seeing the data either as plots or in a tabular form"),
#                                 h3("Users can get an idea about the number of passengers that travel through the 'L' and can also correlate certain major events that occurred in Chicago with respect to the number of riders during that time period."),
#                                 h3("Data was sourced from from the Chicago Data Portal at", tags$a(href="https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f/data", "this link")),
#                                 h3("The dataset consists of 1.1 million rows and has attributes 'stationname', 'station_id', 'date', 'rides' and 'daytype'")
#                                 
#                         )
#                       )
#                     )
# )



# SERVER=======================================================================================================
server <- function(input, output, session){
  # updateSelectizeInput(session, 'select_station', choices = stations, server = TRUE)
  #input$select_station = 'UIC-Halsted'
  
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
      singleDate <- input$date
      return(singleDate)
      })
  
  #Multi date reactive returns difference df with station
  multiDateReactive <- reactive({
      date1 <- input$date1[1]
      date2 <- input$date1[2]
      date_df_1 <- subset(df, df$date == date1)
      date_df_2 <- subset(df, df$date == date2)
      
      difference_df <- inner_join(x=date_df_1, y=date_df_2, by="station_id")
      difference_df <- subset(difference_df, select = c("station_id", "stationname.x", "date.x", "date.y", "lat.x", "long.x", "rides.x","rides.y", "line.x", "line_color.x")) 
      difference_df$rides <- difference_df$rides.x - difference_df$rides.y
      
      difference_df <- difference_df %>%
        rename( stationname = stationname.x,
                date_1 = date.x,
                date_2 = date.y,
                line = line.x,
                lat = lat.x,
                long = long.x,
                line_color = line_color.x)
      
      
      difference_df %>%
        mutate(sign = case_when(
          rides < 0 ~ "Negative",
          rides > -1 ~ "Positive"
        ))
      
      return(difference_df)
  })
  

  
  #Change value of Selectize input on map click
  observeEvent(input$map_dash_marker_click,{
    print("Station cicked on map")
    #updating select-input based on map
    click <- input$map_dash_marker_click
    station <- click$id
    leafletProxy("map_dash", session) %>%
      clearPopups() 
    isolate({
    updateSelectInput(session, "select_station", 
                      selected = click$id)
    })
  })
  
  #Map pop up on selectize Input 
  observeEvent(input$select_station,{
    print('Selectize Input Station selected ')
    station_name <- input$select_station
    df <- dataframeReactive()
    temp <- subset(df, df$stationname == station_name )
    isolate({
    shinyjs::js$markerClick(temp$stationname)
    })
    })
  

  
  
  #rendering map output
  output$map_dash <- renderLeaflet({
    map <- map %>% 
      htmlwidgets::onRender("
          function(el, x) {
            map = this;
          }"
      )    
    #for single date
    if(input$radio_single == 'single'){
      df <- dataframeReactive()
      
      #adding markers for single date to map
      map <- map %>% 
        clearPopups() %>%
        clearMarkers() %>%
        addCircleMarkers(
        data = df, lat = ~lat, lng = ~long, 
        
        #Taking the log and scaling the radius
        radius = ~log(rides+10)*1.25,
        color = ~line_color,
        layerId = ~stationname,
        popup = paste("<center><strong>" ,df$stationname, "</strong>", "<br>",
                      df$line, "<br>",
                      "Rides: ", df$rides, "<br> </center>"
                      )
        )
    }
    
    # adding markers for date comparison on map
    else{
      
      #Adding divergent markers for comparison
      
      diff_df = dataframeReactive()
      
      #changing line color acc to + or - ridership change
      diff_df <- transform(diff_df, line_color = ifelse(diff_df$rides < 0, "#ef8a62", "#67a9cf"))
      #print(head(diff_df))
      
      #Transforming all ride change as +
      #difference_df$rides <- abs(difference_df$rides)
      
      map <- map %>%
        clearPopups() %>%
        clearMarkers() %>%
        addCircleMarkers(data = diff_df,
                         lat = ~lat, lng = ~long, 
                         #Taking the log and scaling the radius
                         radius = ~log(abs(rides)+10)*1.5,
                         color = ~line_color,
                         layerId = ~stationname,
                         popup = paste("<center><strong>" , diff_df$stationname, "</strong>", "<br>",
                                       diff_df$line, "<br>",
                                      "Change in Ridership: ", diff_df$rides, "<br> </center>")
                         ) %>%
        addLegend(position = "bottomleft",
                  colors = c("#ef8a62", "#67a9cf"),
                  labels = c('Less','More')
          
        )
    }
    return(map)  
  })
  
  #Extracting subset of datframe for single date and compare dates
  dataframeReactive <- reactive({ 
    if(input$radio_single == "single"){
      singleDate <- singleDateReactive()
      date_df <- subset(df, df$date == singleDate)
    }
    else{
      
      diff_df = multiDateReactive()
      print(diff_df)
      return(diff_df)
    }
  })
 
  # BARPLOT=====================================================================================================
  
  # Pass dataframe to table layout function
  bar_table <- function(){
      table_frame <-dataframeReactive()
      table_frame <- table_frame[c("stationname", "line", "rides")]
      table_frame <- table_frame %>%
        rename(Station = stationname, Rides = rides, Line = line)
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
    
    
    if(input$radio_single == 'single'){
      data <- dataframeReactive()
      if(input$sortby == 'alpha'){
        data <- data[order(data$stationname),]
      }
      else if(input$sortby == 'asc'){
        data <- data[order(data$rides),]
      }
      else{
        data <- data[order(-data$rides),]  
      }
      
      yform <- list(categoryorder = "array",
                    categoryarray = rev(data$stationname)
      )
      p = plot_ly(data, y = ~stationname, x = ~rides, type = "bar", text=~line) %>%
        layout(title = 'Ridership Data', yaxis = yform)
      return(p)  
    }
    
    else{
      data <- dataframeReactive()
      if(input$sortby == 'alpha'){
        data <- data[order(data$stationname),]
      }
      else if(input$sortby == 'asc'){
        data <- data[order(data$rides),]
      }
      else{
        data <- data[order(-data$rides),]  
      }
      
      
      fig <- ggplot(data = data,
                    aes(x = stationname, y = rides))+
        geom_bar(stat = "identity", aes(fill=rides>0))+ + ggtitle(paste(input$select_station, "CTA Station")) +
        labs(x = "Ride Difference", y = "Stations") + scale_fill_discrete(name = "Ridership Change") +
        coord_flip()
      
    }
      return(fig)
    
  
    
    })
  
  # Render Bar Plot and Table
  output$bar_graph <- renderPlotly( {plot_1()} )

  output$main_table <- renderUI({
    uiOutput("bar_table")
  })
  
  # GRAPHS AND TABLES===================================================================================================== 
    
  # Sum functions============================================
  week_sigma <- function(day, year, station){
    if(year == "All"){
      if(station == "All"){
        sum(df[df$week_day == day,]$rides)
      } else{
        data <- df[df$stationname == station,]
        sum(data[data$week_day == day,]$rides)
      }
    }
    else{
      if(station == "All"){
        sum(df[df$year == year & df$week_day == day,]$rides)
      } else{
        data <- df[df$stationname == station,]
        sum(data[data$year == year & data$week_day == day,]$rides)
      }
    }
  }
  
  month_sigma <- function(month, year, station){
    if(year == "All"){
      if(station == "All"){
        sum(df[df$month_name == month,]$rides)
      } else{
        data <- df[df$stationname == station,]
        sum(data[data$month_name == month,]$rides)
      }
    }
    else{
      if(station == "All"){
        sum(df[df$year == year & df$month_name == month,]$rides)
      } else{
        data <- df[df$stationname == station,]
        sum(data[data$year == year & data$month_name == month,]$rides)
      }
    }
  }
  
  year_sigma <- function(year, station){
    data <- df[df$stationname == station,]
    sum(data[data$year == year,]$rides)
  }
  
  daily_df <- function(year, station){
    if(year == "All"){
      if(station == "All"){
        date_frame <- df[c("date", "rides")]
        return(date_frame)
      }
      else{
        data <- df[df$stationname == station,]
        data <- data[c("date", "rides")]
        return(data)
      }
    } 
    else{
      if(station == "All"){
        data <- df[df$year == year,]
        data <- data[c("date", "rides")]
        return(data)
      } 
      else{
        data <- df[df$stationname == station & df$year == year,]
        data <- data[c("date", "rides")]
        return(data) 
      }
    }
  }
  
  week_df <- function(year, station){
    days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    rides <- sapply(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), function(day) week_sigma(day, year, station))
    week_frame <- data.frame(days, rides)
    return(week_frame)
  }
  
  month_df <- function(year, station){
    month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    rides <- sapply(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), function(month) month_sigma(month, year, station))
    month_frame <- data.frame(month, rides)
    return(month_frame)
  }
  
  year_df <- function(station){
    year <- c(2021:2001)
    rides <-  sapply(c(2021:2001), function(year) year_sigma(year, station))
    year_frame <- data.frame(year,rides)
    return (year_frame)
  }
  
  
  # Pass dataframe to layout function=================================================================
    daily_table <- function(){
      table_frame <- daily_df(input$year, input$select_station)
      table_frame <- table_frame %>%
        rename(Date = date, Rides = rides)
      return(table_frame)
    }
    
    week_table <- function(){
      table_frame <- week_df(input$year, input$select_station)
      table_frame <- table_frame %>%
        rename(Week_Day = days, Rides = rides)
      return(table_frame)
    }
    
    month_table <- function(){
      table_frame <- month_df(input$year, input$select_station)
      table_frame <- table_frame %>%
        rename(Month = month, Rides = rides)
      return(table_frame)
    }
    
    year_table <- function(){
      table_frame <- year_df(input$select_station)
      table_frame <- table_frame %>%
        rename(Year = year, Rides = rides)
      return(table_frame)
    }
  # Table layouts
    # daily layout
    output$daily_table <- renderUI({
      div(
        tags$style(
          HTML('.datatables {width: inherit !important;}')
        ),
        datatable(
          daily_table(),
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
      # week layout
      output$week_table <- renderUI({
        div(
          tags$style(
            HTML('.datatables {width: inherit !important;}')
          ),
          datatable(
            week_table(),
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
    
      # month layout
      output$month_table <- renderUI({
        div(
          tags$style(
            HTML('.datatables {width: inherit !important;}')
          ),
          datatable(
            month_table(),
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
      
      # year layout
      output$year_table <- renderUI({
        div(
          tags$style(
            HTML('.datatables {width: inherit !important;}')
          ),
          datatable(
            year_table(),
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
  
    # GRAPHS========================================
      output$daily_plot <- renderPlot({
        ggplot(data = daily_df(input$year, input$select_station), aes(x = date, y = rides)) +
          geom_bar(stat = "identity", aes(fill = rides)) + 
          labs(x = "Date", y ="Rides", title = "Daily Entries") + scale_y_continuous(labels = comma)
      })
      
      output$week_plot <- renderPlot({
        ggplot(data = week_df(input$year, input$select_station), aes(x = factor(days, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), y = rides)) +
          geom_bar(stat = "identity", aes(fill = rides)) + 
          labs(x = "Week Day", y ="Rides", title = "Weekly entries") + scale_y_continuous(labels = comma)
      })
      
      output$month_plot <- renderPlot({
        ggplot(data = month_df(input$year, input$select_station), aes(x = factor(month, level = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), y = rides)) +
          geom_bar(stat = "identity", aes(fill = rides)) +
          labs(x = "Month", y ="Rides", title = "Monthly entries") + scale_y_continuous(labels = comma)
      })
      
      output$year_plot <- renderPlot({
        ggplot(data = year_df(input$select_station), aes(x = year, y = rides)) +
          geom_bar(stat = "identity", aes(fill = rides)) + 
          labs(x = "Year", y ="Rides", title = "Yearly entries") + scale_y_continuous(labels = comma)
      })
      
    # render graph and table output
    output$plot_and_table <- renderUI({
        # fluidPage(
        #   fluidRow(column(12, div(plotOutput("daily_plot"))),
        #            column(12, div(plotOutput("week_plot"))),
        #            column(12, div(plotOutput("month_plot"))),
        #            column(12, div(plotOutput("year_plot")))
        #            ),
        #   fluidRow(column(12, uiOutput("daily_table")),
        #            column(12, uiOutput("week_table")),
        #            column(12, uiOutput("month_table")),
        #            column(12, uiOutput("year_table")),
        #            )
        #           )
      
          fluidRow(
            column(4, 
                   fluidPage(
                     fluidRow(
                       column(4, div(plotOutput("daily_plot"))),
                       column(4, uiOutput("daily_table"))
                       )
                     )
                   ),
            column(4, 
                   fluidPage(
                     fluidRow(
                       column(4, div(plotOutput("week_plot"))),
                       column(4, uiOutput("week_table"))
                     )
                   )
            ),
            column(4, 
                   fluidPage(
                     fluidRow(
                       column(4, div(plotOutput("month_plot"))),
                       column(4, uiOutput("month_table"))
                     )
                   )
            ),
            column(4, 
                   fluidPage(
                     fluidRow(
                       column(4, div(plotOutput("year_plot"))),
                       column(4, uiOutput("year_table"))
                     )
                   )
            )
            )
          
      })
    
    
    
}

shinyApp(ui = ui, server = server)
