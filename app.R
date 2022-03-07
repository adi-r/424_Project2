setwd("C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project2")
getwd()

# LIBRARIES=======================================================================================================
library(lubridate)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(shiny)
library(shinydashboard)

# READ DATA=======================================================================================================
df <- do.call(rbind, lapply(list.files(pattern = "*.csv"), read.csv))
df <- df[, -1]
# Set date column in Date format
df$date <- as.Date(df$date, "%Y-%m-%d")

str(df)

# UI==============================================================================================================
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Chicago 'L' Visualization"),
                    dashboardSidebar(collapsed = FALSE, disable = FALSE,
                                     sidebarMenu(
                                       id = "menu_tabs",
                                       tags$div(style = "margin-top: 300px;"),
                                       menuItem("Station Comparison", tabName = "station_compare", selected = TRUE, icon = icon("signal", lib = "glyphicon")),
                                       menuItem("Table Data", tabName = "table_data",icon = icon("dashboard")),
                                       menuItem("Dates of Interest", tabName = "doi", icon = icon("calendar")),
                                       menuItem("About", tabName = "about", icon = icon("sunglasses", lib = "glyphicon"))
                                     )
                    ),
                    dashboardBody(
                      tags$head(tags$style(".sidebar-menu li { margin-bottom: 20px; }")),
                      tabItems(
                        tabItem(tabName = "station_compare", 
                                sidebarLayout(position = "left",
                                              sidebarPanel(style = "margin-top: 80%",
                                                           h4("Left"),
                                                           div(selectInput("station1_compare", "Station",
                                                                           choices = c("All", "UIC-Halsted", "O'Hare Airport", "Racine"),
                                                                           selected = c("UIC-Halsted")
                                                           )
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
                                                fluidRow(
                                                  splitLayout(cellWidths = c("75%", "75%"), uiOutput("compare_plots1"), uiOutput("compare_plots2"))), width = 8))
                        ),
                        tabItem(tabName = "table_data",
                                sidebarLayout(position = "left",
                                              sidebarPanel(style = "margin-top: 50%",
                                                           h4("Left"),
                                                           div(selectInput("station1_table_data", "Station",
                                                                           choices = c("All", "UIC-Halsted", "O'Hare Airport", "Racine"),
                                                                           selected = c("UIC-Halsted")
                                                           )
                                                           ),
                                                           fluidRow(column(8,
                                                                           div(selectInput("year1_table_data", "Year",
                                                                                           choices = c("All", 2021:2001),
                                                                                           selected = c(2021)
                                                                           )
                                                                           )
                                                           )
                                                           ),
                                                           fluidRow(column(8,
                                                                           div(selectInput("time1_table_data", "Time Frame",
                                                                                           choices = c("Daily", "Weekly", "Monthly"),
                                                                                           selected = c("Daily")
                                                                           )
                                                                           )
                                                           )
                                                           ),
                                                           
                                                           h4("Right"),
                                                           div(selectInput("station2_table_data", "Station",
                                                                           choices = c("All", "UIC-Halsted", "O'Hare Airport", "Racine"),
                                                                           selected = c("O'Hare Airport")
                                                           )
                                                           ),
                                                           fluidRow(column(8,
                                                                           div(selectInput("year2_table_data", "Year",
                                                                                           choices = c("All", 2021:2001),
                                                                                           selected = c(2021)
                                                                           )
                                                                           )
                                                           )
                                                           ),
                                                           fluidRow(column(8,
                                                                           div(selectInput("time2_table_data", "Time Frame",
                                                                                           choices = c("Daily", "Weekly", "Monthly"),
                                                                                           selected = c("Daily")
                                                                           )
                                                                           )
                                                           )
                                                           ),
                                                           width = 2
                                              ),
                                              mainPanel(tags$div(style = "margin-top: 100px;"),
                                                        fluidRow(
                                                          splitLayout(cellWidths = c("70%", "70%"), uiOutput("table_plots1"), uiOutput("table_plots2"))), width = 8)
                                )
                        ),
                        tabItem(tabName = "doi",
                                fluidRow(
                                  tags$div(style = "margin-top: 20px;"),
                                  tabBox(
                                    title = "",
                                    id = "tabset1", height = NULL, selected = "Date: 01", width=12,
                                    tabPanel("Date: 01", h1("O'Hare Airport 2019"),h3("Heavy Rainfall and Storm Warning might have led to the extreme decline in number of riders from Sept 28th to Oct 5th 2019."),  
                                             fluidPage(
                                               fluidRow(column(12, plotOutput("date_plot1"))),
                                               fluidRow(column(12, uiOutput("assoc_table1")))
                                             )),
                                    tabPanel("Date: 02", h1("9/11 OHare"), h3("Due to the attacks on September 11th, there was a sharp decline in number of riders from Sept."), plotOutput("date_plot2")),
                                    tabPanel("Date: 03", h1("UIC COVID"), h3("Due to the pandemic and CDC guidelines, rider numbers were at a low time from April 2020 all the way till July 2021"), fluidPage(
                                      fluidRow(column(12, plotOutput("date_plot3"))),
                                      fluidRow(column(12, plotOutput("date_plot4")))
                                    )),
                                    tabPanel("Date: 04", h1("O'Hare COVID"), h3("Due to CDC regulations and travel restrictions we can observe a significant decrease in ridership from April 2020 to July 2021"), fluidPage(
                                      fluidRow(column(12, plotOutput("date_plot5"))),
                                      fluidRow(column(12, plotOutput("date_plot13")))
                                    )),
                                    tabPanel("Date: 05", h1("Heavy Rainfall on July 12th 2008 OHare"), h3("As per news reports, there was extremely heavy rainfall and storms occurring which leads to the large gap present in the graph."), fluidPage(
                                      fluidRow(column(12, plotOutput("date_plot6"))),
                                      fluidRow(column(12, uiOutput("assoc_table3")))
                                    )),
                                    tabPanel("Date: 06", h1("Train Derail March 24th 2014"), h3("On this date, a train derailed injuring 34 people. The train was removed on the 27th and the station reopened for activity on the 30th"), fluidPage(
                                      fluidRow(column(12, plotOutput("date_plot7"))),
                                      fluidRow(column(12, uiOutput("assoc_table4")))
                                    )),
                                    tabPanel("Date: 07", h1("Oct 6th 2017 Chicago Cubs win against Washington Nationals"), h3("There's a huge spike in rider activity at O'Hare Airport which could possibly be attributed to the turnout at the game and the win that proceeded it"), fluidPage(
                                      fluidRow(column(12, plotOutput("date_plot8"))),
                                      fluidRow(column(12, uiOutput("assoc_table5")))
                                    )),
                                    tabPanel("Date: 08", h1("Obama Oct/Nov 2008 Speech"), h3("There's unusually high activity in the month of Oct/Nov when the general ridership typically starts declining. This spike in rider increase could be attributed to Obamas presence in Chicago since it was during this time he addressed the people at Grant Park on Nov 8th 2008 "), fluidPage(
                                      fluidRow(column(12, plotOutput("date_plot9"))),
                                      fluidRow(column(12, uiOutput("assoc_table6")))
                                    )),
                                    tabPanel("Date: 09", h1("Pattern in UIC-Halsted Summer Decline"), h3("There's a decline of passengers during the summer months in UIC-Halsted station which could be attributed to the lack of students not attending classes during the summer."), fluidPage(
                                      fluidRow(column(12, plotOutput("date_plot10"))),
                                      fluidRow(column(12, plotOutput("date_plot11")))
                                    )),
                                    tabPanel("Date: 10", h1("Nov 2nd 2016 Cubs win World Series"), h3("The high spikes in the number of riders can be attributed to the popularity of the match considering it's the first time the Cubs have reached the finals since 1908."), fluidPage(
                                      fluidRow(column(12, plotOutput("date_plot12"))),
                                      fluidRow(column(12, uiOutput("assoc_table7")))
                                    ))
                                  )
                                )),
                        
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