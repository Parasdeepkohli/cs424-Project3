library(shiny)
library(leaflet)
library(mapview)
library(ggplot2)
library(DT)

communities <- sort(readLines("Communities.txt"))

fluidPage(title="Chicago power usage",
          
          navbarPage(
            
            title = "Navigation",
            id = "nav",
            position = "static-top",
            collapsible = TRUE,
            selected = "About",
            tabPanel(
              title = "About",
              tags$h1("Welcome to Project 3 of CS 424!", `style` = "text-align:center"),
              tags$h4("Created by: Parasdeep (Spring 2021)", `style` = "text-align:right"),
              tags$u(tags$h3("Purpose:", `style` = "font-weight:bold")),
              tags$ul(tags$li("Visualize the energy and gas used by various types of buildings in the Near West Side community area of Chicago", `style` = "font-size:20px"),
                      tags$li("Compare the energy and gas used between two community areas within the city of Chicago (Compare Communities)",`style` = "font-size:20px"),
                      tags$li("Geographically visualize variance in power usage across the entire city (Compare communities)", `style` = "font-size:20px")),
              tags$u(tags$h3("The Data:", `style` = "font-weight:bold")),
              tags$ul(tags$li("An excel file detailing the power usage by community area in the city of Chicago for the year of 2010", `style` = "font-size:20px"),
                      tags$li("The file provides totals for both gas and energy, as well as providing monthly values for both attributes.",`style` = "font-size:20px"),
                      tags$li("Please find the link to the data source here:", tags$a(`href` = "https://www.kaggle.com/chicago/chicago-energy-usage-2010", "Source"), `style` = "font-size:20px")),
              tags$u(tags$h3("Notes and tips:", `style` = "font-weight:bold")),
              tags$ul(tags$li("Please use the navbar above to navigate the app", `style` = "font-size:20px"),
                      tags$li("The application will either show a blank slate, or an error, in case your filters do not match any data points", `style` = "font-size:20px"),
                      tags$li("Select the source Total in the Month drop-down inputs to see the data for all months. It is selected by default in each visualization.", `style` = "font-size:20px"),
                      tags$li("The application takes some time to start up as the data files to be downloaded are large.", `style` = "font-size:20px"),
                      tags$li("Select the Community 'All of Chicago' to see the data for the entire city. Note that this may take up to a minute to load due to the data size", `style` = "font-size:20px"),
                      tags$li("Switching from block level to tract level takes several seconds, so please be patient!", `style` = "font-size:20px")),
              tags$u(tags$h3("Known Issues", `style` = "font-weight:bold")),
              tags$ul(tags$li("Sometimes, the maps will fail to load and display a gray screen. Changing the selected community from the dropdown fixes this.", `style` = "font-size:20px"))
              
            ),
            tabPanel("Near West Side",
                     sidebarLayout(
                       sidebarPanel(
                         width = 2,
                         tags$head(tags$style("#NWS{height:40vh !important;}")),
                         selectInput(
                           inputId = "SourcesNWS",
                           label = "Pick a view",
                           choices = c("Electricity", "Gas", "Avg Building Age", "Avg Building Height", "Total population"),
                           selected = "Electricity"
                         ),
                         selectInput(
                           inputId = "TypeNWS",
                           label = "Pick a building type",
                           choices = c("All", "Commercial", "Industrial", "Residential"),
                           selected = "All"
                         ),
                         conditionalPanel(condition = "input.SourcesNWS == 'Electricity' || input.SourcesNWS == 'Gas'",
                           selectInput(
                             inputId = 'MonthNWS', 
                             label = 'Select a month',
                             choices = c("Total", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October",
                                         "November", "December"),
                             selected = "Total"
                           )
                         ),
                         actionButton("reset_button", "Reset view")
                       ),
                       mainPanel(
                         width = 10,
                         title = "Near West Side",
                         leafletOutput('NWS'),
                         splitLayout(
                           cellWidths = c("33%", "33%", "33%"),
                           plotOutput('NWSElec', height = "500px"),
                           plotOutput('NWSGas', height = "500px"),
                           dataTableOutput("NWSTable")
                         )
                       )
                     )
            ),
            tabPanel("Compare Communities",
                     fluidRow(
                       column(width = 2,
                              sidebarLayout(
                                sidebarPanel(width = 12,
                                            radioButtons(
                                               inputId = 'BorT',
                                               label = "Pick the view",
                                               choices = c('Blocks', 'Tracts'),
                                               selected = 'Blocks'
                                             ),
                                            selectInput(
                                              inputId = "Communities1",
                                              label = "Top Community",
                                              choices = c('All of Chicago', communities),
                                              selected = "Near West Side"
                                            ),
                                            
                                            selectInput(
                                              inputId = "Sources1",
                                              label = "Top view",
                                              choices = c("Electricity", "Gas", "Avg Building Age", "Avg Building Height", "Total population"),
                                              selected = "Electricity"
                                            ),
                                            
                                            selectInput(
                                              inputId = "Type1",
                                              label = "Top building type",
                                              choices = c("All", "Commercial", "Industrial", "Residential"),
                                              selected = "All"
                                            ),
                                            
                                            conditionalPanel(condition = "input.SourcesNWS == 'Electricity' || input.SourcesNWS == 'Gas'",
                                                             selectInput(
                                                               inputId = 'Month1', 
                                                               label = 'Top month',
                                                               choices = c("Total", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October",
                                                                           "November", "December"),
                                                               selected = "Total"
                                                             )
                                            ),
                                            
                                            selectInput(
                                              inputId = "Communities2",
                                              label = "Bottom Community",
                                              choices = c('All of Chicago', communities),
                                              selected = "Loop"
                                            ),
                                            
                                            selectInput(
                                              inputId = "Sources2",
                                              label = "Bottom view",
                                              choices = c("Electricity", "Gas", "Avg Building Age", "Avg Building Height", "Total population"),
                                              selected = "Electricity"
                                            ),
                                            selectInput(
                                              inputId = "Type2",
                                              label = "Bottom building type",
                                              choices = c("All", "Commercial", "Industrial", "Residential"),
                                              selected = "All"
                                            ),
                                            conditionalPanel(condition = "input.SourcesNWS == 'Electricity' || input.SourcesNWS == 'Gas'",
                                                             selectInput(
                                                               inputId = 'Month2', 
                                                               label = 'Bottom month',
                                                               choices = c("Total", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October",
                                                                           "November", "December"),
                                                               selected = "Total"
                                                             )
                                            ),
                                            selectInput(
                                              inputId = "Colors",
                                              label = "Pick a color scheme",
                                              choices = c("Viridis", "Heat", "SunsetDark"),
                                              selected = "Viridis"
                                            ),
                                            
                                            actionButton("reset_button1", "Reset top view"),
                                            actionButton("reset_button2", "Reset bot view"),
                                          ),
                                mainPanel()
                              )
                       ),
                       column(width = 6,
                              tags$head(tags$style("#map1{height:43vh !important;}
                                            #map2{height:43vh !important;")),
                              leafletOutput("map1"),
                              br(),
                              leafletOutput("map2")
                              
                       ),
                       column(width = 4,
                              sidebarLayout(
                                sidebarPanel(width = 0,
                                             plotOutput('Elec1', height = "210px"),
                                             plotOutput('Gas1', height = "210px"),
                                             plotOutput('Elec2', height = "210px"),
                                             plotOutput('Gas2', height = "210px"),
        
                                ),
                                mainPanel()
                              )
                       )
                       
                     )
            )
          )
)
