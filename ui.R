library(shiny)
library(leaflet)
library(mapview)
library(ggplot2)

fluidPage(title="Chicago power usage",
          
          navbarPage(
            
            title = "Navigation",
            id = "nav",
            position = "static-top",
            collapsible = TRUE,
            selected = "Near West Side",
            tabPanel(
              title = "About",
              tags$h1("Welcome to Project 3 of CS 424!", `style` = "text-align:center"),
              tags$h4("Created by: Parasdeep (Spring 2021)", `style` = "text-align:right"),
              tags$u(tags$h3("Purpose:", `style` = "font-weight:bold")),
              tags$ul(tags$li("Visualize the energy and gas used by various types of buildings in the Near West Side community area of Chicago", `style` = "font-size:20px"),
                      tags$li("Compare the energy and gas used between two community areas within the city of Chicago (Compare areas)",`style` = "font-size:20px"),
                      tags$li("Geographically visualize variance in power usage across the entire city (Compare areas)", `style` = "font-size:20px")),
              tags$u(tags$h3("The Data:", `style` = "font-weight:bold")),
              tags$ul(tags$li("An excel file detailing the power usage by community area in the city of Chicago for the year of 2010", `style` = "font-size:20px"),
                      tags$li("The file provides totals for both gas and energy, as well as providing monthly values for both attributes.",`style` = "font-size:20px"),
                      tags$li("Please find the link to the data sources here:", tags$a(`href` = "https://www.kaggle.com/chicago/chicago-energy-usage-2010", "Source"), `style` = "font-size:20px")),
              tags$u(tags$h3("Notes and tips:", `style` = "font-weight:bold")),
              tags$ul(tags$li("Please use the navbar above to navigate the app", `style` = "font-size:20px"),
                      tags$li("The source US-Total will transform the map into the entire country. It is at the bottom of both state inputs in Compare States", `style` = "font-size:20px"),
                      tags$li("The minimum and maximum sliders affect both zones in compare states", `style` = "font-size:20px"),
                      tags$li("The application will either show a blank slate, or an error, in case your filters do not match any data points", `style` = "font-size:20px"))
              
            ),
            tabPanel("Near West Side",
                     sidebarLayout(
                       sidebarPanel(
                         width = 2,
                         tags$head(tags$style("#NWS{height:90vh !important;}")),
                         selectInput(
                           inputId = "SourcesNWS",
                           label = "Pick a view",
                           choices = c("Electricity", "Gas", "Building Age", "Building Type", "Building Height", "Total population"),
                           selected = "Electricity"
                         ),
                         checkboxInput(inputId = 'allIL', label = 'All', value = TRUE),
                         actionButton(inputId = 'renewableIL', label = 'Renewable'),
                         actionButton(inputId = 'nonrenewIL', label = 'Non-Renew'),
                         br(),
                         actionButton("reset_button", "Reset view")
                       ),
                       mainPanel(
                         width = 10,
                         title = "Near West Side",
                         mapviewOutput('NWS')
                       )
                     )
            ),
            tabPanel("Compare states",
                     fluidRow(
                       column(width = 3,
                              sidebarLayout(
                                sidebarPanel(width = 12,
                                             fluidRow(
                                               column(6,
                                                      checkboxGroupInput(
                                                        inputId = "Sources1", 
                                                        label = "Top map", 
                                                        choices = c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Other", "Solar", "Wind", "Geothermal")
                                                      ),
                                                      checkboxInput(inputId = 'all1', label = 'All', value = TRUE),
                                                      actionButton(inputId = 'renewable1', label = 'Renewable'),
                                                      actionButton(inputId = 'nonrenew1', label = 'Non-Renew'), 
                                                      checkboxInput(inputId = "merge", label = "Link options", value = FALSE)
                                               ),
                                               column(6,
                                                      conditionalPanel(condition = "input.merge == false",
                                                                       checkboxGroupInput(
                                                                         inputId = "Sources2", 
                                                                         label = "Bottom map", 
                                                                         choices = c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Other", "Solar", "Wind", "Geothermal")
                                                                       ),
                                                                       checkboxInput(inputId = 'all2', label = 'All', value = TRUE),
                                                                       actionButton(inputId = 'renewable2', label = 'Renewable'),
                                                                       actionButton(inputId = 'nonrenew2', label = 'Non-Renew'),
                                                      )
                                               )
                                             ),
                                             sliderInput(
                                               inputId <- "MinSlider",
                                               label <- "Minimum Energy Generation",
                                               min = 0,
                                               max = 32000000,
                                               value = 0
                                             ),
                                             sliderInput(
                                               inputId <- "MaxSlider",
                                               label <- "Maximum Energy Generation",
                                               min = 0,
                                               max = 32000000,
                                               value = 32000000
                                             )
                                ),
                                mainPanel()
                              )
                       ),
                       column(width = 7,
                              tags$head(tags$style("#map1{height:43vh !important;}
                                            #map2{height:43vh !important;")),
                              leafletOutput("map1"),
                              br(),
                              leafletOutput("map2")
                              
                       ),
                       column(width = 2,
                              sidebarLayout(
                                sidebarPanel(width = 12,
                                             selectInput(inputId = "Year1",
                                                         label = "Top map year",
                                                         choices = c(2000, 2010, 2018),
                                                         selected = 2000),
                                             selectInput(inputId = "State1",
                                                         label = "Top map state",
                                                         choices = c(state.name, "US-Total"),
                                                         selected = "Illinois"),
                                             selectInput(inputId = "style1",
                                                         label = "Top map style",
                                                         choices = c("Hard boundries (B&W)", "Muted boundries", "Nat geo (Detailed)"),
                                                         selected = "Muted boundries"),
                                             selectInput(inputId = "Year2",
                                                         label = "Bottom map year",
                                                         choices = c(2000, 2010, 2018),
                                                         selected = 2018),
                                             selectInput(inputId = "State2",
                                                         label = "Bottom map state",
                                                         choices = c(state.name, "US-Total"),
                                                         selected = "Illinois"),
                                             selectInput(inputId = "style2",
                                                         label = "Bottom map style",
                                                         choices = c("Hard boundries (B&W)", "Muted boundries", "Nat geo (Detailed)"),
                                                         selected = "Muted boundries"),
                                             
                                             actionButton("reset_button1", "Reset top view"),
                                             actionButton("reset_button2", "Reset bot view")
                                ),
                                mainPanel()
                              )
                       )
                       
                     )
            )
          )
)
