library(shiny)
library(leaflet)

fluidPage(title="Raw power by plant (US)",
          
          navbarPage(
            
            title = "Navigation",
            id = "nav",
            position = "static-top",
            collapsible = TRUE,
            selected = "About",
            tabPanel(
              title = "About",
              tags$h1("Welcome to Project 2 of CS 424!", `style` = "text-align:center"),
              tags$h4("Created by: Parasdeep (Spring 2021)", `style` = "text-align:right"),
              tags$u(tags$h3("Purpose:", `style` = "font-weight:bold")),
              tags$ul(tags$li("Visualize the energy produced by plants across the state of Illinois (Illinois 2018)", `style` = "font-size:20px"),
                      tags$li("Compare the energy produced by plants between any two states (Compare states)",`style` = "font-size:20px"),
                      tags$li("Geographically visualize variance in energy production across the entire country (Compare states)", `style` = "font-size:20px")),
              tags$u(tags$h3("The Data:", `style` = "font-weight:bold")),
              tags$ul(tags$li("Three excel files detailing the energy produced by plants across the U.S.A in the years 2000, 2010 and 2018", `style` = "font-size:20px"),
                      tags$li("Each file provides the plant name, location (LAT and LON), and energy produced divided by energy source",`style` = "font-size:20px"),
                      tags$li("Please find the link to the data sources here:", tags$a(`href` = "https://www.epa.gov/egrid/download-data", "Source"), `style` = "font-size:20px")),
              tags$u(tags$h3("Notes and tips:", `style` = "font-weight:bold")),
              tags$ul(tags$li("Please use the navbar above to navigate the app", `style` = "font-size:20px"),
                      tags$li("Please be patient! The intial load time will take several seconds, but the app will run smoothly afterwards", `style` = "font-size:20px"),
                      tags$li("The source US-Total will transform the map into the entire country. It is at the bottom of both state inputs in Compare States", `style` = "font-size:20px"),
                      tags$li("The minimum and maximum sliders affect both zones in compare states", `style` = "font-size:20px"),
                      tags$li("The application will either show a blank slate, or an error, in case your filters do not match any data points", `style` = "font-size:20px"))
              
            ),
            tabPanel("Illinois 2018",
                     sidebarLayout(
                       sidebarPanel(
                         width = 2,
                         tags$head(tags$style("#mapIL{height:90vh !important;}")),
                         checkboxGroupInput(
                           inputId = "SourcesIL", 
                           label = "Pick the sources", 
                           choices = c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Other", "Solar", "Wind", "Geothermal")
                         ),
                         checkboxInput(inputId = 'allIL', label = 'All', value = TRUE),
                         actionButton(inputId = 'renewableIL', label = 'Renewable'),
                         actionButton(inputId = 'nonrenewIL', label = 'Non-Renew'),
                         br(),
                         actionButton("reset_button", "Reset view")
                       ),
                       mainPanel(
                         width = 10,
                         title = "Illinois 2018",
                         leafletOutput("mapIL")
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
