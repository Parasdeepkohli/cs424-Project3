library(shiny)
library(leaflet)
library(mapview)
library(tigris)
library(ggplot2)

function(input, output, session){
  
  input_data <- read.csv("Energy_Usage_2010(m).csv")
  
  tracts <- tracts(state = 'IL', county = )
  
  output$NWS <- renderMapview({ mapview(zcol = 170316000000000) })
  
}