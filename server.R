library(shiny)
library(sf)
library(stringr)
library(leaflet)
library(dplyr)
library(mapview)
library(tigris)
library(tidycensus)
library(ggplot2)

function(input, output, session){
  
  source("Add-units.R")
  
  cook <- blocks('IL', county = 'Cook', year = 2010) # Loads the data frame containing cook county block codes, lat/lon and geometry
  input_data <- read.csv("Energy_Usage_2010(m).csv") # Loads in the census block data for Chicago
  input_data <- subset(input_data, COMMUNITY.AREA.NAME == "Near West Side")
  final_data <- merge(cook, input_data, by = 'GEOID10')
  
  NWS_Lat <- mean(as.numeric(final_data$INTPTLAT10))
  NWS_Lon <- mean(as.numeric(final_data$INTPTLON10))
  
  
  output$NWS <- renderLeaflet({ 
    
    
    legend_Name <- input$SourcesNWS
    if (input$TypeNWS == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$TypeNWS}
    
    if (input$SourcesNWS == 'Electricity' | input$SourcesNWS == 'Gas'){
    
      input_variable <- ifelse(input$SourcesNWS == 'Electricity', paste0("KWH.", toupper(input$MonthNWS), ".2010"),
                               paste0("THERM.", toupper(input$MonthNWS), ".2010"))
      
      final_data <- final_data[c("GEOID10", "BUILDING.TYPE", input_variable, "geometry", "INTPTLAT10", "INTPTLON10")]
      final_data <- subset(final_data, BUILDING.TYPE %in% input_type)
      final_data <- aggregate(final_data[input_variable], by = final_data['GEOID10'], FUN = sum)
      
      
      legend_Name <- paste0(legend_Name, ' in KWH')
      
    }
    
    else{
      
      input_variable <- str_replace_all(toupper(input$SourcesNWS), ' ', '.')
      final_data <- final_data[c("GEOID10", "BUILDING.TYPE", input_variable, "geometry", "INTPTLAT10","INTPTLON10")]
      final_data <- subset(final_data, BUILDING.TYPE %in% input_type)
      
      }
    
   
    
    mapview(final_data, zcol = input_variable, layer.name = legend_Name)@map
      
    
    })
  
  output$NWSElec <- renderPlot({
    
    if (input$TypeNWS == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$TypeNWS}
    
    final_data <- subset(final_data, BUILDING.TYPE %in% input_type)
    final_data <- final_data[c( "KWH.JANUARY.2010", "KWH.FEBRUARY.2010", "KWH.MARCH.2010", "KWH.APRIL.2010",
                              "KWH.MAY.2010", "KWH.JUNE.2010", "KWH.JULY.2010", "KWH.AUGUST.2010", "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010",
                              "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010")]
    
    final_data <- st_drop_geometry(final_data)
    colnames(final_data) <- c(month.abb)
    final_data <- data.frame(Month = names(final_data),Value=colSums(final_data))
    
    final_data$Month <- factor(final_data$Month, levels = c(month.abb, "Total"))
    
    ggplot(final_data, aes(x = Month, y = Value)) + geom_bar(stat = "identity") +
      scale_y_continuous(name = "Electricity used (KWh)", labels = addUnits) +
      ggtitle(paste0("Electricity usage in ", input$TypeNWS, " by month")) +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14))
    
    
  })
  
  output$NWSGas <- renderPlot({
    
    if (input$TypeNWS == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$TypeNWS}
    final_data <- subset(final_data, BUILDING.TYPE %in% input_type)
    final_data <- final_data[c( "THERM.JANUARY.2010", "THERM.FEBRUARY.2010", "THERM.MARCH.2010", "THERM.APRIL.2010",
                                "THERM.MAY.2010", "THERM.JUNE.2010", "THERM.JULY.2010", "THERM.AUGUST.2010", "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010",
                                "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010")]
    
    final_data <- st_drop_geometry(final_data)
    colnames(final_data) <- c(month.abb)
    final_data <- data.frame(Month = names(final_data),Value=colSums(final_data))
    
    final_data$Month <- factor(final_data$Month, levels = c(month.abb, "Total"))
    
    ggplot(final_data, aes(x = Month, y = Value)) + geom_bar(stat = "identity") +
      scale_y_continuous(name = "Gas used (KWh)", labels = addUnits) +
      ggtitle(paste0("Gas usage in ", input$TypeNWS, " by month")) +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14))
    
    
  })
  
  output$NWSTable <- DT::renderDataTable(
    class = 'cell-border stripe',
    filter = 'top',
    options = list(columnDefs = list(list(className = 'dt-right'))),
    {
      
      if (input$TypeNWS == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
      else{input_type <- input$TypeNWS}
      
      final_data <- subset(final_data, BUILDING.TYPE %in% input_type)
      final_data <- final_data[c( "BUILDING.TYPE", "KWH.JANUARY.2010", "KWH.FEBRUARY.2010", "KWH.MARCH.2010", "KWH.APRIL.2010",
                                  "KWH.MAY.2010", "KWH.JUNE.2010", "KWH.JULY.2010", "KWH.AUGUST.2010", "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010",
                                  "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010", "THERM.JANUARY.2010", "THERM.FEBRUARY.2010", "THERM.MARCH.2010", "THERM.APRIL.2010",
                                  "THERM.MAY.2010", "THERM.JUNE.2010", "THERM.JULY.2010", "THERM.AUGUST.2010", "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010",
                                  "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010")]
      
      final_data <- st_drop_geometry(final_data)
      final_data <- aggregate(.~BUILDING.TYPE, data = final_data, FUN = sum)
    })
  
  
  observe({
    input$reset_button
    leafletProxy("NWS") %>% setView(lat = NWS_Lat, lng = NWS_Lon, zoom = 13.5)
  })
  
}