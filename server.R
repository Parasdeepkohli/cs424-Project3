library(shiny)
library(stringr)
library(stringr)
library(leaflet)
library(mapview)
library(tigris)
library(ggplot2)

function(input, output, session){
  
  source("Add-units.R")
  source("Add-commas.R")
  
  cook <- blocks('IL', county = 'Cook', year = 2010) # Loads the data frame containing cook county block codes, lat/lon and geometry
  input_data <- read.csv("Energy_Usage_2010(m).csv") # Loads in the census block data for Chicago
  input_data_NWS <- subset(input_data, COMMUNITY.AREA.NAME == "Near West Side")
  final_data_NWS <- merge(cook, input_data_NWS, by = 'GEOID10')
  
  NWS_Lat <- mean(as.numeric(final_data_NWS$INTPTLAT10))
  NWS_Lon <- mean(as.numeric(final_data_NWS$INTPTLON10))
  
  
  output$NWS <- renderLeaflet({ 
    
    
    legend_Name <- input$SourcesNWS
    if (input$TypeNWS == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$TypeNWS}
    
    if (input$SourcesNWS == 'Electricity' | input$SourcesNWS == 'Gas'){
    
      input_variable <- ifelse(input$SourcesNWS == 'Electricity', paste0("KWH.", toupper(input$MonthNWS), ".2010"),
                               paste0("THERM.", toupper(input$MonthNWS), ".2010"))
      
      final_data_NWS <- final_data_NWS[c("GEOID10", "BUILDING.TYPE", input_variable, "geometry", "INTPTLAT10", "INTPTLON10")]
      final_data_NWS <- subset(final_data_NWS, BUILDING.TYPE %in% input_type)
      final_data_NWS <- aggregate(final_data_NWS[input_variable], by = final_data_NWS['GEOID10'], FUN = sum)
      
      
      legend_Name <- paste0(legend_Name, ' in KWH')
      
    }
    
    else{
      
      input_variable <- str_replace_all(toupper(input$SourcesNWS), ' ', '.')
      final_data_NWS <- final_data_NWS[c("GEOID10", "BUILDING.TYPE", input_variable, "geometry", "INTPTLAT10","INTPTLON10")]
      final_data_NWS <- subset(final_data_NWS, BUILDING.TYPE %in% input_type)
      
      }
    
   
    
    mapview(final_data_NWS, zcol = input_variable, layer.name = legend_Name)@map
      
    
    })
  
  output$NWSElec <- renderPlot({
    
    if (input$TypeNWS == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$TypeNWS}
    
    final_data_NWS <- subset(final_data_NWS, BUILDING.TYPE %in% input_type)
    final_data_NWS <- final_data_NWS[c( "KWH.JANUARY.2010", "KWH.FEBRUARY.2010", "KWH.MARCH.2010", "KWH.APRIL.2010",
                              "KWH.MAY.2010", "KWH.JUNE.2010", "KWH.JULY.2010", "KWH.AUGUST.2010", "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010",
                              "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010")]
    
    final_data_NWS <- st_drop_geometry(final_data_NWS)
    colnames(final_data_NWS) <- c(month.abb)
    final_data_NWS <- data.frame(Month = names(final_data_NWS),Value=colSums(final_data_NWS))
    
    final_data_NWS$Month <- factor(final_data_NWS$Month, levels = c(month.abb, "Total"))
    
    ggplot(final_data_NWS, aes(x = Month, y = Value)) + geom_bar(stat = "identity") +
      scale_y_continuous(name = "Electricity used (KWh)", labels = addUnits) +
      ggtitle(paste0("Electricity usage in ", input$TypeNWS, " by month")) +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14))
    
    
  })
  
  output$NWSGas <- renderPlot({
    
    if (input$TypeNWS == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$TypeNWS}
    final_data_NWS <- subset(final_data_NWS, BUILDING.TYPE %in% input_type)
    final_data_NWS <- final_data_NWS[c( "THERM.JANUARY.2010", "THERM.FEBRUARY.2010", "THERM.MARCH.2010", "THERM.APRIL.2010",
                                "THERM.MAY.2010", "THERM.JUNE.2010", "THERM.JULY.2010", "THERM.AUGUST.2010", "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010",
                                "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010")]
    
    final_data_NWS <- st_drop_geometry(final_data_NWS)
    colnames(final_data_NWS) <- c(month.abb)
    final_data_NWS <- data.frame(Month = names(final_data_NWS),Value=colSums(final_data_NWS))
    
    final_data_NWS$Month <- factor(final_data_NWS$Month, levels = c(month.abb, "Total"))
    
    ggplot(final_data_NWS, aes(x = Month, y = Value)) + geom_bar(stat = "identity") +
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
      
      final_data_NWS <- subset(final_data_NWS, BUILDING.TYPE %in% input_type)
      final_data_NWS <- final_data_NWS[c( "BUILDING.TYPE", "KWH.JANUARY.2010", "KWH.FEBRUARY.2010", "KWH.MARCH.2010", "KWH.APRIL.2010",
                                  "KWH.MAY.2010", "KWH.JUNE.2010", "KWH.JULY.2010", "KWH.AUGUST.2010", "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010",
                                  "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010", "THERM.JANUARY.2010", "THERM.FEBRUARY.2010", "THERM.MARCH.2010", "THERM.APRIL.2010",
                                  "THERM.MAY.2010", "THERM.JUNE.2010", "THERM.JULY.2010", "THERM.AUGUST.2010", "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010",
                                  "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010")]
      
      final_data_NWS <- st_drop_geometry(final_data_NWS)
      final_data_NWS <- aggregate(.~BUILDING.TYPE, data = final_data_NWS, FUN = sum)
      
    })
  
  
  output$map1 <- renderLeaflet({ 
    
    input_data_1 <- subset(input_data, COMMUNITY.AREA.NAME == input$Communities1)
    final_data_1 <- merge(cook, input_data_1, by = 'GEOID10')
        
    legend_Name <- input$Sources1
    if (input$Type1 == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$Type1}
    
    if (input$Sources1 == 'Electricity' | input$Sources1 == 'Gas'){
      
      input_variable <- ifelse(input$Sources1 == 'Electricity', paste0("KWH.", toupper(input$Month1), ".2010"),
                               paste0("THERM.", toupper(input$Month1), ".2010"))
      
      final_data_1 <- final_data_1[c("GEOID10", "BUILDING.TYPE", input_variable, "geometry", "INTPTLAT10", "INTPTLON10")]
      final_data_1 <- subset(final_data_1, BUILDING.TYPE %in% input_type)
      final_data_1 <- aggregate(final_data_1[input_variable], by = final_data_1['GEOID10'], FUN = sum)
      
      
      legend_Name <- paste0(legend_Name, ' in KWH')
      
    }
    
    else{
      
      input_variable <- str_replace_all(toupper(input$Sources1), ' ', '.')
      final_data_1 <- final_data_1[c("GEOID10", "BUILDING.TYPE", input_variable, "geometry", "INTPTLAT10","INTPTLON10")]
      final_data_1 <- subset(final_data_1, BUILDING.TYPE %in% input_type)
      
    }
    
    mapview(final_data_1, zcol = input_variable, layer.name = legend_Name, col.regions=hcl.colors(palette = input$Colors, n = 4))@map
    
    
  })
  
  output$map2 <- renderLeaflet({ 
    
    input_data_2 <- subset(input_data, COMMUNITY.AREA.NAME == input$Communities2)
    final_data_2 <- merge(cook, input_data_2, by = 'GEOID10')
    
    legend_Name <- input$Sources2
    if (input$Type2 == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$Type2}
    
    if (input$Sources2 == 'Electricity' | input$Sources2 == 'Gas'){
      
      input_variable <- ifelse(input$Sources2 == 'Electricity', paste0("KWH.", toupper(input$Month2), ".2010"),
                               paste0("THERM.", toupper(input$Month2), ".2010"))
      
      final_data_2 <- final_data_2[c("GEOID10", "BUILDING.TYPE", input_variable, "geometry", "INTPTLAT10", "INTPTLON10")]
      final_data_2 <- subset(final_data_2, BUILDING.TYPE %in% input_type)
      final_data_2 <- aggregate(final_data_2[input_variable], by = final_data_2['GEOID10'], FUN = sum)
      
      
      legend_Name <- paste0(legend_Name, ' in KWH')
      
    }
    
    else{
      
      input_variable <- str_replace_all(toupper(input$Sources2), ' ', '.')
      final_data_2 <- final_data_2[c("GEOID10", "BUILDING.TYPE", input_variable, "geometry", "INTPTLAT10","INTPTLON10")]
      final_data_2 <- subset(final_data_2, BUILDING.TYPE %in% input_type)
      
    }
    
    
    
    mapview(final_data_2, zcol = input_variable, layer.name = legend_Name, col.regions=hcl.colors(palette = input$Colors, n = 4))@map
    
    
  })
  
  output$Elec1 <- renderPlot({
    
    input_data_1 <- subset(input_data, COMMUNITY.AREA.NAME == input$Communities1)
    final_data_1 <- merge(cook, input_data_1, by = 'GEOID10')
    
    if (input$Type1 == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$Type1}
    
    final_data_1 <- subset(final_data_1, BUILDING.TYPE %in% input_type)
    final_data_1 <- final_data_1[c( "KWH.JANUARY.2010", "KWH.FEBRUARY.2010", "KWH.MARCH.2010", "KWH.APRIL.2010",
                                        "KWH.MAY.2010", "KWH.JUNE.2010", "KWH.JULY.2010", "KWH.AUGUST.2010", "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010",
                                        "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010")]
    
    final_data_1 <- st_drop_geometry(final_data_1)
    colnames(final_data_1) <- c(month.abb)
    final_data_1 <- data.frame(Month = names(final_data_1),Value=colSums(final_data_1))
    
    final_data_1$Month <- factor(final_data_1$Month, levels = c(month.abb, "Total"))
    
    ggplot(final_data_1, aes(x = Month, y = Value)) + geom_bar(stat = "identity") +
      scale_y_continuous(name = "Electricity used (KWh)", labels = addUnits) +
      ggtitle(paste0("(Top) Electricity usage in ", input$Communities1, " by month")) +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14))
  })
  
  output$Gas1 <- renderPlot({
    
    input_data_1 <- subset(input_data, COMMUNITY.AREA.NAME == input$Communities1)
    final_data_1 <- merge(cook, input_data_1, by = 'GEOID10')
    
    if (input$Type1 == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$Type1}
    
    final_data_1 <- subset(final_data_1, BUILDING.TYPE %in% input_type)
    final_data_1 <- final_data_1[c( "THERM.JANUARY.2010", "THERM.FEBRUARY.2010", "THERM.MARCH.2010", "THERM.APRIL.2010",
                                    "THERM.MAY.2010", "THERM.JUNE.2010", "THERM.JULY.2010", "THERM.AUGUST.2010", "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010",
                                    "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010")]
    
    final_data_1 <- st_drop_geometry(final_data_1)
    colnames(final_data_1) <- c(month.abb)
    final_data_1 <- data.frame(Month = names(final_data_1),Value=colSums(final_data_1))
    
    final_data_1$Month <- factor(final_data_1$Month, levels = c(month.abb, "Total"))
    
    ggplot(final_data_1, aes(x = Month, y = Value)) + geom_bar(stat = "identity") +
      scale_y_continuous(name = "Gas used (KWh)", labels = addUnits) +
      ggtitle(paste0("(Top) Gas usage in ", input$Communities1, " by month")) +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14))
  })
  
  output$Elec2 <- renderPlot({
    
    input_data_2 <- subset(input_data, COMMUNITY.AREA.NAME == input$Communities2)
    final_data_2 <- merge(cook, input_data_2, by = 'GEOID10')
    
    if (input$Type2 == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$Type2}
    
    final_data_2 <- subset(final_data_2, BUILDING.TYPE %in% input_type)
    final_data_2 <- final_data_2[c( "KWH.JANUARY.2010", "KWH.FEBRUARY.2010", "KWH.MARCH.2010", "KWH.APRIL.2010",
                                    "KWH.MAY.2010", "KWH.JUNE.2010", "KWH.JULY.2010", "KWH.AUGUST.2010", "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010",
                                    "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010")]
    
    final_data_2 <- st_drop_geometry(final_data_2)
    colnames(final_data_2) <- c(month.abb)
    final_data_2 <- data.frame(Month = names(final_data_2),Value=colSums(final_data_2))
    
    final_data_2$Month <- factor(final_data_2$Month, levels = c(month.abb, "Total"))
    
    ggplot(final_data_2, aes(x = Month, y = Value)) + geom_bar(stat = "identity") +
      scale_y_continuous(name = "Electricity used (KWh)", labels = addUnits) +
      ggtitle(paste0("(Bottom) Electricity usage in ", input$Communities2, " by month")) +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14))
  })
  
  output$Gas2 <- renderPlot({
    
    input_data_2 <- subset(input_data, COMMUNITY.AREA.NAME == input$Communities2)
    final_data_2 <- merge(cook, input_data_2, by = 'GEOID10')
    
    if (input$Type2 == 'All'){input_type <- c("Commercial", "Residential", "Industrial")}
    else{input_type <- input$Type2}
    
    final_data_2 <- subset(final_data_2, BUILDING.TYPE %in% input_type)
    final_data_2 <- final_data_2[c( "THERM.JANUARY.2010", "THERM.FEBRUARY.2010", "THERM.MARCH.2010", "THERM.APRIL.2010",
                                    "THERM.MAY.2010", "THERM.JUNE.2010", "THERM.JULY.2010", "THERM.AUGUST.2010", "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010",
                                    "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010")]
    
    final_data_2 <- st_drop_geometry(final_data_2)
    colnames(final_data_2) <- c(month.abb)
    final_data_2 <- data.frame(Month = names(final_data_2),Value=colSums(final_data_2))
    
    final_data_2$Month <- factor(final_data_2$Month, levels = c(month.abb, "Total"))
    
    ggplot(final_data_2, aes(x = Month, y = Value)) + geom_bar(stat = "identity") +
      scale_y_continuous(name = "Gas used (KWh)", labels = addUnits) +
      ggtitle(paste0("(Bottom) Gas usage in ", input$Communities2, " by month")) +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14))
  })
  
  
  observe({
    input$reset_button
    leafletProxy("NWS") %>% setView(lat = NWS_Lat, lng = NWS_Lon, zoom = 13.5)
  })
  
  observe({
    
    input$reset_button1
    input_data_1 <- subset(input_data, COMMUNITY.AREA.NAME == input$Communities1)
    final_data_1 <- merge(cook, input_data_1, by = 'GEOID10')
    
    leafletProxy("map1") %>% setView(lat = mean(as.numeric(final_data_1$INTPTLAT10)), lng = mean(as.numeric(final_data_1$INTPTLON10)), zoom = 13.5)
  })
  
  observe({
    input$reset_button2
    input_data_2 <- subset(input_data, COMMUNITY.AREA.NAME == input$Communities2)
    final_data_2 <- merge(cook, input_data_2, by = 'GEOID10')
    
    leafletProxy("map2") %>% setView(lat = mean(as.numeric(final_data_2$INTPTLAT10)), lng = mean(as.numeric(final_data_2$INTPTLON10)), zoom = 13.5)
  })
  
}