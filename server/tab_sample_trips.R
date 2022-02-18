#------------------------------------
# Sampled trips overviews and analysis
#------------------------------------
#
# 
# 
#   


##### Interactive Map - tracks of fished trips and fishing areas #####


# Create the map - leaflet 
output$trip_map <- renderLeaflet({
  leaflet() %>% 
    addProviderTiles(providers$Esri.OceanBasemap) %>% 
    setView(lng = 13.0000, lat = 54.800, zoom = 7)
})

# load data for the map to be displayed 
mapdata_trip <- (select(trip, tr_index, trip_number, year, vessel_name, vessel_sign, group_trip, days_at_sea)) %>%
  left_join(select(haul_fo, ha_index, tr_index, fo_start_date, fo_start_lat, fo_start_lon, fo_stop_lat, fo_stop_lon, fao_area, rectangle), by=c("tr_index")) %>%
    left_join(select(haul_gear, tr_index, ha_index, gear, metier_lvl_6, number_species, total_catch), by=c("tr_index"))


## subset according toi selection
mapdata_trip1 <- reactive({
  subset(mapdata_trip, year %in% input$slideryear_trip)
})

#### add data points to map
observe({
  colorBy <-  input$species_trip
  if(colorBy=="Demersal Fishery") {
    colorData <-  mapdata_trip1()$total_catch
    pal <- colorNumeric("viridis", colorData)
    radius <- (mapdata_trip1()$total_catch)
  
    }else if(colorBy=="Pelagic Fishery"){
      colorData <-  mapdata_trip1()$total_catch
      pal <- colorNumeric("viridis", colorData)
      radius <- (mapdata_trip1()$total_catch)
      
    }else if(colorBy=="Freshwater Fishery"){
      colorData <-  mapdata_trip1()$total_catch
      pal <- colorNumeric("viridis", colorData)
      radius <- (mapdata_trip1()$total_catch)
    }
  #####
  ###ACHTUNG stürzt noch ab, "ColorData wird nicht gefunden?
  #####
#  "Demersal fishery", "Pelagic fishery", "Freshwater fishery", "Harbor samples" 

#  leafletProxy("trip_map", data = mapdata_trip1()) %>% 
#  clearShapes() %>% 
#  addCircles(~fo_star_lon,~fo_start_lat, radius= radius, layerId=~trip_number,                  
#             stroke=FALSE,fillOpacity=0.7, fillColor=pal(colorData)) %>% 
#  addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
#            layerId="colorLegend")
})




##### in progress
#add ICES areas as shapes
# observe({
#  proxy<-leafletProxy("map", data = filter_df())
#  proxy%>%clearShapes()
#  if (input$rec){
#    proxy%>% addPolygons(data=ices.rect, weight=.4,fillOpacity = .1,color = 'grey',group = 'ices_squares',label = ~paste0(ICESNAME),highlight = highlightOptions(weight = 3, color = "red",
#                                                                                                                                                                 bringToFront = TRUE))
#  }
# })