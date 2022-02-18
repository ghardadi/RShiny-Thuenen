#------------------------------------
# Fishery landings 
#------------------------------------
# 
#  
#  
#    
##### Interactive Map - fishing areas #####
# Create the map - leaflet 
#output$map <- renderLeaflet({
#  leaflet() %>% 
#    addProviderTiles(providers$Esri.OceanBasemap) %>% 
#    setView(lng = 13.0000, lat = 54.800, zoom = 7)
#})

# Filtering for Mapping 
#mapdata1 <- reactive({
#  subset(mapdata, Year %in% input$slideryear)
#}) 

  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
#  datasetInput <- eventReactive(input$update, {
#    switch(input$dataset,
#           "rock" = rock,
#           "pressure" = pressure,
#           "cars" = cars)
#  }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
#  output$summary <- renderPrint({
#    dataset <- datasetInput()
#    summary(dataset)
#  })
  
  # Show the first "n" observations ----
#  output$view <- renderTable({
#    head(datasetInput(), n = isolate(input$obs))
#  })
  