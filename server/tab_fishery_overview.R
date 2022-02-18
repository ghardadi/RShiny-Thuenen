#------------------------------------
# Fishery overview
#------------------------------------
# tabPanel( 
#  title = "Flotte", 
#  fluidRow(
#    br(),

  
##### Interactive Map - fishing areas #####


# Create the map - leaflet 
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      setView(lng = 13.0000, lat = 54.800, zoom = 7)
  })

# summarize data for maps to reduce loading time
  mapdata <-landings %>% 
    group_by(jahr, fao_gebiet, rechteck, fischart) %>% 
    summarize(fangkg = sum(fangkg))

  # Filtering for Mapping 
  mapdata1 <- reactive({
    subset(mapdata, jahr %in% input$slideryear)
  }) 


##############  not connected to map yet, only reactive to year slider and drop-down!
##### Pie chart - sum of landings per species #####
##############
    sp_data_gp1 <- reactive({
    subset(landings, jahr %in% input$slideryear)
  })
# define data for reactive plot  
  sp_data_gp2_all <- reactive({
    sp_data_gp1() %>%
      group_by(group_fish) %>% 
      summarise(Totalkg = sum(fangkg))
  })
  sp_data_gp2_def <- reactive({
    sp_data_gp1() %>%  
      subset(group_fish %in% ("demersal species")) %>%
      group_by(fischart) %>% 
      summarise(Totalkg = sum(fangkg))
  })
  sp_data_gp2_spf <- reactive({
    sp_data_gp1() %>%
      subset(group_fish %in% ("pelagic species")) %>% 
      group_by(fischart) %>% 
      summarise(Totalkg = sum(fangkg))
  })
  m <- list(
    l = 30,
    r = 40,
    t = 160,
    b = 20
  ) 
# function to select data selection from above, based on drop-down menue  
  observe({
    P1 <- input$species_groups
    
    if(P1== "Total Landings kg") { 
      
      output$sp_piechart <- renderPlotly({   
        sp_data_gp2_all()  %>%
          plot_ly(labels = ~group_fish, values = ~Totalkg, 
                  textinfo ='label', textposition = "auto",  
                  type = 'pie',
                  marker = list(colors = c('#0c8e98','#06651f', '#F8D51A','#742416','#b8170f', '#f15f06'))) %>% ##8B2E62  
          layout(title = "Catch Composition (Species groups)",  showlegend = FALSE,
                 autosize = T, margin = m,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 plot_bgcolor='transparent',
                 paper_bgcolor='transparent',
                 font = list(color='#FFFFFF'))
      })
    }else if (P1 == "Demersal Species kg") { 
      ##### Loading gif #####
      output$sp_piechart <- renderPlotly({   
        Sys.sleep(1)
        sp_data_gp2_def() %>%
          plot_ly(labels = ~fischart, values = ~Totalkg, 
                  textinfo ='label', textposition = "auto",
                  type = 'pie',
                  marker = list(colors = c('#0c8e98','#06651f', '#F8D51A','#742416','#b8170f', '#f15f06'))) %>% 
          layout(title = "Catch Composition (Catch Kg)",  showlegend = FALSE,
                 autosize = T, margin = m,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 plot_bgcolor='transparent',
                 paper_bgcolor='transparent',
                 font = list(color='#FFFFFF'))
      })
    }else if (P1 == "Pelagic Species kg") { 
      ##### Loading gif #####
      output$sp_piechart <- renderPlotly({   
        Sys.sleep(1)
        sp_data_gp2_spf() %>%
          plot_ly(labels = ~fischart, values = ~Totalkg, 
                  textinfo ='label', textposition = "auto",
                  type = 'pie',
                  marker = list(colors = c('#0c8e98','#06651f', '#F8D51A','#742416','#b8170f', '#f15f06'))) %>% 
          layout(title = "Catch Composition (Catch Kg)",  showlegend = FALSE,
                 autosize = T, margin = m,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 plot_bgcolor='transparent',
                 paper_bgcolor='transparent',
                 font = list(color='#FFFFFF'))
      })
    }
    
  })  
  
       
##### Infographics  #####
  info_fleet <- sqldf("select jahr, fao_gebiet, group_fish, count(distinct(eunr)) as eunr, count(distinct(fischart)) as fischart, round(sum(fangkg/1000),1) as tons from landings group by jahr, group_fish")    
  info_fleet <- transform(info_fleet, fischart = as.integer(fischart), eunr = as.integer(eunr))
  sp_data_fleet <- reactive({subset(info_fleet, jahr %in% input$slideryear)})
 
## Box1
  totalvessel <- reactive({
    sp_data_fleet() %>%
      summarise(Totalvessel = sum(eunr))
  })
    totalvesseldef <- reactive({
    sp_data_fleet() %>%  
      subset(group_fish %in% ("demersal species")) %>%
      summarise(Totalvessel = sum(eunr))
  })
  totalvesselspf <- reactive({
    sp_data_fleet() %>%  
      subset(group_fish %in% ("pelagic species")) %>%
      summarise(Totalvessel = sum(eunr))
  }) 

## Box 1 output
observe({ 
  P1 <- input$species_groups 
   if(P1== "Total Landings kg") {
        output$box1 <- renderValueBox({valueBox(value = totalvessel(),subtitle="Operating fishing vessels", icon = icon("fas fa-ship"))
      }) 
   }else if (P1 == "Demersal Species kg") {
        output$box1 <- renderValueBox({valueBox(value = totalvesseldef(),subtitle="Operating demersal fishing vessels", icon = icon("fas fa-ship"))})
   }else if (P1 == "Pelagic Species kg") {
        output$box1 <- renderValueBox({valueBox(value = totalvesselspf(),subtitle="Operating pelagic fishing vessels", icon = icon("fas fa-ship"))})
    }
  })

## Box2  
totalfish <- reactive({
  sp_data_fleet() %>%
    summarise(Totalfish = sum(fischart))
})
totalfishdef <- reactive({
  sp_data_fleet() %>%  
    subset(group_fish %in% ("demersal species")) %>%
    summarise(Totalfish = sum(fischart))
})
totalfishspf <- reactive({
  sp_data_fleet() %>%  
    subset(group_fish %in% ("pelagic species")) %>%
    summarise(Totalfish = sum(fischart))
}) 

## Box 2 output
observe({ 
  P1 <- input$species_groups 
  if(P1== "Total Landings kg") {
    output$box2 <- renderValueBox({valueBox(value = totalfish(),subtitle="Species landed", icon = icon("fas fa-fish"))
    }) 
  }else if (P1 == "Demersal Species kg") {
    output$box2 <- renderValueBox({valueBox(value = totalfishdef(),subtitle="Demersal species landed", icon = icon("fas fa-fish"))})
  }else if (P1 == "Pelagic Species kg") {
    output$box2 <- renderValueBox({valueBox(value = totalfishspf(),subtitle="pelagic species", icon = icon("fas fa-fish"))})
  }
})

## Box3  
totalcatch <- reactive({
  sp_data_fleet() %>%
    summarise(Totalcatch = sum(tons))
})
totalcatchdef <- reactive({
  sp_data_fleet() %>%  
    subset(group_fish %in% ("demersal species")) %>%
    summarise(Totalcatch = sum(tons))
})
totalcatchspf <- reactive({
  sp_data_fleet() %>%  
    subset(group_fish %in% ("pelagic species")) %>%
    summarise(Totalcatch = sum(tons))
}) 

## Box 3 output
observe({ 
  P1 <- input$species_groups 
  if(P1== "Total Landings kg") {
    output$box3 <- renderValueBox({valueBox(value = totalcatch(),subtitle="total catch (tons)", icon = icon("fas fa-stroopwafel"))
    }) 
  }else if (P1 == "Demersal Species kg") {
    output$box3 <- renderValueBox({valueBox(value = totalcatchdef(),subtitle="total demersal catch (tons)", icon = icon("fas fa-stroopwafel"))})
  }else if (P1 == "Pelagic Species kg") {
    output$box3 <- renderValueBox({valueBox(value = totalcatchspf(),subtitle="total pelagic catch (tons)", icon = icon("fas fa-stroopwafel"))})
  }
})  



# -----------------------------------
# Add ICES Rectangles Shapefile
# -----------------------------------

#observe({
#  proxy<-leafletProxy("map", data = sp_data_gp1())
#  proxy%>%clearShapes()
#  if (input$rec){
#    proxy%>% addPolygons(data=ices.rect, weight=.4,fillOpacity = .1,color = 'grey',group = 'ices_squares',label = ~paste0(ICESNAME),highlight = highlightOptions(weight = 3, color = "red",
#                                                                                                                                                                 bringToFront = TRUE))
#  }
# })

# -----------------------------------
# This observe is responsible for maintaining the circles and legend,histogram and gauges
# according to the variables the user has chosen to map/display in the "station explorer" 
# -----------------------------------
#observe({
#  colorBy <-  input$catch_measure
#  if(colorBy=="Number of fish (per hour)") {
#    colorData <-  mapdata1()$RaisedNo
#    pal <- colorNumeric("viridis", colorData)
#    radius <- (mapdata1()$RaisedNo / max(mapdata1()$RaisedNo)) * 80000
#  }else if(colorBy=="Biomass (Kg)"){
#    colorData <-   mapdata1()$CatchKg
#    pal <-  colorNumeric("magma", colorData)
#    radius <- (mapdata1()$CatchKg/ max(mapdata1()$CatchKg)) *60000
#  }
  
#  leafletProxy("map", data = mapdata1()) %>% 
#    clearShapes() %>% 
#    addCircles(~Longitude,~Latitude, radius= radius, layerId=~PrimeStation,                  
#               stroke=FALSE,fillOpacity=0.7, fillColor=pal(colorData)) %>% 
#    addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
#              layerId="colorLegend")
# })
