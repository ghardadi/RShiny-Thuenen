#------------------------------------
# Sampling overview
#------------------------------------
# tabPanel( 
#  title = "Sampling", 
#  fluidRow(
#    br(),


##### Interactive ggplot for sample overviews #####
##############

sample_data_1 <- reactive({
  subset(trip, year %in% input$slideryear_sample)
})

# define data for reactive ggplot (trip overview) 
sample_data_2_all <- reactive({
  sample_data_1() # %>%
# sample_data_1()[order(sample_data_1()$start_date), ]  
})
sample_data_2_def <- reactive({
  sample_data_1() %>%  
# sample_data_1()[order(sample_data_1()$start_date), ] %>%
    subset(group_trip %in% ("demersal fishery"))
})
sample_data_2_spf <- reactive({
  sample_data_1() %>%
    subset(group_trip %in% ("pelagic fishery"))
})
sample_data_2_har <- reactive({
  sample_data_1() %>%
    subset(group_trip %in% ("harbor samples")) 
})
#m <- list(
#  l = 30,
#  r = 40,
#  t = 160,
#  b = 20
#) 

# Render the interactive ggplot  

observe({
  P1 <- input$sample_groups
  
  if(P1== "Total Samples") { 
    output$ggplot_sample <- renderPlot({
      ggplot(sample_data_2_all(), aes(x=as.Date(start_date), y=fct_reorder(as.character(trip_number), desc(start_date)), fill=group_trip)) +
        geom_point(aes(colour=group_trip, shape=group_trip), size=2.5) +
        facet_grid(quarter(start_date) ~ ., scales = "free_y",space= "free_y", switch = 'y') +
    #    geom_point(alpha= .4)+
        labs( x="Month", y="Trip number") +
        theme(legend.position = "bottom", legend.title=element_blank()) +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b%Y", expand = c(0.07, 0))
    })
  }else if (P1 == "Demersal Fishery") { 
    ##### Loading gif #####
    output$ggplot_sample <- renderPlot({
      ggplot(sample_data_2_def(), aes(x=as.Date(start_date), y=fct_reorder(as.character(trip_number), desc(start_date)), fill=group_trip)) +
        geom_point(aes(colour=group_trip, shape=group_trip), size=2.5) +
        facet_grid(quarter(start_date) ~ ., scales = "free_y",space= "free_y", switch = 'y') +
      #  geom_point(alpha= .4)+
        labs( x="Month", y="Trip number") +
        theme(legend.position = "bottom", legend.title=element_blank()) +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b%Y", expand = c(0.07, 0))
    })
  }else if (P1 == "Pelagic Fishery") { 
    ##### Loading gif #####
    output$ggplot_sample <- renderPlot({
      ggplot(sample_data_2_spf(), aes(x=as.Date(start_date), y=fct_reorder(as.character(trip_number), desc(start_date)), fill=group_trip)) +
        geom_point(aes(colour=group_trip, shape=group_trip), size=2.5) +
        facet_grid(quarter(start_date) ~ ., scales = "free_y", space= "free_y", switch = 'y') +
    #    geom_point(alpha= .4)+
        labs( x="Month", y="Trip number") +
        theme(legend.position = "bottom", legend.title=element_blank()) +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b%Y", expand = c(0.07, 0))
      
    })
  }else if (P1 == "Harbor Samples") { 
 
  ##### Loading gif #####
    output$ggplot_sample <- renderPlot({
      ggplot(sample_data_2_har(), aes(x=as.Date(start_date), y=fct_reorder(as.character(trip_number), desc(start_date)), fill=group_trip)) +
        geom_point(aes(colour=group_trip, shape=group_trip), size=2.5) +
        facet_grid(quarter(start_date) ~ ., scales = "free_y", space= "free_y", switch = 'y') +
    #    geom_point(alpha= .4)+
        labs( x="Month", y="Trip number") +
        theme(legend.position = "bottom", legend.title=element_blank()) +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b%Y", expand = c(0.07, 0))
      
    })
  }
  
})  



##### Infographics  #####
info_sample <- sqldf("select year, group_trip, count(distinct(eunr)) as eunr, count(distinct(trip_number)) as trips, sum(days_at_sea) as days_at_sea from trip group by year,group_trip")    
info_sample <- transform(info_sample, eunr = as.integer(eunr))
sp_data_sample <- reactive({subset(info_sample, year %in% input$slideryear_sample)})



## Box1
samplevessel <- reactive({
  sp_data_sample() %>%
    summarise(Samplevessel = sum(eunr))
})
samplevesseldef <- reactive({
  sp_data_sample() %>%  
    subset(group_trip %in% ("demersal fishery")) %>%
    summarise(Samplevessel = sum(eunr))
})
samplevesselspf <- reactive({
  sp_data_sample() %>%  
    subset(group_trip %in% ("pelagic fishery")) %>%
    summarise(Samplevessel = sum(eunr))
}) 
samplevesselhar <- reactive({
  sp_data_sample() %>%  
    subset(group_trip %in% ("harbor samples")) %>%
    summarise(Samplevessel = sum(eunr))
}) 

## Box 1 output
observe({ 
  P1 <- input$sample_groups 
  if(P1== "Total Samples") {
    output$box1_sample <- renderValueBox({valueBox(value = samplevessel(),subtitle="Unique fishing vessels sampled", icon = icon("fas fa-ship"))
    }) 
  }else if (P1 == "Demersal Fishery") {
    output$box1_sample <- renderValueBox({valueBox(value = samplevesseldef(),subtitle="Unique demersal fishing vessels sampled", icon = icon("fas fa-ship"))})
  }else if (P1 == "Pelagic Fishery") {
    output$box1_sample <- renderValueBox({valueBox(value = samplevesselspf(),subtitle="Unique pelagic fishing vessels sampled", icon = icon("fas fa-ship"))})
  }else if (P1 == "Harbor Samples") {
    output$box1_sample <- renderValueBox({valueBox(value = samplevesselhar(),subtitle="Unique fishing vessels sampled at Harborsite", icon = icon("fas fa-ship"))})
  }
  })


## Box2  
totaltrip <- reactive({
  sp_data_sample() %>%
    summarise(Totaltrip = sum(trips))
})
totaltripdef <- reactive({
  sp_data_sample() %>%  
    subset(group_trip %in% ("demersal fishery")) %>%
    summarise(Totaltrip = sum(trips))
})
totaltripspf <- reactive({
  sp_data_sample() %>%  
    subset(group_trip %in% ("pelagic fishery")) %>%
    summarise(Totaltrip = sum(trips))
}) 
totaltriphar <- reactive({
  sp_data_sample() %>%  
    subset(group_trip %in% ("harbor samples")) %>%
    summarise(Totaltrip = sum(trips))
}) 
## Box 2 output
observe({ 
  P1 <- input$sample_groups 
  if(P1== "Total Samples") {
    output$box2_sample <- renderValueBox({valueBox(value = totaltrip(),subtitle="Trips sampled", icon = icon("fas fa-binoculars"))
    }) 
  }else if (P1 == "Demersal Fishery") {
    output$box2_sample <- renderValueBox({valueBox(value = totaltripdef(),subtitle="Demersal trips sampled", icon = icon("fas fa-binoculars"))})
  }else if (P1 == "Pelagic Fishery") {
    output$box2_sample <- renderValueBox({valueBox(value = totaltripspf(),subtitle="Pelagic trips sampled", icon = icon("fas fa-binoculars"))})
  }else if (P1 == "Harbor Samples") {
    output$box2_sample <- renderValueBox({valueBox(value = totaltriphar(),subtitle="Trips sampled in harbors", icon = icon("fas fa-binoculars"))})
  }
})

## Box3  
totaldays <- reactive({
  sp_data_sample() %>%
    summarise(Totaldays = sum(days_at_sea))
})
totaldaysdef <- reactive({
  sp_data_sample() %>%  
    subset(group_trip %in% ("demersal fishery")) %>%
    summarise(Totaldays = sum(days_at_sea))
})
totaldaysspf <- reactive({
  sp_data_sample() %>%  
    subset(group_trip %in% ("pelagic fishery")) %>%
    summarise(Totaldays = sum(days_at_sea))
}) 
totaldayshar <- reactive({
  sp_data_sample() %>%  
    subset(group_trip %in% ("harbor samples")) %>%
    summarise(Totaldays = sum(days_at_sea))
})

## Box 3 output
observe({ 
  P1 <- input$sample_groups 
  if(P1== "Total Samples") {
    output$box3_sample <- renderValueBox({valueBox(value = totaldays(),subtitle="total days at Sea covered", icon = icon("fas fa-calendar-alt"))
    }) 
  }else if (P1 == "Demersal Fishery") {
    output$box3_sample <- renderValueBox({valueBox(value = totaldaysdef(),subtitle="demersal fishery days at Sea covered", icon = icon("fas fa-calendar-alt"))})
  }else if (P1 == "Pelagic Fishery") {
    output$box3_sample <- renderValueBox({valueBox(value = totaldaysspf(),subtitle="pelagic fishery days at Sea covered", icon = icon("fas fa-calendar-alt"))})
  }else if (P1 == "Harbor Samples") {
    output$box3_sample <- renderValueBox({valueBox(value = totaldayshar(),subtitle="Days at Sea covered in harbor samples", icon = icon("fas fa-calendar-alt"))})
  }
})  







