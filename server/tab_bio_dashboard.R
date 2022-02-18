#------------------------------------
# Dashboard for species overview
#------------------------------------
#library(shiny)
#library(tidyr)



##### Infographics  #####
select_year <- reactive({
  subset(haul_fo, year==input$year,
         select=c(year, quarter, ha_index,
                  fao_area, rectangle))
})

gear_data <- reactive({
  gear <- subset(haul_gear, ha_index %in% select_year()$ha_index,
                 select=c(ha_index,gear))
  gear %>%
    left_join(select_year(), by=c("ha_index"))
})

output$GearFilter <- renderUI({
  gearlist <- unique(gear_data()$gear)
  gearlist2 = factor(append("All", as.character(gearlist)))
  selectInput(inputId="gearselect", label="Select gear type",
              choices=gearlist2, selected= "All")
})

select_gear <- reactive({
  if (input$gearselect == "All"|| is.null(input$gearselect)||
      input$biooptionselection=="None"){
    gear_data()
  } else {
    filter(gear_data(), gear==input$gearselect)
  }
})

select_quarter <- reactive({
  if (input$quarter=="All"|| is.null(input$quarter)){
    select_gear()
  } else {
    filter(select_gear(), quarter==input$quarter)
  }
})

select_area <- reactive({
  if (input$Id=="FAO Area") {
    filter(select_quarter(), fao_area %in% input$subselect_fao)
  } else {
    filter(select_quarter(), rectangle %in% input$subselect_rect)
  }
})

select_weight <- reactive({
  we_subset <- subset(sample_weight, ha_index %in% select_area()$ha_index)

  we_subset %>%
    left_join(select_area(), by = c("ha_index"))
})

select_species <- reactive({
  if (input$species=="All"){
    select_weight()
  }
  else {
    filter(select_weight(), species == input$species)
  }
})

select_length <- reactive({
  le_subset <- subset(sample_length, we_index %in% select_species()$we_index)
  le_subset %>%
    left_join(select_species(), by=c("we_index"))
})

select_bio <- reactive({
  clean_sample <- sample_bio[,2:5] %>% spread(parameter, value)
  names(clean_sample)[3:8] <- c("Length","Weight","Sex","Maturity","Age","Readability")

  clean_sample$Length <- lapply(clean_sample$Length, as.numeric)
  clean_sample$Weight <- lapply(clean_sample$Weight, as.numeric)

  bi_subset <- subset(clean_sample, le_index %in% select_length()$le_index)

  bi_subset %>%
    left_join(select_length(), by = c("le_index")) %>%
    arrange("Length")
})

# Creating Sub Area filter based on the full data
output$spatialops.w <- renderUI({
  if(input$fishtab == 'A'){
    if(input$Id=="FAO Area"){
      pickerInput(
        inputId = "subselect_fao",
        label = "FAO Area",
        choices = str_sort(as.character(
          unique(select_quarter()$fao_area)),numeric = TRUE),
        selected = str_sort(as.character(
          unique(select_quarter()$fao_area)),numeric = TRUE),
        options = list(
          `actions-box` = TRUE
        ),
        multiple = TRUE
      )}
    else{
      pickerInput(
        inputId = "subselect_rect",
        label = "Rectangle",
        choices = str_sort(as.character(
          unique(select_quarter()$rectangle)),numeric = TRUE),
        selected=str_sort(as.character(
          unique(select_quarter()$rectangle)),numeric = TRUE),
        options = list(
          `actions-box` = TRUE
        ),
        multiple = TRUE
      )}
  }
})

output$spatialops.a <- renderUI({
  if(input$fishtab == 'B'){
    if(input$Id.a=="FAO Area"){
      pickerInput(
        inputId = "subselect_fao",
        label = "FAO Area",
        choices = str_sort(as.character(
          unique(select_quarter()$fao_area)),numeric = TRUE),
        selected = str_sort(as.character(
          unique(select_quarter()$fao_area)),numeric = TRUE),
        options = list(
          `actions-box` = TRUE
        ),
        multiple = TRUE
      )}
    else{
      pickerInput(
        inputId = "subselect_rect",
        label = "Rectangle",
        choices = str_sort(as.character(
          unique(select_quarter()$rectangle)),numeric = TRUE),
        selected=str_sort(as.character(
          unique(select_quarter()$rectangle)),numeric = TRUE),
        options = list(
          `actions-box` = TRUE
        ),
        multiple = TRUE
      )}
  }
})

##### Histogram #######
output$bio_lw<- renderPlotly({
  if(input$biooptionselection=="Sex"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Weight", "Sex") %>%
        mutate(Length = as.numeric(Length),
               Weight = as.numeric(Weight)) %>%
        arrange(Length)
    })
    
    reg1 <- reactive({
      length <- unlist(select_bio_fin()$Length)
      weight <- unlist(select_bio_fin()$Weight)
      reg <- NA
      try(reg <- lm(log(weight) ~ log(length)),
          silent = TRUE)
      
      return (reg)
    })
    
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      
      try(x <- data.frame(Length = unique(length)),
          silent = TRUE)
      try(x$Weight <- exp(reg1()$coef[1]) * x$Length ** (reg1()$coef[2]),
          silent = TRUE)
      
      return (x)
    })
    
    p <- plot_ly() %>%
      add_trace(x = select_bio_fin()$Length, y = select_bio_fin()$Weight,
                type = 'scatter', color = select_bio_fin()$Sex, colors = "Set1",
                mode = 'markers', marker = list(opacity = 0.5), hoverinfo='text',
                text = paste("Length:", select_bio_fin()$Length,
                             "cm<br>Weight:", select_bio_fin()$Weight,
                             "g<br>Sex:", select_bio_fin()$Sex)) %>%
      add_trace(x = fit()$Length, y = fit()$Weight, type = 'scatter',
                mode = "lines", colors = "Spectral", hoverinfo='none') %>%
      layout(title=paste(input$species, "Length vs Weight (points coloured by sex)"),
             margin=(list(t=80)), showlegend = FALSE)
    p$elementId <- NULL
    p
    
  }else if(input$biooptionselection=="Age"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Weight", "Age") %>%
        mutate(Length = as.numeric(Length),
               Weight = as.numeric(Weight)) %>%
        arrange(Length)
    })
    
    reg1 <- reactive({
      length <- unlist(select_bio_fin()$Length)
      weight <- unlist(select_bio_fin()$Weight)
      reg <- NA
      try(reg <- lm(log(weight) ~ log(length)),
          silent = TRUE)
      
      return (reg)
    })
    
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      
      try(x <- data.frame(Length = unique(length)),
          silent = TRUE)
      try(x$Weight <- exp(reg1()$coef[1]) * x$Length ** (reg1()$coef[2]),
          silent = TRUE)
      
      return (x)
    })
    
    p <- plot_ly() %>%
      add_trace(x = select_bio_fin()$Length, y = select_bio_fin()$Weight,
                type = 'scatter', color = select_bio_fin()$Age, colors = "Spectral",
                mode = 'markers', marker = list(opacity = 0.5), hoverinfo='text',
                text = paste("Length:", select_bio_fin()$Length,
                             "cm<br>Weight:", select_bio_fin()$Weight,
                             "g<br>Age:", select_bio_fin()$Age)) %>%
      add_trace(x = fit()$Length, y = fit()$Weight, type = 'scatter',
                mode = "lines", colors = "Spectral", hoverinfo='none') %>%
      layout(title=paste(input$species, "Length vs Weight (points coloured by age)"),
             margin=(list(t=80)), showlegend = FALSE)
    p$elementId <- NULL
    p
    
  }else if(input$biooptionselection=="Sample Type"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Weight", "catch_category") %>%
        mutate(Length = as.numeric(Length),
               Weight = as.numeric(Weight)) %>%
        arrange(Length)
    })
    
    reg1 <- reactive({
      length <- unlist(select_bio_fin()$Length)
      weight <- unlist(select_bio_fin()$Weight)
      reg <- NA
      try(reg <- lm(log(weight) ~ log(length)),
          silent = TRUE)
      
      return (reg)
    })
    
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      
      try(x <- data.frame(Length = unique(length)),
          silent = TRUE)
      try(x$Weight <- exp(reg1()$coef[1]) * x$Length ** (reg1()$coef[2]),
          silent = TRUE)
      
      return (x)
    })
    
    p <- plot_ly() %>%
      add_trace(x = select_bio_fin()$Length, y = select_bio_fin()$Weight,
                type = 'scatter', color = select_bio_fin()$catch_category,
                colors = c('D'='red','L'='lightgreen'),
                mode = 'markers', marker = list(opacity = 0.5), hoverinfo='text',
                text = paste("Length:", select_bio_fin()$Length,
                             "cm<br>Weight:", select_bio_fin()$Weight,
                             "g<br>Sample type:", select_bio_fin()$catch_category)) %>%
      add_trace(x = fit()$Length, y = fit()$Weight, type = 'scatter',
                mode = "lines", colors = "Spectral", hoverinfo='none') %>%
      layout(title=paste(input$species, "Length vs Weight (points coloured by sample type)"),
             margin=(list(t=80)), showlegend = FALSE)
    p$elementId <- NULL
    p
    
  }else if(input$biooptionselection=="Gear"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Weight", "gear") %>%
        mutate(Length = as.numeric(Length),
               Weight = as.numeric(Weight)) %>%
        arrange(Length)
    })
    
    reg1 <- reactive({
      length <- unlist(select_bio_fin()$Length)
      weight <- unlist(select_bio_fin()$Weight)
      reg <- NA
      try(reg <- lm(log(weight) ~ log(length)),
          silent = TRUE)
      
      return (reg)
    })
    
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      
      try(x <- data.frame(Length = unique(length)),
          silent = TRUE)
      try(x$Weight <- exp(reg1()$coef[1]) * x$Length ** (reg1()$coef[2]),
          silent = TRUE)
      
      return (x)
    })
    
    p <- plot_ly() %>%
      add_trace(x = select_bio_fin()$Length, y = select_bio_fin()$Weight,
                type = 'scatter', color = select_bio_fin()$gear, colors = "Set1",
                mode = 'markers', marker = list(opacity = 0.5), hoverinfo='text',
                text = paste("Length:", select_bio_fin()$Length,
                             "cm<br>Weight:", select_bio_fin()$Weight,
                             "g<br>Gear type:", select_bio_fin()$gear)) %>%
      add_trace(x = fit()$Length, y = fit()$Weight, type = 'scatter',
                mode = "lines", colors = "Spectral", hoverinfo='none') %>%
      layout(title=paste(input$species, "Length vs Weight"),
             margin=(list(t=80)), showlegend = FALSE)
    p$elementId <- NULL
    p
  }
  else{
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Weight") %>%
        mutate(Length = as.numeric(Length),
               Weight = as.numeric(Weight)) %>%
        arrange(Length)
      })
    
    reg1 <- reactive({
      length <- unlist(select_bio_fin()$Length)
      weight <- unlist(select_bio_fin()$Weight)
      reg <- NA
      try(reg <- lm(log(weight) ~ log(length)),
          silent = TRUE)
      
      return (reg)
      })
    
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      
      try(x <- data.frame(Length = unique(length)),
          silent = TRUE)
      try(x$Weight <- exp(reg1()$coef[1]) * x$Length ** (reg1()$coef[2]),
          silent = TRUE)
      
      return (x)
    })
    
    p <- plot_ly() %>%
      add_trace(x = select_bio_fin()$Length, y = select_bio_fin()$Weight,
                type = 'scatter', color = select_bio_fin()$Weight, colors = "viridis",
                mode = 'markers', marker = list(opacity = 0.5), hoverinfo='text',
                text = paste("Length:", select_bio_fin()$Length,
                             "cm<br>Weight:", select_bio_fin()$Weight, "g")) %>%
      add_trace(x = fit()$Length, y = fit()$Weight, type = 'scatter',
                mode = "lines", colors = "Spectral", hoverinfo='none') %>%
      layout(title=paste(input$species, "Length vs Weight"),
             margin=(list(t=80)), showlegend = FALSE)
    p$elementId <- NULL
    p
  }
})

output$ageVB<- renderPlotly({
  if(input$ageoptionselection=="Sex"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Age", "Sex") %>%
        mutate(Length = as.numeric(Length),
               Age = as.numeric(Age)) %>%
        arrange(Age, Length)
    })

    if ("COD" %in% select_bio_fin()$species){
      select_bio_reg <- reactive({
        select_bio_fin() %>%
          subset(Age < 7)
      })
    } else {
      select_bio_reg <- reactive({
        select_bio_fin()
      })
    }

    reg1 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      try(reg <- vbStarts(length ~ age, data=vonBertalanffy))
      return (reg)
    })

    reg2 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      vb <- vbFuns(param="vonBertalanffy")
      try(reg <- nls(length ~ vb(age,Linf,K,t0),
                     data=vonBertalanffy, start=reg1()))
      return (reg)
    })
    print(reg2())
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      age <- unlist(select_bio_fin()$Age)

      try(x <- data.frame(age = age, Age = age),
          silent = TRUE)
      try(x$Length <- predict(reg2(), x))

      return (x)
    })

    fig <- plot_ly(type = 'table',
                   header = list(
                     values = c('<b>Linf</b>', '<b>K</b>','<b>t0</b>'),
                     line = list(color = '#506784'),
                     fill = list(color = '#119DFF'),
                     align = c('left','center'),
                     font = list(color = 'white', size = 12)),
                   cells = list(
                     values= rbind(round(coef(reg2())[1], 2),
                                   round(coef(reg2())[2], 5),
                                   round(coef(reg2())[3], 2))),
                   align = c('left', 'center'),
                   font = list(color = c('#506784'), size = 12)
    )
    
    fig

  }else if(input$ageoptionselection=="Weight"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Age", "Weight") %>%
        mutate(Length = as.numeric(Length),
               Age = as.numeric(Age)) %>%
        arrange(Age, Length)
    })

    if ("COD" %in% select_bio_fin()$species){
      select_bio_reg <- reactive({
        select_bio_fin() %>%
          subset(Age < 7)
      })
    } else {
      select_bio_reg <- reactive({
        select_bio_fin()
      })
    }

    reg1 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      try(reg <- vbStarts(length ~ age, data=vonBertalanffy))
      return (reg)
    })

    reg2 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      vb <- vbFuns(param="vonBertalanffy")
      try(reg <- nls(length ~ vb(age,Linf,K,t0),
                     data=vonBertalanffy, start=reg1()))
      return (reg)
    })
    print(reg2())
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      age <- unlist(select_bio_fin()$Age)

      try(x <- data.frame(age = age, Age = age),
          silent = TRUE)
      try(x$Length <- predict(reg2(), x))

      return (x)
    })
    
    fig <- plot_ly(type = 'table',
                   header = list(
                     values = c('<b>Linf</b>', '<b>K</b>','<b>t0</b>'),
                     line = list(color = '#506784'),
                     fill = list(color = '#119DFF'),
                     align = c('left','center'),
                     font = list(color = 'white', size = 12)),
                   cells = list(
                     values= rbind(round(coef(reg2())[1], 2),
                                   round(coef(reg2())[2], 5),
                                   round(coef(reg2())[3], 2))),
                   align = c('left', 'center'),
                   font = list(color = c('#506784'), size = 12)
    )
    
    fig

  }else if(input$ageoptionselection=="Sample Type"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Age", "catch_category") %>%
        mutate(Length = as.numeric(Length),
               Age = as.numeric(Age)) %>%
        arrange(Age, Length)
    })

    if ("COD" %in% select_bio_fin()$species){
      select_bio_reg <- reactive({
        select_bio_fin() %>%
          subset(Age < 7)
      })
    } else {
      select_bio_reg <- reactive({
        select_bio_fin()
      })
    }

    reg1 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      try(reg <- vbStarts(length ~ age, data=vonBertalanffy))
      return (reg)
    })

    reg2 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      vb <- vbFuns(param="vonBertalanffy")
      try(reg <- nls(length ~ vb(age,Linf,K,t0),
                     data=vonBertalanffy, start=reg1()))
      return (reg)
    })
    print(reg2())
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      age <- unlist(select_bio_fin()$Age)

      try(x <- data.frame(age = age, Age = age),
          silent = TRUE)
      try(x$Length <- predict(reg2(), x))

      return (x)
    })

    fig <- plot_ly(type = 'table',
                   header = list(
                     values = c('<b>Linf</b>', '<b>K</b>','<b>t0</b>'),
                     line = list(color = '#506784'),
                     fill = list(color = '#119DFF'),
                     align = c('left','center'),
                     font = list(color = 'white', size = 12)),
                   cells = list(
                     values= rbind(round(coef(reg2())[1], 2),
                                   round(coef(reg2())[2], 5),
                                   round(coef(reg2())[3], 2))),
                   align = c('left', 'center'),
                   font = list(color = c('#506784'), size = 12)
    )
    
    fig

  }else if(input$ageoptionselection=="Gear"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Age", "gear") %>%
        mutate(Length = as.numeric(Length),
               Age = as.numeric(Age)) %>%
        arrange(Age, Length)
    })

    if ("COD" %in% select_bio_fin()$species){
      select_bio_reg <- reactive({
        select_bio_fin() %>%
          subset(Age < 7)
      })
    } else {
      select_bio_reg <- reactive({
        select_bio_fin()
      })
    }

    reg1 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      try(reg <- vbStarts(length ~ age, data=vonBertalanffy))
      return (reg)
    })

    reg2 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      vb <- vbFuns(param="vonBertalanffy")
      try(reg <- nls(length ~ vb(age,Linf,K,t0),
                     data=vonBertalanffy, start=reg1()))
      return (reg)
    })
    print(reg2())
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      age <- unlist(select_bio_fin()$Age)

      try(x <- data.frame(age = age, Age = age),
          silent = TRUE)
      try(x$Length <- predict(reg2(), x))

      return (x)
    })

    fig <- plot_ly(type = 'table',
                   header = list(
                     values = c('<b>Linf</b>', '<b>K</b>','<b>t0</b>'),
                     line = list(color = '#506784'),
                     fill = list(color = '#119DFF'),
                     align = c('left','center'),
                     font = list(color = 'white', size = 12)),
                   cells = list(
                     values= rbind(round(coef(reg2())[1], 2),
                                   round(coef(reg2())[2], 5),
                                   round(coef(reg2())[3], 2))),
                   align = c('left', 'center'),
                   font = list(color = c('#506784'), size = 12)
    )
    
    fig

  }
  else{
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Age") %>%
        mutate(Length = as.numeric(Length),
               Age = as.numeric(Age)) %>%
        arrange(Age, Length)
    })

    if ("COD" %in% select_bio_fin()$species){
      select_bio_reg <- reactive({
        select_bio_fin() %>%
          subset(Age < 7)
      })
    } else {
      select_bio_reg <- reactive({
        select_bio_fin()
      })
    }

    reg1 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      try(reg <- vbStarts(length ~ age, data=vonBertalanffy))
      return (reg)
    })

    reg2 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      vb <- vbFuns(param="vonBertalanffy")
      try(reg <- nls(length ~ vb(age,Linf,K,t0),
                     data=vonBertalanffy, start=reg1()))
      return (reg)
    })
    print(reg2())
    
    fig <- plot_ly(type = 'table',
                   header = list(
                     values = c('<b>Linf</b>', '<b>K</b>','<b>t0</b>'),
                     line = list(color = '#506784'),
                     fill = list(color = '#119DFF'),
                     align = c('left','center'),
                     font = list(color = 'white', size = 12)),
                   cells = list(
                     values= rbind(round(coef(reg2())[1], 2),
                                   round(coef(reg2())[2], 5),
                                   round(coef(reg2())[3], 2))),
                   align = c('left', 'center'),
                   font = list(color = c('#506784'), size = 12)
    )
    
    fig

  }
})

output$bio_la<- renderPlotly({
  if(input$ageoptionselection=="Sex"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Age", "Sex") %>%
        mutate(Length = as.numeric(Length),
               Age = as.numeric(Age)) %>%
        arrange(Age, Length)
    })
    
    if ("COD" %in% select_bio_fin()$species){
      select_bio_reg <- reactive({
        select_bio_fin() %>%
          subset(Age < 7)
      })
    } else {
      select_bio_reg <- reactive({
        select_bio_fin()
      })
    }
    
    reg1 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      try(reg <- vbStarts(length ~ age, data=vonBertalanffy))
      return (reg)
    })
    
    reg2 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      vb <- vbFuns(param="vonBertalanffy")
      try(reg <- nls(length ~ vb(age,Linf,K,t0),
                     data=vonBertalanffy, start=reg1()))
      return (reg)
    })
    print(reg2())
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      age <- unlist(select_bio_fin()$Age)
      
      try(x <- data.frame(age = age, Age = age),
          silent = TRUE) 
      try(x$Length <- predict(reg2(), x))
      
      return (x)
    })
    
    p <- plot_ly() %>%
      add_trace(x = select_bio_fin()$Age, y = select_bio_fin()$Length,
                type = 'scatter', color = select_bio_fin()$Sex, colors = "Set1",
                mode = 'markers', marker = list(opacity = 0.5), hoverinfo='text',
                text = paste("Age:", select_bio_fin()$Age,
                             "years<br>Length:", select_bio_fin()$Length,
                             "cm<br>Sex:", select_bio_fin()$Sex)) %>%
      add_trace(x = fit()$Age, y = fit()$Length, type = 'scatter',
                mode = "lines", colors = "Spectral", hoverinfo='none') %>%
      layout(title=paste(input$species, "Age vs Length (points coloured by sex)"),
             x = "Age (years)", y = "Length (cm)",
             margin=(list(t=80)), showlegend = FALSE)
    p$elementId <- NULL
    p
    
  }else if(input$ageoptionselection=="Weight"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Age", "Weight") %>%
        mutate(Length = as.numeric(Length),
               Age = as.numeric(Age)) %>%
        arrange(Age, Length)
    })
    
    if ("COD" %in% select_bio_fin()$species){
      select_bio_reg <- reactive({
        select_bio_fin() %>%
          subset(Age < 7)
      })
    } else {
      select_bio_reg <- reactive({
        select_bio_fin()
      })
    }
    
    reg1 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      try(reg <- vbStarts(length ~ age, data=vonBertalanffy))
      return (reg)
    })
    
    reg2 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      vb <- vbFuns(param="vonBertalanffy")
      try(reg <- nls(length ~ vb(age,Linf,K,t0),
                     data=vonBertalanffy, start=reg1()))
      return (reg)
    })
    print(reg2())
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      age <- unlist(select_bio_fin()$Age)
      
      try(x <- data.frame(age = age, Age = age),
          silent = TRUE) 
      try(x$Length <- predict(reg2(), x))
      
      return (x)
    })
    
    p <- plot_ly() %>%
      add_trace(x = select_bio_fin()$Age, y = select_bio_fin()$Length,
                type = 'scatter', color = select_bio_fin()$Weight, colors = "Spectral",
                mode = 'markers', marker = list(opacity = 0.5), hoverinfo='text',
                text = paste("Age:", select_bio_fin()$Age,
                             "years<br>Length:", select_bio_fin()$Length,
                             "cm<br>Weight:", select_bio_fin()$Weight, "g")) %>%
      add_trace(x = fit()$Age, y = fit()$Length, type = 'scatter',
                mode = "lines", colors = "Spectral", hoverinfo='none') %>%
      layout(title=paste(input$species, "Age vs Length (points coloured by weight)"),
             x = "Age (years)", y = "Length (cm)",
             margin=(list(t=80)), showlegend = FALSE)
    p$elementId <- NULL
    p
    
  }else if(input$ageoptionselection=="Sample Type"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Age", "catch_category") %>%
        mutate(Length = as.numeric(Length),
               Age = as.numeric(Age)) %>%
        arrange(Age, Length)
    })
    
    if ("COD" %in% select_bio_fin()$species){
      select_bio_reg <- reactive({
        select_bio_fin() %>%
          subset(Age < 7)
      })
    } else {
      select_bio_reg <- reactive({
        select_bio_fin()
      })
    }
    
    reg1 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      try(reg <- vbStarts(length ~ age, data=vonBertalanffy))
      return (reg)
    })
    
    reg2 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      vb <- vbFuns(param="vonBertalanffy")
      try(reg <- nls(length ~ vb(age,Linf,K,t0),
                     data=vonBertalanffy, start=reg1()))
      return (reg)
    })
    print(reg2())
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      age <- unlist(select_bio_fin()$Age)
      
      try(x <- data.frame(age = age, Age = age),
          silent = TRUE) 
      try(x$Length <- predict(reg2(), x))
      
      return (x)
    })
    
    p <- plot_ly() %>%
      add_trace(x = select_bio_fin()$Age, y = select_bio_fin()$Length,
                type = 'scatter', color = select_bio_fin()$catch_category,
                colors = c('D'='red','L'='lightgreen'),
                mode = 'markers', marker = list(opacity = 0.5), hoverinfo='text',
                text = paste("Age:", select_bio_fin()$Age,
                             "years<br>Length:", select_bio_fin()$Length,
                             "cm<br>Sample type:", select_bio_fin()$catch_category)) %>%
      add_trace(x = fit()$Age, y = fit()$Length, type = 'scatter',
                mode = "lines", colors = "Spectral", hoverinfo='none') %>%
      layout(title=paste(input$species, "Age vs Length (points coloured by sample type)"),
             x = "Age (years)", y = "Length (cm)",
             margin=(list(t=80)), showlegend = FALSE)
    p$elementId <- NULL
    p
    
  }else if(input$ageoptionselection=="Gear"){
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Age", "gear") %>%
        mutate(Length = as.numeric(Length),
               Age = as.numeric(Age)) %>%
        arrange(Age, Length)
    })
    
    if ("COD" %in% select_bio_fin()$species){
      select_bio_reg <- reactive({
        select_bio_fin() %>%
          subset(Age < 7)
      })
    } else {
      select_bio_reg <- reactive({
        select_bio_fin()
      })
    }
    
    reg1 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      try(reg <- vbStarts(length ~ age, data=vonBertalanffy))
      return (reg)
    })
    
    reg2 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      vb <- vbFuns(param="vonBertalanffy")
      try(reg <- nls(length ~ vb(age,Linf,K,t0),
                     data=vonBertalanffy, start=reg1()))
      return (reg)
    })
    print(reg2())
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      age <- unlist(select_bio_fin()$Age)
      
      try(x <- data.frame(age = age, Age = age),
          silent = TRUE) 
      try(x$Length <- predict(reg2(), x))
      
      return (x)
    })
    
    p <- plot_ly() %>%
      add_trace(x = select_bio_fin()$Age, y = select_bio_fin()$Length,
                type = 'scatter', color = select_bio_fin()$gear, colors = "Set1",
                mode = 'markers', marker = list(opacity = 0.5), hoverinfo='text',
                text = paste("Age:", select_bio_fin()$Age,
                             "years<br>Length:", select_bio_fin()$Length,
                             "cm<br>Gear type:", select_bio_fin()$gear)) %>%
      add_trace(x = fit()$Age, y = fit()$Length, type = 'scatter',
                mode = "lines", colors = "Spectral", hoverinfo='none') %>%
      layout(title=paste(input$species, "Age vs Length (points coloured by gear)"),
             x = "Age (years)", y = "Length (cm)",
             margin=(list(t=80)), showlegend = FALSE)
    p$elementId <- NULL
    p
    
  }
  else{
    select_bio_fin <- reactive({
      select_bio() %>%
        drop_na("Length", "Age") %>%
        mutate(Length = as.numeric(Length),
               Age = as.numeric(Age)) %>%
        arrange(Age, Length)
    })
    
    if ("COD" %in% select_bio_fin()$species){
      select_bio_reg <- reactive({
        select_bio_fin() %>%
          subset(Age < 7)
      })
    } else {
      select_bio_reg <- reactive({
        select_bio_fin()
      })
    }
    
    reg1 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      try(reg <- vbStarts(length ~ age, data=vonBertalanffy))
      return (reg)
    })
    
    reg2 <- reactive({
      length <- unlist(select_bio_reg()$Length)
      age <- unlist(select_bio_reg()$Age)
      vonBertalanffy <- cbind(data.frame(length), data.frame(age))
      reg <- NA
      vb <- vbFuns(param="vonBertalanffy")
      try(reg <- nls(length ~ vb(age,Linf,K,t0),
                     data=vonBertalanffy, start=reg1()))
      return (reg)
    })
    print(reg2())
    fit <- reactive({
      length <- unlist(select_bio_fin()$Length)
      age <- unlist(select_bio_fin()$Age)
      
      try(x <- data.frame(age = age, Age = age)) 
      try(x$Length <- predict(reg2(), x))
      return (x)
    })
    
    p <- plot_ly() %>%
      add_trace(x = select_bio_fin()$Age, y = select_bio_fin()$Length,
                type = 'box', colors = "Set1") %>%
      add_trace(x = fit()$Age, y = fit()$Length, type = 'scatter',
                mode = "lines", colors = "Spectral", hoverinfo='none') %>%
      layout(title=paste(input$species, "Age vs Length"), x = "Age (years)",
             y = "Length (cm)", margin=(list(t=80)), showlegend = FALSE)
    p$elementId <- NULL
    p
  }
})

observeEvent(input$showhist, {
  output$age_hist <- renderPlotly({
    
    select_bio_hist <- reactive({
      select_bio() %>%
        drop_na("Length", "Age")  %>%
        mutate(Length = as.numeric(Length),
               Age = as.numeric(Age)) %>%
        subset(Length == as.numeric(paste(input$lengthcm)))
      })
    
    p <- ggplot(select_bio_hist(), aes(Age)) +
      geom_bar(color="black", fill="white", width=1) +
      labs(title = 'Histogram of observered ages', x = "Age", y = "Count")
    
    ggplotly(p)
    
    })
})

# ##### Bio dashboard text and graphs
#   output$fish_biology<- renderText({
#     as.character(Supp_table[which(Supp_table[,"Fish_Code"] %in% input$species),"biology"])
#   })
# 
# # add fish images to "Bio" sub table
#   output$fish_drawing<- renderImage({
#     filename <- paste("www/FishSketches/",
#                       Supp_table[which(Supp_table[,"Fish_Code"] %in% input$species),
#                                  "Fish_Code"],
#                       ".png", sep="")
#     list(src = filename, filetype = "png",width= "auto", height = 150)},
#     deleteFile = FALSE)
# 
