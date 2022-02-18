
library(shiny)
#library(shinyjs)
#library(shinydashboard)
#library(rintrojs)

# myUrl <- "https://thuenen-sampling.de"

# setwd("C:/Dateien/Datenbank 2.0/Thuenen/commercial/04_shiny/")


##########################################################
# UI definition - sets the frame, Panels and starting page 
##########################################################

shinyUI(
  
  #navBarPage
  navbarPage(
    theme = "style.css",
    collapsible = TRUE,
    id = "navbar",
    useShinyjs(),
    fluid = TRUE,
    windowTitle= tags$head(
      tags$link(rel = "icon", type = "image/png", href = "logo_small.png"),
      tags$title("Thünen OF")
    ),
    position = "fixed-top",
    header = tags$style(
      ".navbar-right { float: right !important;}",
      "body {padding-top: 55px;}"),
    tabPanel(id ="tabHome", title = introBox(icon("home")),
             
             fluidRow(
               column(12,align="center",
                      img( src="logo_regular.png", height=100)
               )),
             br(), 
             fluidRow(
               column(12,align="center",
                      includeHTML('www/home.html') 
               )),
             br(),
             fluidRow(
               column(12, align="center",
                      div(style="display: inline-block;",class= "image",
                          img(id="fisheryID", src="fishery_logo_eng.png",
                              height=250, style="cursor:pointer;margin-right:40px;")),
                      div(style="display: inline-block;",class= "image",
                          img(id ="sampleID", src="sampling_logo_eng.png",
                              height=250, style="cursor:pointer;margin-right:40px;")),
                      div(style="display: inline-block;",class= "image",
                          img(id ="stockID", src="stock_logo_eng.png", height=250,
                              style="cursor:pointer;margin-right:40px;"))
                      )
                    ),
             br(), br(), br(), br(),
             br(),
             # fluidRow(
             #   width =12,style = "margin-top:-4em",
             #   box(width =12, img(
             #     src="header-fang2.jpg", width = "1200px", height = "75px",
             #     style="display: block; margin-left: auto; margin-right: auto;
             #     margin-top:0em"))),
             br(),
             br(),
             fluidRow(
               column(12,  style= "margin-top:-4em", #align="center",
                box(title = "Further information", width = 4, background = "blue",
                  textOutput("moreInfo"),
                  br(),
                  actionButton("modal1", "Introduction"),
                  actionButton("modal2", "Project Objectives"),
                  br(),
                  br(),
                  actionButton("modal3", "Sampling Design"),
                  actionButton("modal4", "Methodology"),
                  br(),
                  br()),
                box(title ="Useful links",width = 4, status = "success",
                  a(href=paste0("https://www.dcf-germany.de/"),
                     "The German DCF Webpage",target="_blank"),
                  p(),
                  a(href=paste0("https://dmar01-hro.thuenen.de/"),
                     "The Thünen-OF Database project",target="_blank"),
                  p(),
                  a(href=paste0("https://www.fischbestaende-online.de/"),
                     "The Thünen fish and fisheries information website",target="_blank"),
                  br(),
#                 br(),
                  br(),
                  br()),
                box(title = "Disclaimer Information", width = 4, background = "blue",
                   textOutput("moreDetails"),
                  br(),
                  actionButton("modal5", "Read the Disclaimer again"),
                  br()),
    )),
    hr()
    ), #end of TabPanel
    # ------------------------------------------------------------
    # Fisheries Panel - presents the BLE and federal fishing data 
    # ------------------------------------------------------------
    navbarMenu(
      "Fishery",
      
      # -----------------------------------
      # fishing fleet overview
      # -----------------------------------
      tabPanel(id="tabfish_over", "Fishery overview",
               dashboardPage(
                 dashboardHeader(disable = TRUE),
                 dashboardSidebar(disable = TRUE),
                 dashboardBody(
                   fluidPage(
                     # setSliderColor("white", 1),
                     # to change the color of the first slider in this fluidpage
                     chooseSliderSkin("Round"),
                     # change the style of all slider in this fluidpage
                     fluidRow(valueBoxOutput("box1"),
                              valueBoxOutput("box2"),
                              valueBoxOutput("box3")),
                     br(), 
                     fluidRow(
                       box(h4("Interactive map"), style = "margin-top:-1.5em",
                           width=12, background = "light-blue",
                           column(8, leafletOutput("map", height=550, width="auto")),
                           column(
                             4, id="controls", fixed=FALSE, draggable = TRUE,
                             fluidRow(
                               column(
                                 12, selectInput(
                                   "species_groups",label=NULL,
                                   choices = c("Total Landings kg",
                                               "Demersal Species kg",
                                               "Pelagic Species kg")),
                                 sliderInput("slideryear", "Year:",
                                             min = 2003, max(landings$jahr), #max = 2021
                                             value = 2020, step = 1, sep = "", animate = TRUE),
                                 # checkboxInput("rec", "ICES Rectangles", FALSE),
                                 selectInput("spatialICES", label="spatial resolution",
                                             choices = c("FAO_area", "ICES rectangles"))
                                 )),
                             br(), br(), br(), br(),
                             fluidRow(
                               column(12,
                                      plotlyOutput("sp_piechart") %>% 
                                        withSpinner(color="#0dc5c1"),
                                      style = "margin-top:-9em"),
                               solidHeader =FALSE, collapsible = T,
                               width= "auto", background = "navy")
                             ), 
                           )),
                     fluidRow(width =12,style = "margin-top:-4em", 
                              box(width =12,
                                  img(src="header-fang2.jpg",
                                      width = "1200px", height = "75px",
                                      style="display: block; margin-left: auto;
                                      margin-right: auto;margin-top:0em"))
                              ),
    # buttons to select the subcategories of fishery overview, work in progress,  
    # needs to finish subcategories first 
#                     fluidRow(
#                       column(12, align="center",
#                                actionButton("button","Landings"),
#                                 tags$button(id = "landing_button",
#                                  class = "btn action_button",   
#                                   tags$img(src = "fas fa-ship",height = "50px")),
#                                         
#                             div(style="display: inline-block;",class= "image",
#                                 img(id ="logbookID", src="sampling_logo_eng.png", height=100,
#                                     style="cursor:pointer;margin-right:40px;")),
#                              div(style="display: inline-block;",class= "image",
#                                  img(id ="inventoryID", src="stock_logo_eng.png", height=100,
#                                      style="cursor:pointer;margin-right:40px;"))
#                     )
                   ) #end of fluidpage
                 ) #end of dashboardBody
               ) # end of dashboardPage
      ), #end of tabPanel   
      
      # -----------------------------------
      # Landings by area, harbor and as an output table 
      # -----------------------------------
      tabPanel(id="tab_fish_landings", "Commercial Landings",
               fluidRow(
                 column(width = 3,
                        selectInput("species_groups",label="Species Group",
                                    choices = c("Demersal Species", "Pelagic Species",
                                                "Freshwater Species")),
                        sliderInput("slideryear", "Year:", min = 2003,
                                    max(landings$jahr), #max = 2021
                                    value = 2020, step = 1, sep = "", animate = TRUE),
                        checkboxGroupInput("variable", "Select Quarter:",
                                           c("Q1" = "q1", "Q2" = "q2",
                                             "Q3" = "q3", "Q4" = "q4"))
                               
               ),
               column(width=3, 
                      selectInput("area_lan", label="FAO Area",
                                  choices = list("All", "27.3.c.22 (Belt)",
                                                 "27.3.d.24 (Arkona)",
                                                 "27.3.d.25 (Bornholm)",
                                                 "27.3.d.26 (East of Gotland)"),
                                  selected = "All"),
                      actionButton("showres",label = "Show Results")
               ),
               
               column(width = 6, 
                      tabsetPanel(id = "lantab",
                                  tabPanel("Map",#value= "A", 
                                           p(), fluidRow(column(width=7))), 
                                  tabPanel("Harbors",# value = "B", 
                                           p(), fluidRow(column(width=5))),
                                  tabPanel("Table",# value = "B", 
                                           p(), fluidRow(column(width=5)))
                      )
               )
               ) #end of fluidRow
      ), #end of TabPanel      
      
      # -----------------------------------
      # logbooks overview
      # -----------------------------------
      tabPanel(id="tab_fish_log", "Logbooks"
               
      ),
      
      
      # ------------------------------------------------------------
      # Inventory tables for the landings and effort (generated from the all_vessel structure)
      # ---------------------------------------------------------------
      tabPanel(id="tab_fish_inventory", "Inventory tables",
               
               titlePanel("Fishery census data"),
               helpText("These inventories contain landings and effort data
                        after they have been processed at the Thünen-OF"),
               helpText("The current year is considered preliminary"),
               tabsetPanel(
                 type = "tabs",
                 tabPanel(
                   "commercial landings (CL)",
                   align = 'center',
                   br(),
                   downloadButton(outputId = 'download_filtered_inventorytable_CL',
                                  label = "Download the filtered dataset",
                                  icon("arrow-alt-circle-down")),
                   br(),
                   
                   DT::dataTableOutput("inventorytable_CL"),
                   add_busy_spinner(spin = "scaling-squares", color = "grey",
                                    timeout = 5, position = "top-right",
                                    margins = c(55,20))
                 ),
                 tabPanel(
                   "fishing effort (CE)",
                   align = 'center',
                   br(),
                   downloadButton(
                     outputId = 'download_filtered_invetorytable_CE',
                     label = "Download the filtered dataset",
                     icon("arrow-alt-circle-down")),
                   br(),
                   
                   DT::dataTableOutput("inventorytable_CE"),
                   add_busy_spinner(spin = "scaling-squares", color = "grey",
                                    timeout = 5, position = "top-right",
                                    margins = c(55,20))
                 )
               ) #end of TabSetPanel
      ) #end of TabPanel inventory
      
    ), #end of Fishery NavBar       
    
    
    # -----------------------------------------------------------------------
    # Sampling Panel - presents the sampled cruises, weight and length data 
    # -----------------------------------------------------------------------
    navbarMenu(
      "Sampling",

      #----------------------      
      # Sample overviews
      #----------------------
      
      tabPanel(id="tab_sample_overview", "Sampling overview",
               dashboardPage(
                 dashboardHeader(disable = TRUE),
                 dashboardSidebar(disable = TRUE),
                 dashboardBody(
                   fluidPage(
                     chooseSliderSkin("Round"),
                     # change the style of all slider in this fluidpage
                     fluidRow(valueBoxOutput("box1_sample"),
                              valueBoxOutput("box2_sample"),
                              valueBoxOutput("box3_sample")),
                     br(), 
                     fluidRow(
                       box(h4("Sampling coverage"), style = "margin-top:-1.5em",
                           width=12, background = "light-blue",
                           column(8, plotOutput("ggplot_sample", height=550, width="auto")),
                           column(
                             4, id="controls", fixed=FALSE, draggable = TRUE,
                             fluidRow(
                               column(
                                 12, selectInput(
                                   "sample_groups",label=NULL,
                                   choices = c("Total Samples", "Demersal Fishery",
                                               "Pelagic Fishery", "Harbor Samples")),
                                 sliderInput("slideryear_sample", "Year:",
                                             min = 2003, max(trip$year), #max = 2021
                                             value = 2019, step = 1, sep = "", animate = TRUE)
                                 )), 
#                                         br(), br(), br(), br(), 
#                                         fluidRow(column(
#                                           12, plotlyOutput("sp_piechart") %>%
#                                             withSpinner(color="#0dc5c1"),
#                                           style = "margin-top:-9em"),
#                                         solidHeader = FALSE, collapsible = T,
#                                         width= "auto", background = "navy")
                                  ), 
                     )),
                     fluidRow(width =12,style = "margin-top:-4em", 
                              box(width =12,
                                  img(src="header-fang2.jpg", width = "1200px",
                                      height = "75px", style="display: block;
                                      margin-left: auto; margin-right: auto;
                                      margin-top:0em"))
                     )
                   ) #end of fluidRow
                 ) #end of dashboardBody
               ) # end of dashboardPage
      ), #end of tabPanel 

      

      # -----------------------------------
      # trips 
      # -----------------------------------
      
      tabPanel(id="tab_sample_trip", "spatiotemporal coverage",
                titlePanel("Spatiotemporal overview of sampled fishing trips"),
    #            helpText("text text text"),
                helpText("Samples of the current year are considered preliminary"),
               
              fluidRow(
                column(width = 4,
                  selectInput("species_trip",label="Sampling Type",
                              choices = c("Demersal fishery", "Pelagic fishery",
                                          "Freshwater fishery", "Harbor samples")),
                  
                  checkboxGroupInput("sample_type", "Fishery:",
                                     c("Active" = "active", "Passive" = "passive")),
                  
                  sliderInput("slideryear_trip", "Year:", min = 2003,
                              max(landings$jahr), #max = 2021
                              value = 2020, step = 1, sep = "", animate = TRUE),
                  checkboxGroupInput("quarter_trip", "Select Quarter:",
                                     c("Q1" = "q1", "Q2" = "q2",
                                       "Q3" = "q3", "Q4" = "q4")),
                 
                  selectInput("area_trip", label="FAO Area",
                                choices = list("All", "27.3.c.22 (Belt)",
                                               "27.3.d.24 (Arkona)",
                                               "27.3.d.25 (Bornholm)",
                                               "27.3.d.26 (East of Gotland)"),
                                selected = "All"),
                 
                  actionButton("showres_trip",
                               label = "Show Results",
                               icon("paper-plane"))#,
                #  actionButton("down_trip",label = "Download")
                              ), #end of column 1
                        
                column(width = 8, 
                  tabsetPanel(id = "trip_tabs",
                    tabPanel("Map", 
                             helpText("Sampling locations as recorded by the observer"), 
                             p(),
                             fluidRow(
                               column(8, leafletOutput(
                                 "trip_map", height=550, width="auto")),
                               column(4, selectInput(
                                 "spatial_ICES_trip", label="spatial resolution",
                                 choices = c("FAO_area", "ICES rectangles")),
                                 selectInput("census_data_trip",
                                             label="add commercial data",
                                             choices = c("landings", "effort")),
                                 br(), br(),
                                 actionButton("down_map_trip",
                                              label = "Download Map Data",
                                              icon("arrow-alt-circle-down")))
                               )
                             ),
                    
                    tabPanel("statistics", helpText("Trip statistics"), 
                             p(), fluidRow(column(width=5))),

                    tabPanel("Data", helpText("Data tables"), 
                             p(), fluidRow(column(width=5)))
                    )
          ) #end of column 2
        ) #end of fluidRow
      ), #end of TabPanel      


      # -----------------------------------
      # cruise report markdown
      # -----------------------------------
      tabPanel(id="tab_sample_weight", "weight and length frequencies"
         
      ),
# add PSD statistics (page 113 of R book)

      # -----------------------------------
      # Sampling lists and inventory
      # -----------------------------------
      tabPanel(id="tab_sample_list", "Sample inventory"
         
      ),



      # -----------------------------------
      # cruise report markdown
      # -----------------------------------
      tabPanel(id="tabInventory", "create report"
               
      )




    ), #end of NavBarMenu "Sampling"
    

    # ---------------------------------------------------------
    # Biology Panel - presents the biological single fish data 
    # ---------------------------------------------------------
   
    navbarMenu("Biology",
              
      # -----------------------------------
      # biological data overview
      # -----------------------------------
               
                tabPanel(id="tabstock_over", "Biology overview"
                        
               ),
               
               
      # -----------------------------------
      # Species dashboard
      # -----------------------------------                
       tabPanel(id="tabstock_dash", "Species Dashboard",
                titlePanel("Species Dashboard"),
                helpText("The species dashboard displays biological
                         data from the commercial fishery samples"),
                helpText("select the desired parameter below"),
                fluidRow(
                  column(
                    width = 7,
                    fluidRow(
                      column(width=3,
                             # We set the species list and default selection in server.R now
                             selectInput("species",label="Species",
                                         choices = list("All", "COD", "FLE",
                                                        "PLE", "DAB", "HER", "TUR"),
                                         selected = "COD"),
                             conditionalPanel(
                               condition = "input.fishtab == 'A'",
                               selectInput(
                                 inputId="biooptionselection", label="Select parameter",
                                 choices=list("None","Age","Sex","Gear","Sample Type"),
                                 selected = "None")),
                             conditionalPanel(
                               condition = "input.fishtab == 'B'",
                               selectInput(
                                 inputId="ageoptionselection", label="Select parameter",
                                 choices=list("None","Weight","Sex","Gear","Sample Type"),
                                 selected = "None")),
                             conditionalPanel(
                             condition = "input.biooptionselection =='Gear'
                             && input.fishtab == 'A'", uiOutput("GearFilter")),
                             conditionalPanel(
                             condition = "input.ageoptionselection =='Gear'
                             && input.fishtab == 'B'", uiOutput("GearFilter.a"))),
                             column(width=4, selectInput(
                               "quarter", label="Quarter",
                               choices = list("All", 1, 2, 3, 4),
                               selected = "All"),
                               sliderInput(
                                 "year", "Years", min=min(trip$year, na.rm=TRUE),
                                 max=max(trip$year, na.rm=TRUE),
                                 value=max(trip$year, na.rm=TRUE),
                                 sep="", step=1)), #by one year
                      column(width=5,
                             conditionalPanel(
                               "input.fishtab == 'A'",
                               radioGroupButtons(
                                 inputId = "Id",label = "",
                                 choices = c("FAO Area", "Rectangle"),
                                 direction = "horizontal",
                                 checkIcon = list(
                                   yes = tags$i(class = "fas fa-check-square",
                                                style = "color: steelblue"),
                                   no = tags$i(class = "fas fa-square-o",
                                               style = "color: steelblue"))),
                               uiOutput("spatialops.w")),
                             
                             conditionalPanel(
                               "input.fishtab == 'A'",
                               downloadButton("downloadDatalw", "Download data")),
                             
                             conditionalPanel(
                               "input.fishtab == 'B'",
                               radioGroupButtons(
                                 inputId = "Id.a", label = "",
                                 choices = c("FAO Area", "Rectangle"),
                                 direction = "horizontal",
                                 checkIcon = list(
                                   yes = tags$i(class = "fas fa-check-square",
                                                style = "color: steelblue"),
                                   no = tags$i(class = "fas fa-square-o",
                                               style = "color: steelblue"))),
                               uiOutput("spatialops.a")),
                             
                             conditionalPanel(
                               "input.fishtab == 'B'",
                               downloadButton(
                                 "downloadDatala", "Download data",
                                 class="btn btn-outline-primary")
                               ))),
                    
                    ##### Fish sp tab - Maps and plots  ######
                    
                    fluidRow(
                      column(width=12,
                             conditionalPanel(
                               condition = "input.fishtab == 'A'",
                               plotlyOutput("bio_lw") %>%
                                 withSpinner(color="#0dc5c1")),
                             conditionalPanel(
                               condition = "input.fishtab == 'B'",
                               plotlyOutput("bio_la") %>%
                                 withSpinner(color="#0dc5c1"))
                             )),
                    fluidRow(
                      column(width=12,
                             conditionalPanel(
                               condition = "input.fishtab == 'B'",
                               plotlyOutput("ageVB"))
                      ))
                    ), 
                  
                ##### Fish sp tab - Species tabsets #####
                column(
                  width = 5,
                  tabsetPanel(
                    id = "fishtab",
                    tabPanel(
                      "Biology", value= "A", p(), htmlOutput("fish_biology"),
                      fluidRow(
                        column(width=7, imageOutput("fish_drawing", height='100%')),
                        column(width=5,
                               conditionalPanel(
                                 condition = "input.species =='COD'",
                                 imageOutput("monk_belly"))))),
                    tabPanel(
                      "Age", value = "B", p(),
                      fluidRow(column(width=5, htmlOutput("ageingtxt")),
                               column(width=7, imageOutput(
                                 "speciesotolith", height='100%'))),
                      p(),
                      fluidRow(column(
                        width=5, textInput(
                          "lengthcm", label = "Enter fish length in cm:"),
                        value = 0),
                        column(width=7, tags$b("Age range observed*:"),
                               h4(textOutput("agerange")),
                               tags$b("Modal age is:"),
                               h4(textOutput("mode")),
                               tags$small("*age range based on age readings
                                            and lengths taken from fish sampled
                                            from commercial vessels"))),
                      hr(),
                      fluidRow(column(width=5, actionButton(
                        "showhist", label = "Show Histogram")),
                        column(width=7,p())),
                      fluidRow(p()), plotlyOutput("age_hist"), fluidRow(p())),
                    tabPanel("Age Cohort",value= "C",
                             p(),htmlOutput("fish_distribution"),
                             p(),htmlOutput("fish_b1a"))
                )
                )
              )),                
        
      
      # -----------------------------------
      # stock specific data and plots
      # -----------------------------------
      tabPanel(id="tabInventory", "Stock parameter"),
               
               
              
      
      # -----------------------------------
      # stock specific data and plots
      # -----------------------------------
      tabPanel(id="tablength", "Length distributions",
               fluidPage(
                  titlePanel("fish length test"), 
                  sidebarLayout(
                  sidebarPanel(
                    # selectInput("tbl_sample_length", "Choose a dataset:",
                    #             choices = c("cod", "plaice", "flounder")
                    #             ),
                  # numericInput("area", "Number of observations to view:", 10),
                  sliderInput("year", "Year", min = 2016, max = 2020,
                              value = 2019, sep = ""),
                  helpText("test some stuff")),
                  mainPanel(plotOutput("distPlot"))
                  ) # end layout
                  ) # end fluidpage
               ) # end tabPanel 
    ) # end navbarmenue    
    

# -------                     
    
  ) # end Navbarpage
)# end of ui
