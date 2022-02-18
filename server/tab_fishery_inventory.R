#------------------------------------
# Fishery inventories
#------------------------------------
# 
# inventory tables require the "all_vessel_" format 
#  
# tables for lan and effort are loaded separately   


##### create DT frame for data #####

# that's an SQL command to make later transition/connection to DB easier, otherwise use dplyr to re-arrange


# modify the CL.Rdata
cl<-as.data.table(all_vessel_cl)
#cl<-fread(file) 
clinventory <-sqldf("select year,quarter,month,area,zone,statistical_rectangle,harbour,vessel_length_class,geraet,masche,species,target_species,metier, 
								count(distinct(eunr)) as NoVessel, 
								count(distinct(reisenr)) as NoTrips, 
								round(sum(sum_fangkg_fangnr_ort_monat),3) as landings_kg, 
								round(sum(official_landings_value),0) as revenue_eur, 
								round(avg(avg_trip_price),3) as avg_revenue  
								from cl group by year,quarter,month,area,zone,statistical_rectangle,harbour,vessel_length_class,geraet,masche,species,target_species,metier"
)   

# modify the CL.Rdata
ce<-as.data.table(all_vessel_ce)
#cl<-fread(file) 
ceinventory <-sqldf("select year,quarter,month,area,zone,statistical_rectangle,harbour,vessel_length_class,geraet,masche,metier, 
								count(distinct(eunr)) as NoVessel, 
								count(distinct(reisenr)) as NoTrips, 
								round(sum(BRZ),0) as tonnage_total,
								round(sum(BRZ),0) as kilowatts_total,
								round(sum(days_at_sea),0) as days_at_sea, 
								round(sum(fangzeit),0) as catch_hours, 
							  round(sum(fangzeit_minuten),0) as catch_minutes
								from ce group by year,quarter,month,area,zone,statistical_rectangle,harbour,vessel_length_class,geraet,masche,metier"
)  



## reactive table approach, does not load correctly (sql error maybe?)

#data_list<-reactive({
#  req(input$file)
#  load(input$file$datapath, envir = .GlobalEnv) 
#  # datatable wants factors for filter = 
#  clinventory$area<-as.factor(clinventory$area)
#  clinventory$zone<-as.factor(clinventory$zone)
#  clinventory$statistical_rectangle<-as.factor(clinventory$statistical_rectangle)
#  clinventory$harbour<-as.factor(clinventory$harbour)
#  clinventory$vessel_length_class<-as.factor(clinventory$vessel_length_class)
#  clinventory$geraet<-as.factor(clinventory$geraet)
#  clinventory$species<-as.factor(clinventory$species)
#  clinventory$target_species<-as.factor(clinventory$target_species)
#  clinventory$metier<-as.factor(clinventory$metier)
#  
#  list1<-vector(mode = "list")
#  list1[[1]]<-clinventory
#  list1[[2]]<-ceinventory
#  
#  list1
#  
#})


# output for CL inventory
output$inventorytable_CL <-DT::renderDataTable({clinventory}
                                               
                                               , options = list(
                                                 pageLength = 20,autoWidth=T,scrollX=TRUE
                                               ),filter = 'top'
)
# output for CL inventory
output$inventorytable_CE <-DT::renderDataTable({ceinventory}
                                               
                                               , options = list(
                                                 pageLength = 20,autoWidth=T,scrollX=TRUE
                                               ),filter = 'top'
)


### output for reactive approach 
#output$inventorytable_CL <- DT::renderDT(DT::datatable({data_list()[[1]]
#}
#, options = list(
#  pageLength = 20,autoWidth=T,scrollX=TRUE
#),filter = 'top'
#))


## output for CE inventory
#output$inventorytable_CE <- DT::renderDT(DT::datatable({data_list()[[2]]}
#                                                       , options = list(
#                                                         pageLength = 20,autoWidth=T,scrollX=TRUE
#                                                       ),filter = 'top'
#))



########
#download widget
########

output$download_filtered_inventorytable_CL <- 
  downloadHandler(
    filename = "cl_inventory_data.csv",
    content = function(file){
      write.csv(clinventory[input[["inventorytable_CL_rows_all"]], ],
                file)
    }
  )
#download widget
output$download_filtered_inventorytable_CE <- 
  downloadHandler(
    filename = "CE_inventory_data.csv",
    content = function(file){
      write.csv(ceinventory[input[["inventorytable_CE_rows_all"]], ],
                file)
    }
  )

