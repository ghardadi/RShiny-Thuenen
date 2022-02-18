##--------------
## libraries
##--------------

library(shinythemes)
library(shinyBS)
library(shinyTime)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(shinyalert)
library(shinybusy)
library(shinyWidgets)

# data handling and plotting
library(plotly)
library(dplyr)
library(leaflet)
library(tidyr)
#library(leafem)
library(ggplot2)
library(lubridate)
library(data.table)
library(DT)
library(forcats)
#library(colorspace)

# markdown scripts
library(rmarkdown)
library(stringr)
library(knitr)

#for mapping 
library("sf")
library(viridis)
library("rnaturalearth") # map of countries of the entire world
library("rnaturalearthdata") # Use ne_countries to pull country data
library(rgdal)
#library(webshot)
library(mapplots)

# data handling
library(rintrojs)
library(ggrepel) # required by shinyappsio
library(rgeos) # required by shinyappsio
library(sqldf)
library(FSA)

ices.rect <- read_sf("shp/ices_rectangles/ices_squares_simple.shp")
ices.rect<-as(ices.rect, 'Spatial')

trip <- read.csv("data/trip.csv", sep=";")
haul_fo <- read.csv("data/haul_fo.csv")
haul_gear <- read.csv("data/haul_gear.csv")

sample_weight <- read.csv("data/sample_weight.csv")
sample_length <- read.csv("data/sample_length.csv")
sample_bio <- read.csv("data/sample_bio.csv")

landings <- read.csv("data/anlandung.csv", sep=";")
all_vessel_cl <- read.csv("data/all_vessel_cl.csv", sep=";")
all_vessel_ce <- read.csv("data/all_vessel_ce.csv", sep=";")

# add species group to the landings
landings <- landings %>% mutate(group_fish = case_when(fischart %in% c("COD", "PLE", "FLE", "DAB", "SOL", "TUR", "BLL", "WHG", "LUM") ~ "demersal species",
                                                       fischart %in% c("HER", "SPR", "GAR", "MAC", "HKE") ~ "pelagic species",
                                                       fischart %in% c("FPE", "FRO", "FBR", "FBU", "FPI", "FPP", "FTE", "ELP", "FCC", "FCP") ~ "freshwater species",
                                                       fischart %in% c("TRS","PLN", "ELE","SAL","TRO") ~ "freshwater species", TRUE ~ "other")
                                )
# add trip type to the sampled trip_numbers
trip <- trip %>% mutate(group_trip = case_when(str_sub(trip_number,2,3) %in% c("01", "02", "03", "04", "05", "06", "07", "11", "12", "13") ~ "demersal fishery",
                                               str_sub(trip_number,2,3) %in% c("08", "09") ~ "pelagic fishery",
                                               str_sub(trip_number,2,3) %in% c("14", "15", "16") ~ "harbor samples",
                                               TRUE ~ "other")) %>%
                 mutate(days_at_sea = as.numeric(as.Date(end_date)- as.Date(start_date)+1))


#
Supp_table <- read.csv("data/supplement_species.csv", header=TRUE, sep = ";")

