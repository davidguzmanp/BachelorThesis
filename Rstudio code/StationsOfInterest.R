#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(sf)

#Source functions
source("~/GitHub/BachelorThesis/Rstudio code/Functions.R", encoding = 'UTF-8')

#Stations we are interested in:
#1 703: Schivenoglia (R)
#2 681: Moggio (R)
#3 627: Cremona P.zza Cadorna (U) 

#Array with station IDs
arrayStations <-c(703, 681, 627)


#Time-period of interest
startyear <- 2018
endyear   <- 2020

#dataset download
total <- NULL

for(i in 1:length(arrayStations)){
  
  data <- Easydownload(startyear, endyear, arrayStations[i])
  
  total[[i]] <- data
}

#