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

#

startyear <- 2018
lastyear  <- 2020

registry <- data.frame(get_ARPA_Lombardia_AQ_registry()) %>% rename(PM25 = PM2.5)

IDStat <- sqldf('select distinct IDStation from registry where 
                 (Pollutant="Ammonia" or Pollutant="PM10" or Pollutant="PM2.5") and
                 DateStart <= "2017-01-01" and DateStop is null')

RegistryRed <- sqldf('select * from registry where IDStation in IDStat')

#TODO PART 2: looking for stations with 1 or more pollutants 

CentralineMorethan1 <- RegistryRed %>% group_by(IDStation) %>% 
  summarise(n=n()) %>% filter(n>=1) %>% distinct(IDStation) %>%
  pull() # Stations that measure at least 2 of the variables at the same time

cast2 <- Download(startyear, lastyear, CentralineMorethan1)

#Queries for counting the amount of missing data
tableMissingAmmmonia2<-NULL
tableMissingPM102<-NULL
tableMissingPM252<-NULL
tableMissingalldata2 <-NULL
tableMissingdataTotal2<-NULL

for(index in startyear:lastyear) {
  interestedTable2 <- cast2[[index-startyear+1]]
  
  tableMissingAmmmonia2[[index-startyear+1]] <- MissingTable('Ammonia', 'interestedTable2')
  
  tableMissingPM102[[index-startyear+1]] <- MissingTable('PM10', 'interestedTable2')
  
  tableMissingPM252[[index-startyear+1]] <- MissingTable('PM25', 'interestedTable2')
  
  tableMissingalldata2[[index-startyear+1]] <- MissingAll('interestedTable2')
  
  tableMissingAmmmoniatemp2 <- tableMissingAmmmonia2[[index-startyear+1]]
  
  tableMissingPM10temp2     <- tableMissingPM102[[index-startyear+1]]
  
  tableMissingPM25temp2     <- tableMissingPM252[[index-startyear+1]]
  
  tableMissingalldatatemp2 <- tableMissingalldata2[[index-startyear+1]]
  
  tableMissingdataTotal2[[index-startyear+1]] <- sqldf(' SELECT ma.IDStation, ma.NameStation, ma.MissingAmmonia, m10.MissingPM10, m25.MissingPM25,mtodos.MissingAllThree
                                  FROM tableMissingAmmmoniatemp2 ma  JOIN tableMissingPM10temp2 m10
                                  ON ma.IDStation = m10.IDStation
                                  JOIN tableMissingPM25temp2 m25
                                  ON ma.IDStation = m25.IDStation
                                  JOIN tableMissingalldatatemp2 mtodos
                                  on ma.IDStation = mtodos.IDStation
                                ')
}


#COUNT OF THE TOTAL OF THE MISSING data for every station
totalMissingFromBeginning <- NULL
temp <- NULL
for(index in 1:(length(tableMissingdataTotal2))) {
  if(index==1) {
    totalMissingFromBeginning <- tableMissingdataTotal2[[index]]
  } else {
    temp <- tableMissingdataTotal2[[index]]
    auxiliaryTable <- totalMissingFromBeginning 
    auxiliaryTable
    totalMissingFromBeginning <- sqldf('SELECT t.IDStation,t.NameStation, 
                                      SUM(t.MissingAmmonia+a.MissingAmmonia)  
                                      as MissingAmmonia,
                                      SUM(t.MissingPM10+a.MissingPM10)
                                      as MissingPM10 ,
                                      SUM(t.MissingPM25+a.MissingPM25)
                                      as MissingPM25,
                                      SUM(t.MissingAllThree+a.MissingAllThree)
                                      as t.MissingAllThree
                                      FROM temp t 
                                      JOIN auxiliaryTable a
                                      ON t.IDstation = a.IDStation
                                      GROUP BY t.IDStation
                                       ')
  } 
}

#YES/NO TABLE

TableA <- tableMissingAmmmonia2[[1]]

ColumnA <- sqldf('SELECT IDStation, NameStation, 1 as Ammonia
      FROM TableA 
      WHERE MissingAmmonia < 365
      union
      SELECT IDStation, NameStation, 0 as Ammonia
      FROM TableA 
      WHERE MissingAmmonia >= 365
      order by IDStation')

Table10 <- tableMissingPM102[[1]]

Column10 <- sqldf('SELECT IDStation, NameStation, 1 as PM10
      FROM Table10 
      WHERE MissingPM10 < 365
      union
      SELECT IDStation, NameStation, 0 as PM10
      FROM Table10 
      WHERE MissingPM10 >= 365
      order by IDStation')

Table25 <- tableMissingPM252[[1]]

Column25 <- sqldf('SELECT IDStation, NameStation, 1 as PM25
      FROM Table25
      WHERE MissingPM25 < 365
      union
      SELECT IDStation, NameStation, 0 as PM25
      FROM Table25
      WHERE MissingPM25 >= 365
      order by IDStation')
#Legend
#1 means presence
#0 means absence
presencetable <- sqldf("SELECT c25.IDStation, C25.NameStation, c25.PM25, c10.PM10, ca.Ammonia, SUM(c25.PM25+c10.PM10+ca.Ammonia) as Somma
                 FROM Column25 c25 JOIN Column10 c10
                 ON c25.IDStation = c10.IDStation
                 JOIN ColumnA ca
                 ON c25.IDStation = ca.IDStation
                 GROUP BY c25.IDStation ") 

presencetable <- sqldf('SELECT IDStation, NameStation, PM25, PM10, Ammonia
                        FROM presencetable 
                        GROUP BY  Somma, IDStation')

#Plot of the centralines with at least 1 observation for one of the pollutant
presencetable <- presencetable %>%
  mutate(Tag = case_when(PM10 == 1 & PM25 == 1 & Ammonia == 1 ~ "All",
                         PM10 == 1 & PM25 == 1 & Ammonia == 0 ~ "PM10-PM2.5",
                         PM10 == 1 & PM25 == 0 & Ammonia == 1 ~ "PM10-NH3",
                         PM10 == 0 & PM25 == 1 & Ammonia == 1 ~ "PM2.5-NH3",
                         PM10 == 1 & PM25 == 0 & Ammonia == 0 ~ "PM10",
                         PM10 == 0 & PM25 == 1 & Ammonia == 0 ~ "PM2.5",
                         PM10 == 0 & PM25 == 0 & Ammonia == 1 ~ "NH3"))

presencetable_red <- presencetable %>%
  select(IDStation,Tag)

RegistryRed <- full_join(RegistryRed,presencetable_red,by = c("IDStation"))

write.table(presencetable, "presencetable_red.csv")
