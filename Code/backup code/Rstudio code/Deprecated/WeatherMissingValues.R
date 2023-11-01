#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggfortify)

registry <- get_ARPA_Lombardia_W_registry()

data <- get_ARPA_Lombardia_W_data(
  ID_station = 628,
  Year = 2019,
  Frequency = "weekly",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)