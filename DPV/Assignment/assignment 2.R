library(DBI)
library(RPostgreSQL)
library(readr)
library(dplyr)
library(lubridate)

data <-read_delim(file="DPV/Data/SuperSales/SuperstoreSales_main.csv", delim=";")
head(data)

