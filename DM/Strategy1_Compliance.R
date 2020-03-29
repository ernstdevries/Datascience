library(DBI)
library(RPostgreSQL)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
library(openxlsx)

data0 <-read_delim(file="Activiteitenoverzicht_2013-2014_v2 (1)/Activiteitenoverzicht_2013-2014_v2.csv", delim=",", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
head(data0)
data1 <-read_delim(file="Activiteitenoverzicht_2014-2015_v2/Activiteitenoverzicht_2014-2015_v2.csv", delim=",", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
head(data0)
data2 <-read_delim(file="Activiteitenoverzicht_2015-2016_v2/Activiteitenoverzicht_2015-2016_v2.csv", delim=",", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
head(data0)
data3 <-read_delim(file="Activiteitenoverzicht_2016-2017_v2/Activiteitenoverzicht_2016-2017_v2.csv", delim=",", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
head(data0)

finalData <- rbind(data0, data1, data2, data3) 

dataCourses <-read_excel("UT_courses_Osiris_with_teacher_2013-2014.xlsx", col_names = TRUE, col_types = NULL, skip = 3)
head(data0)
dataCourses2 <-read_excel("UT_courses_Osiris_with_teacher_2014-2015.xlsx", col_names = TRUE, col_types = NULL, skip = 3)
head(data0)

finalDataCourses <- rbind(dataCourses, dataCourses2) 

dataSaxion <- read.xlsx("All timetabling activities SAX 2013-2015 (1).xlsx", sheet = 1, startRow = 1, colNames = TRUE)
head(data0)