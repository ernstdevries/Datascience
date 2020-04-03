library(DBI)
library(RPostgreSQL)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
library(openxlsx)
library(stringr)

data1 <-read_delim(file="Project timetable/Activiteitenoverzicht_2014-2015_v2.csv", delim=",", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
head(data0)
data2 <-read_delim(file="Project timetable/Activiteitenoverzicht_2015-2016_v2.csv", delim=",", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
head(data0)
data3 <-read_delim(file="Project timetable/Activiteitenoverzicht_2016-2017_v2.csv", delim=",", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
head(data0)

finalData <- rbind(data0, data1, data2, data3) 

dataCourse <-read_excel("Project timetable/UT_courses_Osiris_with_teacher_2013-2014.xlsx", col_names = TRUE, col_types = NULL, skip = 3)
dataCourse <- rename(dataCourse,Collegejaar = Collegeyear, Cursus = Course, Cursusnaam = Coursename, Medewerker = Teachernr)
dataCourse <- within(dataCourse,rm("Teacher-lastname"))
  

dataCourse2 <-read_excel("Project timetable/UT_courses_Osiris_with_teacher_2014-2015.xlsx", col_names = TRUE, col_types = NULL, skip = 3)
dataCourse2 <- rename(dataCourse2,Collegejaar = Collegeyear, Cursus = Course, Cursusnaam = Coursename, Medewerker = Teachernr)
dataCourse2 <- within(dataCourse2,rm("Teacher-lastname"))



dataCourse3 <-read_delim(file="Project timetable/Docentenoverzicht_2015_Osiris.csv", delim=",", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
dataCourse4 <-read_delim(file="Project timetable/Docentenoverzicht_2016_Osiris.csv", delim=",", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))

finalDataCourses <- rbind(dataCourse, dataCourse2,dataCourse3,dataCourse4) 

dataSaxion <- read.xlsx("Project timetable/All timetabling activities SAX 2013-2015.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
head(data0)


data0 <-read_delim(file="Project timetable/Activiteitenoverzicht_2013-2014_v2.csv", delim=",", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))



data0 <-data0 %>%
  mutate_all(~as.character(replace(.,grepl("^#",.),"abc"))) %>%
  unite(Hostkey,Hostkey,Hostkey_1) %>%
  mutate(Hostkey = stringr::str_extract_all(Hostkey,'\\d+')) %>%
  unnest(Hostkey) %>%
  group_by(.,Hostkey, `Tijd van`, `Tijd tot en met`, Datum) %>%
  distinct() %>%
  ungroup() %>%
  rename(.,Cursus = Hostkey) %>%
  mutate(.,Collegejaar = strsplit(Datum,"/")[[1]][1]) %>%
  transform(.,Collegejaar = as.numeric(Collegejaar)) %>%
  mutate(Tijd.van = as.POSIXct(Tijd.van,format="%H:%M:%S")) %>%
  mutate(Tijd.tot.en.met = as.POSIXct(Tijd.tot.en.met,format="%H:%M:%S")) %>%
  mutate(Tdiff = difftime(Tijd.tot.en.met,Tijd.van , units = "hours"))




##Only drop NA at Medewerker if you dont need them.
data0 <-data0%>% 
  left_join(dataCourse2)%>%
  drop_na(.,Medewerker)

##KPI's 

##Room 70% occupied assume that room needs a date to take place
data0 <- data0 %>%
  drop_na(.,Zaal.Activiteit) %>%
  drop_na(.,Datum) %>%
  arrange(Tijd.van,Tijd.tot.en.met)%>%
  group_by(Tijd.tot.en.met) %>%
  mutate(diff(Tijd.tot.en.met - Tijd.van, differences = as.numeric(diff,units=days)))
  

KPIRoom <- data0 %>%
  group_by(Zaal.Activiteit) %>% 
  summarize(count=n())  %>%
  mutate(K=sum(data0$Tdiff))





  





