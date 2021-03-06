library(DBI)
library(RPostgreSQL)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
library(openxlsx)
library(stringr)
library(ggplot2)

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



finalData <- finalData %>%
  mutate_all(~as.character(replace(.,grepl("^#",.),"abc"))) %>%
  unite(Hostkey,Hostkey,Hostkey_1) %>%
  mutate(Hostkey = stringr::str_extract_all(Hostkey,'\\d+')) %>%
  unnest(Hostkey) %>%
  group_by(.,Hostkey, `Tijd van`, `Tijd tot en met`, Datum) %>%
  distinct() %>%
  ungroup() %>%
  rename(.,Cursus = Hostkey) %>%
  transform(.,Datum = as.POSIXct(Datum)) %>%
  mutate(Collegejaar = format(Datum,"%Y")) %>%
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
  drop_na(.,Datum)
  

KPIRoom <- data0 %>%
  drop_na(Zaal.Activiteit) %>%
  drop_na(Tdiff) %>%
  group_by(Zaal.Activiteit) %>% 
  distinct(Datum,Tijd.tot.en.met,Tijd.tot.en.met,Tdiff) %>%
  summarize(Timeoccupied = sum(Tdiff)) %>%
  mutate(MaxhoursDay = length(unique(data0$Datum)) * 8) %>%
  mutate(MaxhoursFullday = length(unique(data0$Datum)) * 14) %>%
  mutate(OccupationDay = Timeoccupied/MaxhoursDay) %>%
  mutate(OccupationFullday = Timeoccupied/MaxhoursFullday)



Mosthours <- data0 %>%
  drop_na(Beschrijving.Activiteit) %>%
  group_by(Cursus) %>%
  filter(Activiteitstype == "WC" | Activiteitstype == "HC") %>%
  distinct(Cursus,Datum,Tijd.van,Beschrijving.Activiteit,Tdiff,Naam.Activiteit) %>%
  summarize(.,Timespend = sum(Tdiff))


## Get different years into data set, calculate building occupation for each building for each year

  Occupation2013 <- filter(finalData, Collegejaar == 2013)
  Occupation2014 <- filter(finalData, Collegejaar == 2014)
  Occupation2015 <- filter(finalData, Collegejaar == 2015)
  Occupation2016 <- filter(finalData, Collegejaar == 2016)
  Occupation2017 <- filter(finalData, Collegejaar == 2017)

  
  Occupation2013 <- Occupation2013 %>%
    drop_na(Zaal.Activiteit) %>%
    group_by(Zaal.Activiteit) %>%
    distinct(Datum, Tijd.van,Tijd.tot.en.met, Tdiff) %>%
    summarise(TimeOccupied = sum(Tdiff), count = n()) %>%
    ungroup(Zaal.Activiteit) %>%
    mutate(UD = length(unique(Occupation2013$Datum)) *8) %>%
    mutate(Year = 2013) %>%
    mutate(Occup = TimeOccupied / UD *100)

  Occupation2014 <- Occupation2014 %>%
    drop_na(Zaal.Activiteit) %>%
    group_by(Zaal.Activiteit) %>%
    distinct(Datum, Tijd.van,Tijd.tot.en.met, Tdiff) %>%
    summarise(TimeOccupied = sum(Tdiff), count = n()) %>%
    ungroup(Zaal.Activiteit) %>%
    mutate(UD = length(unique(Occupation2014$Datum)) *8) %>%
    mutate(Year = 2014) %>%
    mutate(Occup = TimeOccupied / UD *100)
  
  Occupation2015 <- Occupation2015 %>%
    drop_na(Zaal.Activiteit) %>%
    group_by(Zaal.Activiteit) %>%
    distinct(Datum, Tijd.van,Tijd.tot.en.met, Tdiff) %>%
    summarise(TimeOccupied = sum(Tdiff), count = n()) %>%
    ungroup(Zaal.Activiteit) %>%
    mutate(UD = length(unique(Occupation2015$Datum)) *8) %>%
    mutate(Year = 2015) %>%
    mutate(Occup = TimeOccupied / UD *100)
  
  Occupation2016 <- Occupation2016 %>%
    drop_na(Zaal.Activiteit) %>%
    group_by(Zaal.Activiteit) %>%
    distinct(Datum, Tijd.van,Tijd.tot.en.met, Tdiff) %>%
    summarise(TimeOccupied = sum(Tdiff), count = n()) %>%
    ungroup(Zaal.Activiteit) %>%
    mutate(UD = length(unique(Occupation2016$Datum)) *8) %>%
    mutate(Year = 2016) %>%
    mutate(Occup = TimeOccupied / UD *100)
  
  Occupation2017 <- Occupation2017 %>%
    drop_na(Zaal.Activiteit) %>%
    group_by(Zaal.Activiteit) %>%
    distinct(Datum, Tijd.van,Tijd.tot.en.met, Tdiff) %>%
    summarise(TimeOccupied = sum(Tdiff), count = n()) %>%
    ungroup(Zaal.Activiteit) %>%
    mutate(UD = length(unique(Occupation2017$Datum)) *8) %>%
    mutate(Year = 2017) %>%
    mutate(Occup = TimeOccupied / UD *100)
  
  
  
  BuildingOccupation2013 <- Occupation2013 
  BuildingOccupation2013 <- BuildingOccupation2013 %>%
    mutate(Zaal.Activiteit = substr(sub("ZZ ",'',Occupation2013$Zaal.Activiteit),1,2)) %>%
    group_by(Zaal.Activiteit) %>%
    summarise(TimeOccupied = sum(TimeOccupied)) %>%
    mutate(Year = 2013) %>%
    rename(Gebouw = Zaal.Activiteit)
    
  ##Multiple the n() by 640 because unique dates * 8 gives this value. 
  ##So there are 80 days where a room can be booked for 8 hours. For every room in a building we count
  ## this room can be booked 640 times. Thats why n() is multipled by 640 for MaxOccupation
  
  BuildingOccupation2014 <- Occupation2014 
  BuildingOccupation2014 <- BuildingOccupation2014 %>%
    mutate(Zaal.Activiteit = substr(sub("ZZ ",'',Occupation2014$Zaal.Activiteit),1,2)) %>%
    group_by(Zaal.Activiteit) %>%
    summarise(TimeOccupied = sum(TimeOccupied)) %>%
    mutate(Year = 2014) %>%
    rename(Gebouw = Zaal.Activiteit)
    
    
  BuildingOccupation2015 <- Occupation2015 
  BuildingOccupation2015 <- BuildingOccupation2015 %>%
    mutate(Zaal.Activiteit = substr(sub("ZZ ",'',Occupation2015$Zaal.Activiteit),1,2)) %>%
    group_by(Zaal.Activiteit) %>%
    summarise(TimeOccupied = sum(TimeOccupied)) %>%
    mutate(Year = 2015) %>%
    rename(Gebouw = Zaal.Activiteit)

  BuildingOccupation2016 <- Occupation2016 
  BuildingOccupation2016 <- BuildingOccupation2016 %>%
    mutate(Zaal.Activiteit = substr(sub("ZZ ",'',Occupation2016$Zaal.Activiteit),1,2)) %>%
    group_by(Zaal.Activiteit) %>%
    summarise(TimeOccupied = sum(TimeOccupied)) %>%
    mutate(Year = 2016) %>%
    rename(Gebouw = Zaal.Activiteit)

  BuildingOccupation2017 <- Occupation2017 
  BuildingOccupation2017 <- BuildingOccupation2017 %>%
    mutate(Zaal.Activiteit = substr(sub("ZZ ",'',Occupation2017$Zaal.Activiteit),1,2)) %>%
    group_by(Zaal.Activiteit) %>%
    summarise(TimeOccupied = sum(TimeOccupied)) %>%
    mutate(Year = 2017) %>%
    rename(Gebouw = Zaal.Activiteit)
  
 TotalOccupation =  rbind(Occupation2013,Occupation2014,Occupation2015,Occupation2016,Occupation2017)
 TotalBuildingOccupation = rbind(BuildingOccupation2013,BuildingOccupation2014,BuildingOccupation2015,BuildingOccupation2016,BuildingOccupation2017)


write.xlsx(TotalOccupation,"TotalOccupation.xlsx")
write.xlsx(TotalBuildingOccupation,"TotalBuildingOccupation.xlsx")


Activities2013 <- filter(finalData, Collegejaar == 2013)
Activities2014 <- filter(finalData, Collegejaar == 2014)
Activities2015 <- filter(finalData, Collegejaar == 2015)
Activities2016 <- filter(finalData, Collegejaar == 2016)
Activities2017 <- filter(finalData, Collegejaar == 2017)


Activities2013 <- Activities2013 %>%
  drop_na(Zaal.Activiteit) %>%
  drop_na(Activiteitstype) %>%
  group_by(Zaal.Activiteit, Activiteitstype) %>%
  distinct(Datum, Tijd.van,Tijd.tot.en.met, Activiteitstype) %>%
  select(Zaal.Activiteit,Activiteitstype) %>%
  summarize(Frequency = n()) %>%
  ungroup() %>%
  mutate(Zaal.Activiteit = substr(sub("ZZ ",'',Zaal.Activiteit),1,2)) %>%
  group_by(Zaal.Activiteit,Activiteitstype) %>%
  summarise(Frequency= sum(Frequency)) %>%
  ungroup(Zaal.Activiteit,Activiteitstype) %>%
  group_by(Zaal.Activiteit) %>%
  mutate(Total = sum(Frequency)) %>%
  mutate(Percentage = Frequency / Total *100) %>%
  mutate(Year = 2013)

Activities2014 <- Activities2014 %>%
  drop_na(Zaal.Activiteit) %>%
  drop_na(Activiteitstype) %>%
  group_by(Zaal.Activiteit, Activiteitstype) %>%
  distinct(Datum, Tijd.van,Tijd.tot.en.met, Activiteitstype) %>%
  select(Zaal.Activiteit,Activiteitstype) %>%
  summarize(Frequency = n()) %>%
  ungroup() %>%
  mutate(Zaal.Activiteit = substr(sub("ZZ ",'',Zaal.Activiteit),1,2)) %>%
  group_by(Zaal.Activiteit,Activiteitstype) %>%
  summarise(Frequency= sum(Frequency)) %>%
  ungroup(Zaal.Activiteit,Activiteitstype) %>%
  group_by(Zaal.Activiteit) %>%
  mutate(Total = sum(Frequency)) %>%
  mutate(Percentage = Frequency / Total *100) %>%
  mutate(Year = 2014)

Activities2015 <- Activities2015 %>%
  drop_na(Zaal.Activiteit) %>%
  drop_na(Activiteitstype) %>%
  group_by(Zaal.Activiteit, Activiteitstype) %>%
  distinct(Datum, Tijd.van,Tijd.tot.en.met, Activiteitstype) %>%
  select(Zaal.Activiteit,Activiteitstype) %>%
  summarize(Frequency = n()) %>%
  ungroup() %>%
  mutate(Zaal.Activiteit = substr(sub("ZZ ",'',Zaal.Activiteit),1,2)) %>%
  group_by(Zaal.Activiteit,Activiteitstype) %>%
  summarise(Frequency= sum(Frequency)) %>%
  ungroup(Zaal.Activiteit,Activiteitstype) %>%
  group_by(Zaal.Activiteit) %>%
  mutate(Total = sum(Frequency)) %>%
  mutate(Percentage = Frequency / Total *100) %>%
  mutate(Year = 2015)

Activities2016 <- Activities2016 %>%
  drop_na(Zaal.Activiteit) %>%
  drop_na(Activiteitstype) %>%
  group_by(Zaal.Activiteit, Activiteitstype) %>%
  distinct(Datum, Tijd.van,Tijd.tot.en.met, Activiteitstype) %>%
  select(Zaal.Activiteit,Activiteitstype) %>%
  summarize(Frequency = n()) %>%
  ungroup() %>%
  mutate(Zaal.Activiteit = substr(sub("ZZ ",'',Zaal.Activiteit),1,2)) %>%
  group_by(Zaal.Activiteit,Activiteitstype) %>%
  summarise(Frequency= sum(Frequency)) %>%
  ungroup(Zaal.Activiteit,Activiteitstype) %>%
  group_by(Zaal.Activiteit) %>%
  mutate(Total = sum(Frequency)) %>%
  mutate(Percentage = Frequency / Total *100) %>%
  mutate(Year = 2016)

Activities2017 <- Activities2017 %>%
  drop_na(Zaal.Activiteit) %>%
  drop_na(Activiteitstype) %>%
  group_by(Zaal.Activiteit, Activiteitstype) %>%
  distinct(Datum, Tijd.van,Tijd.tot.en.met, Activiteitstype) %>%
  select(Zaal.Activiteit,Activiteitstype) %>%
  summarize(Frequency = n()) %>%
  ungroup() %>%
  mutate(Zaal.Activiteit = substr(sub("ZZ ",'',Zaal.Activiteit),1,2)) %>%
  group_by(Zaal.Activiteit,Activiteitstype) %>%
  summarise(Frequency= sum(Frequency)) %>%
  ungroup(Zaal.Activiteit,Activiteitstype) %>%
  group_by(Zaal.Activiteit) %>%
  mutate(Total = sum(Frequency)) %>%
  mutate(Percentage = Frequency / Total *100) %>%
  mutate(Year = 2017)

TotalActivities = rbind(Activities2013,Activities2014,Activities2015,Activities2016,Activities2017)
write.xlsx(TotalActivities,"TotalActivities.xlsx")