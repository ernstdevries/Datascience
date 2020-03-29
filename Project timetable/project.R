library(DBI)
library(RPostgreSQL)
library(readr)
library(dplyr)
library(lubridate)

activityData <- read_delim(file="Project timetable/Activiteitenoverzicht_2013-2014_v2.csv",delim=",",locale=locale(encoding="ISO-8859-1"),col_name=TRUE,col_types=NULL)
teachersData <- read_delim(file="Project timetable/UT_courses_Osiris_with_teacher_2013-2014.csv",delim=";",locale=locale(encoding = "ISO-8859-1"), col_name=TRUE, col_types=NULL)

activityData <-activityData %>%
  mutate_all(~as.character(replace(.,grepl("^#",.),NA))) %>%
  mutate(Hostkey = coalesce(Hostkey_1))


activity <- activityData %>%
  select("Hostkey","Datum","Tijd van","Tijd tot en met") %>%
  rename(courseNumber = "Hostkey",timeStart = "Tijd van" , timeEnd = "Tijd tot en met") %>%
  arrange(courseNumber,timeStart,timeEnd) %>%
  group_by(courseNumber, timeStart, timeEnd) %>%
  distinct() %>%
  ungroup() %>%
  mutate(activityID = row_number())


  

teachers <- teachersData %>%
  select("Course","Teachernr") %>%
  rename(courseNumber = "Course",teacherNr = "Teachernr") %>%
  arrange(courseNumber,teacherNr) %>%
  group_by(courseNumber, teacherNr) %>%
  distinct() %>%
  ungroup() %>%
  mutate(teacherID = row_number())

overview <- activity %>%
  select(courseNumber,timeStart,timeEnd)
  full_join(teachers,overview)

  