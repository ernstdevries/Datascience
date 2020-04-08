library(DBI)
library(RPostgreSQL)
library(lubridate)
library(readxl)
library(openxlsx)
library(chron)
library(tidyverse)

data0 <-read_delim(file="Activiteitenoverzicht_2013-2014_v2 (1)/Activiteitenoverzicht_2013-2014_v2.csv", delim=",", col_names = TRUE, locale = locale(encoding = 'LATIN1'))
data1 <-read_delim(file="Activiteitenoverzicht_2014-2015_v2/Activiteitenoverzicht_2014-2015_v2.csv", delim=",", col_names = TRUE, locale = locale(encoding = 'LATIN1'))
data2 <-read_delim(file="Activiteitenoverzicht_2015-2016_v2/Activiteitenoverzicht_2015-2016_v2.csv", delim=",", col_names = TRUE, locale = locale(encoding = 'LATIN1'))
data3 <-read_delim(file="Activiteitenoverzicht_2016-2017_v2/Activiteitenoverzicht_2016-2017_v2.csv", delim=",", col_names = TRUE, locale = locale(encoding = 'LATIN1'))
data4 <- read_excel("overview of programs and abbreviations.xlsx", col_names = TRUE, col_types = NULL)

#Appending all of the Utwente Activity data to one dataset so that they are together
data5 <- rbind(data0, data1, data2, data3) 

#Combine both the Hostkey columns in the Utwente Activity data so that there is only one column containing
#only the course codes. Additionally, change the format of the Data Time columns. Finally, create a new columns
#containing the length of each activity and replace all NA's with 0's.
data5 <-data5 %>%
  mutate_all(~as.character(replace(.,grepl("^#",.),"abc"))) %>%
  unite(Hostkey,Hostkey,Hostkey_1) %>%
  mutate(Hostkey = stringr::str_extract_all(Hostkey,'\\d+')) %>%
  unnest(Hostkey) %>%
  group_by(.,Hostkey, `Tijd van`, `Tijd tot en met`, Datum) %>%
  distinct() %>%
  ungroup() %>%
  rename(.,Cursus = Hostkey) %>%
  filter(Cursus > 10000000) %>%
  mutate(Collegejaar = year(Datum)) %>%
  transform(Collegejaar = as.numeric(Collegejaar)) %>%
  mutate(Date = gsub(" 00:00:00", "",Datum)) %>%
  select(-c(Datum)) %>%
  mutate(Tijd.van = as.POSIXct(Tijd.van,format="%H:%M:%S")) %>%
  transform(Tijd.van = make_datetime(year(Date), month(Date), day(Date), hour(Tijd.van), minute(Tijd.van), second(Tijd.van))) %>%
  mutate(Tijd.tot.en.met = as.POSIXct(Tijd.tot.en.met,format="%H:%M:%S")) %>%
  transform(Tijd.tot.en.met = make_datetime(year(Date), month(Date), day(Date), hour(Tijd.tot.en.met), minute(Tijd.tot.en.met), second(Tijd.tot.en.met))) %>%
  mutate(Tdiff = difftime(Tijd.tot.en.met,Tijd.van , units = "hours")) %>%
  replace_na(list(Tdiff = 0))

#Change the type of the classsize column, as well as the course code column to numeric.Then, filter out
#wrong course codes and add a new column containing only the study programme abbreviation that the 
#activity is belonging to
data5 <- data5 %>%
  mutate(Size = as.numeric(Grootte)) %>%
  select(-c(Grootte)) %>%
  mutate(Coursecode = as.numeric(Cursus)) %>%
  select(-c(Cursus)) %>%
  mutate(Programme = gsub( " .*$", "", Naam.Activiteit )) %>%
  mutate(Activity = as.factor(Activiteitstype)) %>%
  select(-c(Activiteitstype))

#Based on the study programme abbreviation column join the activity data with the study programmes data, so
#that there are now also the column containing the full programme name for each activity.
UtwenteActivity <- data5 %>%
  full_join(data4, by = c("Programme" = "Abbreviation"))

temp<- UtwenteActivity %>%
  filter(Coursecode < 10000000)

#Create a new table containing the contact hours the student have with the teacher on each date grouped by 
#study programme. Add new columns indicating whether the number of contact hours is below 4 hours or more
#than 6 hours, reflecting the KPI's that were determined.
UtwenteStudentContactHours <- UtwenteActivity %>%
  group_by(Date, Programme) %>%
  summarise(contact_hours = sum(Tdiff, na.rm = TRUE), Size = sum(Size, na.rm = TRUE)) %>%
  filter(Size != 0) %>%
  filter(contact_hours != 0) %>%
  mutate(contact_hours_below_4_hours = if_else(contact_hours < 4, TRUE, FALSE)) %>%
  mutate(contact_hours_above_6_hours = if_else(contact_hours > 6, TRUE, FALSE))

levels(data5$Activity)

#Get the number, as well as the percentage of student that have contact hours below 4 hours
countBelow4 <- sum(UtwenteStudentContactHours$contact_hours_below_4_hours)
percentageBelow4 <- (countBelow4 / length(UtwenteStudentContactHours$contact_hours_below_4_hours)) * 100
cat("Number of student with less than 4 contact hours per day: ", countBelow4)
cat("Percentage of student with less than 4 contact hours per day: ", percentageBelow4)

#Get the number, as well as the percentage of student that have contact hours over 6 hours
countAbove6 <- sum(UtwenteStudentContactHours$contact_hours_above_6_hours)
percentageAbove6 <- (countAbove6 / length(UtwenteStudentContactHours$contact_hours_above_6_hours)) * 100
cat("Number of student with more than 6 contact hours per day: ", countAbove6)
cat("Percentage of student with more than 6 contact hours per day: ", percentageAbove6)

#Make a new table with the college hours of each student by taking the difference between the end of the last session -
#the start of the first session for each day by study programme
UtwenteStudentCollegeHours <- UtwenteActivity %>%
  group_by(Date, Programme) %>%
  summarise(latestClass = max(Tijd.van, na.rm = TRUE),earliestClass = min(Tijd.van, na.rm = TRUE), Size = sum(Size, na.rm = TRUE)) %>%
  filter(Size != 0) %>%
  mutate(latestTime = latestClass + 6300) %>%
  mutate(collegeHours = difftime(latestTime,earliestClass , units = "hours")) %>%
  filter(collegeHours != 0) %>%
  mutate(college_hours_over_8.15 = if_else(collegeHours > 8.15, TRUE, FALSE)) %>%
  mutate(lastHour = if_else(hour(latestClass) == 15, TRUE, FALSE)) %>%
  mutate(firstHour = if_else(hour(earliestClass) == 8, TRUE, FALSE)) %>%
  mutate(lastAndFirst = if_else(lastHour == TRUE & firstHour == TRUE, TRUE, FALSE)) %>%
  mutate(weekDay = wday(Date)) %>%
  mutate(classesOnFriday = if_else(weekDay == 5, TRUE, FALSE)) %>%
  mutate(fridayAndEveningClasses = if_else(classesOnFriday == TRUE & hour(latestClass) == 15, TRUE, FALSE))

#Get the number, as well as the percentage of student that have college hours over 8.15 hours
countAbove815 <- sum(UtwenteStudentCollegeHours$college_hours_over_8.15)
percentageAbove815 <- (countAbove815 / length(UtwenteStudentCollegeHours$college_hours_over_8.15)) * 100
cat("Number of student with more than 8.15 college hours per day: ", countAbove815)
cat("Percentage of student with more than 8.15 college hours per day: ", percentageAbove815)

countFirstAndLast <- length(UtwenteStudentCollegeHours$lastAndFirst[UtwenteStudentCollegeHours$lastAndFirst == TRUE])
percentageFirstAndLast <- (countFirstAndLast / length(UtwenteStudentCollegeHours$lastAndFirst)) * 100
cat("Number of student with classes on the first and last hours: ", countFirstAndLast)
cat("Percentage of student with classes on the first and last hours: ", percentageFirstAndLast)

countFridayAndEveningClasses <- length(UtwenteStudentCollegeHours$fridayAndEveningClasses[UtwenteStudentCollegeHours$fridayAndEveningClasses == TRUE])
percentageFridayAndEveningClasses <- (countFridayAndEveningClasses / length(UtwenteStudentCollegeHours$weekDay[UtwenteStudentCollegeHours$weekDay == 5])) * 100
cat("Number of student with classes on friday and the last hours: ", countFridayAndEveningClasses)
cat("Percentage of student with classes on friday and the last hours out of all friday classes: ", percentageFridayAndEveningClasses)

data6 <-read_excel("UT_courses_Osiris_with_teacher_2013-2014.xlsx", col_names = TRUE, col_types = NULL, skip = 3)
data7 <-read_excel("UT_courses_Osiris_with_teacher_2014-2015.xlsx", col_names = TRUE, col_types = NULL, skip = 3)

#Append the Utwente teacher data into one table
UtwenteCourses <- rbind(data6, data7) 
UtwenteCourses <- UtwenteCourses %>%
  mutate(CourseCode = as.numeric(Course)) %>%
  select(-c(Course)) 

UtwenteTeacherActivity <- UtwenteActivity %>%
  full_join(UtwenteCourses, by = c("Coursecode" = "CourseCode"))

UtwenteTeacherContactHours <- UtwenteTeacherActivity %>%
  select(`Teacher-lastname`, Date, Size, Tdiff, Tijd.van, Tijd.tot.en.met) %>%
  filter(Size != 0)%>%
  filter(Tdiff != 0)%>%
  filter(!is.na(`Teacher-lastname`))

UtwenteTeacherContactHours <- distinct(UtwenteTeacherContactHours, Tdiff, Date, Tijd.van, Tijd.tot.en.met, .keep_all = TRUE)

UtwenteTeacherContactHours2 <- UtwenteTeacherContactHours %>%
  group_by(Date,`Teacher-lastname`) %>%
  summarise(latestClass = max(Tijd.van, na.rm = TRUE),earliestClass = min(Tijd.van, na.rm = TRUE)) %>%
  mutate(lastHour = if_else(hour(latestClass) == 15, TRUE, FALSE)) %>%
  mutate(firstHour = if_else(hour(earliestClass) == 8, TRUE, FALSE)) %>%
  mutate(lastAndFirst = if_else(lastHour == TRUE & firstHour == TRUE, TRUE, FALSE)) 

agg <- aggregate(UtwenteTeacherContactHours$Tdiff, by=list(Name = UtwenteTeacherContactHours$`Teacher-lastname`, Date = UtwenteTeacherContactHours$Date), FUN =sum)
agg <- agg %>%
  mutate(over8Hours = if_else(x > 8, TRUE, FALSE))

countTeacherAbove8 <- sum(agg$over8Hours)
percentageTeacherAbove8 <- (countTeacherAbove8 / length(agg$over8Hours)) * 100
cat("Number of times teachers had to workmore than 8 hours per day: ", countTeacherAbove8)
cat("Percentage of all days teachers had to work over 8 hours per day: ", percentageTeacherAbove8)

countTeacherFirstAndLast <- length(UtwenteTeacherContactHours2$lastAndFirst[UtwenteTeacherContactHours2$lastAndFirst == TRUE])
percentageTeacherFirstAndLast <- (countTeacherFirstAndLast / length(UtwenteTeacherContactHours2$lastAndFirst)) *100
cat("Number of times teachers have classes on the first and last hours: ", countTeacherFirstAndLast)
cat("Percentage of times teachers have classes on the first and last hours: ", percentageTeacherFirstAndLast)

UtwenteRoomActivity <- UtwenteActivity %>%
  filter(Size != 0)%>%
  filter(Tdiff != 0)%>%
  drop_na(Zaal.Activiteit) %>%
  select(Zaal.Activiteit, Date, Tdiff)

UtwenteRoomActivity <- distinct(UtwenteRoomActivity, Tdiff, Date, Zaal.Activiteit, .keep_all = TRUE)

agg2 <- aggregate(UtwenteRoomActivity$Tdiff, by=list(Date = UtwenteRoomActivity$Date, Room = UtwenteRoomActivity$Zaal.Activiteit), FUN = sum)

SaxionActivity <- read.xlsx("All timetabling activities SAX 2013-2015 (1).xlsx", sheet = 1, startRow = 1, colNames = TRUE)
SaxionCopy <- SaxionActivity

SaxionCopy$START <- hm(format(as.POSIXct((SaxionCopy$START) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"))
SaxionCopy$END <- hm(format(as.POSIXct((SaxionCopy$END) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"))
SaxionCopy$DATE <- as.Date((SaxionCopy$DATE), origin = "1900-01-01")
SaxionCopy <- distinct(SaxionCopy)

SaxionCopy <- SaxionCopy %>%
  transform(START = make_datetime(year(DATE), month(DATE), day(DATE), hour(START), minute(START))) %>%
  transform(END = make_datetime(year(DATE), month(DATE), day(DATE), hour(END), minute(END))) %>%
  mutate(Tdiff = difftime(END, START , units = "hours")) %>%
  group_by(BISONCODE) %>%
  summarise(contact_hours = sum(Tdiff, na.rm = TRUE)) %>%
  filter(contact_hours != 0) %>%
  mutate(contact_hours_below_4_hours = if_else(contact_hours < 4, TRUE, FALSE)) %>%
  mutate(contact_hours_above_6_hours = if_else(contact_hours > 6, TRUE, FALSE))
