library(DBI)
library(RPostgreSQL)
library(lubridate)
library(readxl)
library(openxlsx)
library(chron)
library(tidyverse)
library(ggplot2)
library(zoo)

#-------------------------Reading and cleaning the Utwente data------------------------------

data0 <-read_delim(file="Activiteitenoverzicht_2013-2014_v2.csv", delim=",", col_names = TRUE, locale = locale(encoding = 'LATIN1'))
data1 <-read_delim(file="Activiteitenoverzicht_2014-2015_v2.csv", delim=",", col_names = TRUE, locale = locale(encoding = 'LATIN1'))
data2 <-read_delim(file="Activiteitenoverzicht_2015-2016_v2.csv", delim=",", col_names = TRUE, locale = locale(encoding = 'LATIN1'))
data3 <-read_delim(file="Activiteitenoverzicht_2016-2017_v2.csv", delim=",", col_names = TRUE, locale = locale(encoding = 'LATIN1'))
data4 <- read_excel("overview of programs and abbreviations (3).xlsx", col_names = TRUE, col_types = NULL)

#Appending all of the Utwente Activity data to one dataset so that they are together
data5 <- rbind(data0, data1, data2, data3) 

#Combine both the Hostkey columns in the Utwente Activity data so that there is only one column containing
#only the course codes. Additionally, change the format of the Data Time columns. Finally, create a new columns
#containing the length of each activity called Tdiff and replace all NA's with 0's.
data5 <-data5 %>%
  mutate_all(~as.character(replace(.,grepl("^#",.),"abc"))) %>%
  unite(Hostkey,Hostkey,Hostkey_1) %>%
  mutate(Hostkey = stringr::str_extract_all(Hostkey,'\\d+')) %>%
  unnest(Hostkey) %>%
  group_by(.,Hostkey, `Tijd van`, `Tijd tot en met`, Datum) %>%
  distinct() %>%
  ungroup() %>%
  rename(.,Cursus = Hostkey) %>%
  mutate(Collegejaar = year(Datum)) %>%
  transform(Collegejaar = as.numeric(Collegejaar)) %>%
  mutate(Date = gsub(" 00:00:00", "",Datum)) %>% #Get rid o the time in the date
  select(-c(Datum)) %>%
  mutate(Tijd.van = as.POSIXct(Tijd.van,format="%H:%M:%S")) %>% # This will give a string, so it still needs to be converted to datetime
  transform(Tijd.van = make_datetime(year(Date), month(Date), day(Date), hour(Tijd.van), minute(Tijd.van), second(Tijd.van))) %>% #Also add the year to make ordering easier
  mutate(Tijd.tot.en.met = as.POSIXct(Tijd.tot.en.met,format="%H:%M:%S")) %>%
  transform(Tijd.tot.en.met = make_datetime(year(Date), month(Date), day(Date), hour(Tijd.tot.en.met), minute(Tijd.tot.en.met), second(Tijd.tot.en.met))) %>%
  mutate(Tdiff = difftime(Tijd.tot.en.met,Tijd.van , units = "hours")) %>% #The length of each class is calculated
  replace_na(list(Tdiff = 0))

#Change the type of the classsize column, as well as the course code column to numeric.Then, filter out
#wrong course codes and add a new column containing only the study programme abbreviation that the 
#activity is belonging to
data5 <- data5 %>%
  mutate(Size = as.numeric(Grootte)) %>%
  select(-c(Grootte)) %>%
  mutate(Coursecode = as.numeric(Cursus)) %>%
  select(-c(Cursus)) %>%
  #filter(Coursecode > 10000000) %>% #Remove invalid course coded like "2" for example
  mutate(Programme = gsub( " .*$", "", Naam.Activiteit )) %>% #Get the abbreviations and put them into a seperate column. Late used to join with abbreviations excel file
  mutate(Activity = as.factor(Activiteitstype)) %>%
  select(-c(Activiteitstype))

#Based on the study programme abbreviation column join the activity data with the study programmes data, so
#that there are now also the column containing the full programme name for each activity.
UtwenteActivity <- data5 %>%
  full_join(data4, by = c("Programme" = "Abbreviation"))  # Remove unnecessary duplicates


UtwenteActivity <- UtwenteActivity %>%
  mutate(level = if_else(str_detect(Naam.Activiteit, " M ") | str_detect(Naam.Activiteit, " M\\d ") | str_detect(Naam.Activiteit, " M. ") , "Master", "Bachelor"))# %>%
  #mutate(Byear1 = if_else)

UtwenteActivity <- UtwenteActivity %>%
  mutate(Byear1= if_else(str_detect(Naam.Activiteit, regex(" B1 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD01 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD1 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD02 ", ignore_case = TRUE) )| str_detect(Naam.Activiteit, regex(" MOD2 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD03 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD3 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD04 ", ignore_case = TRUE) )| str_detect(Naam.Activiteit, regex(" MOD4 ", ignore_case = TRUE)), TRUE,FALSE)) %>%
  mutate(Byear2= if_else(str_detect(Naam.Activiteit, regex(" B2 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD05 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD5 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD06 ", ignore_case = TRUE) )| str_detect(Naam.Activiteit, regex(" MOD6 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD07 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD7 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD08 ", ignore_case = TRUE) )| str_detect(Naam.Activiteit, regex(" MOD8 ", ignore_case = TRUE)), TRUE,FALSE)) %>%
  mutate(Byear3= if_else(str_detect(Naam.Activiteit, regex(" B3 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD09 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD9 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD10 ", ignore_case = TRUE) )| str_detect(Naam.Activiteit, regex(" MOD10 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD011 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD11 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD012 ", ignore_case = TRUE) )| str_detect(Naam.Activiteit, regex(" MOD12 ", ignore_case = TRUE)), TRUE,FALSE)) %>%
  mutate(Byear4= if_else(str_detect(Naam.Activiteit, regex(" B4 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD013 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD13 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD014 ", ignore_case = TRUE) )| str_detect(Naam.Activiteit, regex(" MOD14 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD015 ", ignore_case = TRUE)) | str_detect(Naam.Activiteit, regex(" MOD15 ", ignore_case = TRUE))| str_detect(Naam.Activiteit, regex(" MOD016 ", ignore_case = TRUE) )| str_detect(Naam.Activiteit, regex(" MOD16 ", ignore_case = TRUE)), TRUE,FALSE)) %>%
  mutate(years = if_else(Byear1 == TRUE, "b1", "")) %>%
  transform(years = if_else(Byear2 == TRUE, "b2", years))%>%
  transform(years = if_else(Byear3 == TRUE, "b3", years))%>%
  transform(years = if_else(Byear4 == TRUE, "b4", years))

UtwenteActivity <- UtwenteActivity %>%
  mutate(Myear2 = if_else(level == "Master" & str_detect(Naam.Activiteit, regex(" M2 ", ignore_case = TRUE)), TRUE, FALSE)) %>%
  filter(!str_detect(Naam.Activiteit, regex(" M3 ", ignore_case = TRUE))) %>%
  mutate(Myear1 = if_else(level == "Master" & Myear2 == FALSE, TRUE, FALSE)) %>%
  transform(years = if_else(Myear1 == TRUE, "m1", years)) %>%
  transform(years = if_else(Myear2 == TRUE, "m2", years)) %>%
  transform(years = if_else(years == "", "Remove", years)) %>%
  filter(years != "Remove") %>%
  drop_na(Date) %>%
  distinct(Tijd.van, years, Programme,  .keep_all = TRUE)

Dates <- UtwenteActivity %>%
  select(Date)%>%
  distinct(Date)

print(nrow(Dates))
numberDays <- nrow(Dates)





#-----------------------------------------------KPI-------------------------------------------------







#--------------------------------------Utwente Student Break KPI-------------------------------------

#Get all the student breaks by calculating the time between 2 session for students from the same programme at the same day.

UtwenteBreaks <- UtwenteActivity %>%
  #2 times arrange after each other is necessary. If only the second arrange is used it could be that for each course on each day
  #the sessions are still not ordered correctly. For example in Tijd.van the first session starts at 10:30 while the second session
  #starts at 8:45, even though they are on the same day for the same course. Because of this, before this arrange is used, another arrange
  #is used to ensure that all Tijd.van values are in the correct order.
  arrange(Tijd.van, Coursecode) %>%
  arrange(Date,  Programme, years) %>%
  mutate(Nextyear = lead(years, 1)) %>%
  mutate(NextSession = lead(Tijd.van, 1)) %>% #Put the start of the next session into a seperate column. For calculating breaks inbetween them.
  mutate(NextDate = lead(Date, 1)) %>% #Put the date of the next entry in a seperate column. Break is only calculated if next entry is on the same day.
  mutate(NextCourse = lead(Programme, 1)) %>% #Get the programme the next entry is belonging to and put it into a column. Breaks are only calculated per study programme.
  mutate(Break = if_else(Date != NextDate | Programme != NextCourse | Nextyear != years, 0, abs(as.numeric(difftime(NextSession, Tijd.tot.en.met, units = "hours"))))) 

#Make a new table containing information about the breaks for each programme per day.
UtwenteBreaksSummary <- UtwenteBreaks %>%
  mutate(BreaksOver2Hours = if_else(Break >= 2, TRUE, FALSE)) %>%
  group_by(Date, Programme, years) %>%
  summarise(timesOver2Hours = length(BreaksOver2Hours[BreaksOver2Hours]))

#Get the percentage of times a study programme has more than 2 hours break per day
MoreThan2FreeHours <- length(UtwenteBreaksSummary$timesOver2Hours[UtwenteBreaksSummary$timesOver2Hours > 0])
percentageMoreThan2FreeHours <- (MoreThan2FreeHours / length(UtwenteBreaksSummary$timesOver2Hours)) * 100
breakHours <- sum(UtwenteBreaks$Break, na.rm = TRUE)
cat("Number of days a study cohort participating in a course have breaks longer than 2 hours: ", MoreThan2FreeHours)
cat("Number of break hours the students participating in a course have: ", breakHours)
cat("Percentage of days a study cohort participating in a study programme have breaks longer than 2 hours: ", percentageMoreThan2FreeHours)


#-----------------------------Utwente Student Contact Hours---------------------------------


#Create a new table containing the contact hours the student have with the teacher on each date grouped by 
#study programme. Add new columns indicating whether the number of contact hours is below 4 hours or more
#than 6 hours, reflecting the KPI's that were determined.
UtwenteStudentContactHours <- UtwenteActivity %>%
  group_by(Date, Programme, years) %>%
  summarise(contact_hours = sum(Tdiff), Size = sum(Size)) %>%
  filter(contact_hours != 0) %>%
  mutate(contact_hours_below_4_hours = if_else(contact_hours < 4, TRUE, FALSE)) %>%
  mutate(contact_hours_above_6_hours = if_else(contact_hours > 6, TRUE, FALSE))

#Get  the percentage of times student that have contact hours below 4 hours
countBelow4 <- sum(UtwenteStudentContactHours$contact_hours_below_4_hours)
percentageBelow4 <- (countBelow4 / length(UtwenteStudentContactHours$contact_hours_below_4_hours)) * 100
cat("Number of days a programme cohort had less than 4 contact hours per day for all days where there are classes: ", countBelow4)
cat("Percentage of days a programme cohort had less than 4 contact hours per day for all days where there are classes: ", percentageBelow4)

#Get the percentage of times student that have contact hours over 6 hours
countAbove6 <- sum(UtwenteStudentContactHours$contact_hours_above_6_hours)
percentageAbove6 <- (countAbove6 / length(UtwenteStudentContactHours$contact_hours_above_6_hours)) * 100
#cat("Number of student with more than 6 contact hours per day for all days where there are classes: ", countAbove6)
cat("Percentage of days a programme cohort more than 6 contact hours per day for all days where there are classes: ", percentageAbove6)


#---------------------------------------------Utwente Student College Hours-------------------------------------


#Make a new table with the college hours of each student by taking the difference between the end of the last session -
#the start of the first session for each day by study programme. College hours are therefore defined as time spent by
#students at the university whereas contact hours are defined as time students spent in classes. College hours also include
#breaks. Also make column for times student have classes on fridays at the last hours.
UtwenteStudentCollegeHours <- UtwenteActivity %>%
  group_by(Date, Programme, years) %>%
  summarise(latestClass = max(Tijd.tot.en.met, na.rm = TRUE),earliestClass = min(Tijd.van, na.rm = TRUE), Size = sum(Size, na.rm = TRUE)) %>%
    mutate(latestTime = latestClass + 6300) %>%
  mutate(collegeHours = difftime(latestClass,earliestClass , units = "hours")) %>%
  filter(collegeHours != 0) %>%
  mutate(college_hours_over_8.75 = if_else(collegeHours > 8.75, TRUE, FALSE)) %>%
  mutate(lastHour = if_else(hour(latestClass) == 15, TRUE, FALSE)) %>%
  mutate(firstHour = if_else(hour(earliestClass) == 8, TRUE, FALSE)) %>%
  mutate(lastAndFirst = if_else(lastHour == TRUE & firstHour == TRUE, TRUE, FALSE)) %>%
  mutate(weekDay = wday(Date)) %>%
  mutate(classesOnFriday = if_else(weekDay == 5, TRUE, FALSE)) %>%
  mutate(fridayAndEveningClasses = if_else(classesOnFriday == TRUE & hour(latestClass) == 15, TRUE, FALSE))

#Get the percentage of times student that have college hours over 8.15 hours
countAbove815 <- sum(UtwenteStudentCollegeHours$college_hours_over_8.75)
percentageAbove815 <- (countAbove815 / length(UtwenteStudentCollegeHours$college_hours_over_8.75)) * 100
#cat("Number of student with more than 8.15 college hours per day: ", countAbove815)
cat("Percentage of days a study cohort with more than 8.75 college hours per day: ", percentageAbove815)

#Get the percentage of times students have classes on the first as well as the last hours
countFirstAndLast <- length(UtwenteStudentCollegeHours$lastAndFirst[UtwenteStudentCollegeHours$lastAndFirst == TRUE])
percentageFirstAndLast <- (countFirstAndLast / length(UtwenteStudentCollegeHours$lastAndFirst)) * 100
#cat("Number of times students have classes on the first and last hours (for all days where there are classes): ", countFirstAndLast)
cat("Percentage of times students have classes on the first and last hours (for all days where there are classes): ", percentageFirstAndLast)

#Get the percentage of times students have evening classes on fridays
countFridayAndEveningClasses <- length(UtwenteStudentCollegeHours$fridayAndEveningClasses[UtwenteStudentCollegeHours$fridayAndEveningClasses == TRUE])
percentageFridayAndEveningClasses <- (countFridayAndEveningClasses / length(UtwenteStudentCollegeHours$weekDay[UtwenteStudentCollegeHours$weekDay == 5])) * 100
#cat("Number of student with classes on friday and the last hours (for all days where there are classes): ", countFridayAndEveningClasses)
cat("Percentage of times student with classes on friday and the last hours out of all friday classes: ", percentageFridayAndEveningClasses)

#Read the teacher data
data6 <-read_excel("UT_courses_Osiris_with_teacher_2013-2014.xlsx", col_names = TRUE, col_types = NULL, skip = 3)
data7 <-read_excel("UT_courses_Osiris_with_teacher_2014-2015.xlsx", col_names = TRUE, col_types = NULL, skip = 3)


#-----------------------------------Utwente Teacher Working Hours KPIS---------------------------------


#Append the Utwente teacher data into one table
UtwenteCourses <- rbind(data6, data7) 
UtwenteCourses <- UtwenteCourses %>%
  mutate(CourseCode = as.numeric(Course)) %>%
  select(-c(Course)) 

#Join the course table with the teachers table to have all of the activities of the teachers for each day
UtwenteTeacherActivity <- UtwenteActivity %>%
  full_join(UtwenteCourses, by = c("Coursecode" = "CourseCode"))

#Make a new table for the teachers that includes only filtered information with unnecessay information excluded
UtwenteTeacherContactHours <- UtwenteTeacherActivity %>%
  select(`Teacher-lastname`, Date, Size, Tdiff, Tijd.van, Tijd.tot.en.met) %>% # Classes with group size 0 should not be included
  filter(Tdiff != 0)%>% # Class duration of 0 should also not be included
  filter(!is.na(`Teacher-lastname`)) # Missing teacher entries should be filtered out

#Remove unnessesary duplicates
UtwenteTeacherContactHours <- distinct(UtwenteTeacherContactHours, Tdiff, Date, Tijd.van, Tijd.tot.en.met, .keep_all = TRUE)

#Make a new table containing summatised information of the teachers like latest class and earliest class. Check if the teacher
#has to work on the last hour, first hour,and last and first hour
UtwenteTeacherContactHours2 <- UtwenteTeacherContactHours %>%
  group_by(Date,`Teacher-lastname`) %>%
  summarise(latestClass = max(Tijd.van, na.rm = TRUE),earliestClass = min(Tijd.van, na.rm = TRUE)) %>%
  mutate(lastHour = if_else(hour(latestClass) == 15, TRUE, FALSE)) %>%
  mutate(firstHour = if_else(hour(earliestClass) == 8, TRUE, FALSE)) %>%
  mutate(lastAndFirst = if_else(lastHour == TRUE & firstHour == TRUE, TRUE, FALSE)) 

#Get the class duration for each teacher at each date
agg <- aggregate(UtwenteTeacherContactHours$Tdiff, by=list(Name = UtwenteTeacherContactHours$`Teacher-lastname`, Date = UtwenteTeacherContactHours$Date), FUN =sum)
#Check if the teacher has over 8 workhours
agg <- agg %>%
  mutate(over8Hours = if_else(x > 8, TRUE, FALSE))

#Get the percantage of days teachers had to work more than 8 hours per day
countTeacherAbove8 <- sum(agg$over8Hours)
percentageTeacherAbove8 <- (countTeacherAbove8 / length(agg$over8Hours)) * 100
#cat("Number of times teachers had to workmore than 8 hours per day: ", countTeacherAbove8)
cat("Percentage of all days teachers had to work over 8 hours per day: ", percentageTeacherAbove8)

#Get the percantage of times teachers had to work both on the last as well as on the first hour
countTeacherFirstAndLast <- length(UtwenteTeacherContactHours2$lastAndFirst[UtwenteTeacherContactHours2$lastAndFirst == TRUE])
percentageTeacherFirstAndLast <- (countTeacherFirstAndLast / length(UtwenteTeacherContactHours2$lastAndFirst)) *100
#cat("Number of times teachers have classes on the first and last hours: ", countTeacherFirstAndLast)
cat("Percentage of times teachers have classes on the first and last hours: ", percentageTeacherFirstAndLast)


#-------------------------------------Utwente Room Occupation-------------------------------

#Make a new table containing only relevat filtered information about th rooms used for classes
UtwenteRoomActivity <- UtwenteActivity %>%
  filter(Size != 0)%>% #Class size of 0 should not be included 
  filter(Tdiff != 0)%>%# class lengths of 0 should also not be included 
  drop_na(Zaal.Activiteit) %>% # Filter out missing class data
  select(Zaal.Activiteit, Date, Tdiff)

#Remove unnecessary duplicates
UtwenteRoomActivity <- distinct(UtwenteRoomActivity, Tdiff, Date, Zaal.Activiteit, .keep_all = TRUE)

#Give the class length for each class on each day
agg2 <- aggregate(UtwenteRoomActivity$Tdiff, by=list(Date = UtwenteRoomActivity$Date, Room = UtwenteRoomActivity$Zaal.Activiteit), FUN = sum)


#-------------------------------Reading and Cleaning Saxion Data--------------------------------------


SaxionActivity <- read.xlsx("All timetabling activities SAX 2013-2015.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
#Make a copyof the saxion data to make reruns faster, so that the original data doesn't have to be read every time
SaxionCopy <- SaxionActivity

#Put the date, as well as the start and end time of classes, into the right format.These will still be of type character,
#so they need to be transformed later
SaxionCopy$START <- hm(format(as.POSIXct((SaxionCopy$START) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"))
SaxionCopy$END <- hm(format(as.POSIXct((SaxionCopy$END) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"))
SaxionCopy$DATE <- as.Date((SaxionCopy$DATE), origin = "1900-01-01")
#Remove unnecessary duplicates
SaxionCopy <- distinct(SaxionCopy,DATE, START, CLASS, EDUC.CODE1,   .keep_all = TRUE)

#Make a new column containing the class sizes Tdiff, change types, add a column with the weekdays of each class,
#and dropmissing entries without EDUC.CODE
SaxionCopy <- SaxionCopy %>%
  #Transform the times into datetime instances
  transform(START = make_datetime(year(DATE), month(DATE), day(DATE), hour(START), minute(START))) %>%
  transform(END = make_datetime(year(DATE), month(DATE), day(DATE), hour(END), minute(END))) %>%
  mutate(Tdiff = difftime(END, START , units = "hours")) %>%
  transform(ACTIVITY = as.factor(ACTIVITY)) %>%
  mutate(WEEKDAY = wday(DATE)) %>%
  drop_na(EDUC.CODE1)

#Define a function that gets the n last characters of a string. Later used for filtering the V out of classes to get the
#full time classes
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#-----------------------------------------------Saxion Fulltime Students------------------------------------------------


#Get the fulltime students of saxion by using the previously defined function, so by finding the V in the classes column,
#as well as filtering out all student that have classes after 18:00
SaxionFulltimeStudents <- SaxionCopy %>%
  mutate(endFullTime = 0.75) %>%
  transform(endFullTime = hm(format(as.POSIXct(endFullTime * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M")))%>%
  transform(endFullTime = make_datetime(year(DATE), month(DATE), day(DATE), hour(endFullTime), minute(endFullTime))) %>%
  filter(END <= endFullTime) %>%
  mutate(Fulltime = if_else(str_detect(substrRight(CLASS, 3), "V"), TRUE, FALSE)) %>%
  filter(Fulltime == TRUE)

#Get summarised information about the length of classes only for relevant variables.
aggSax <- aggregate(SaxionFulltimeStudents$Tdiff, by=list(Date = SaxionFulltimeStudents$DATE, Bisoncode = SaxionFulltimeStudents$BISONCODE, Weekday = SaxionFulltimeStudents$WEEKDAY, Activity = SaxionFulltimeStudents$ACTIVITY, Week = SaxionFulltimeStudents$CALENDER_WEEK, Year = SaxionFulltimeStudents$SCHOOLYEAR, Class = SaxionFulltimeStudents$CLASS), FUN = sum)

#Add a counter to the summarised information to later use it to count and sum all classes in a course or study per week. This is needed for the check
#that classes are not spanning more than 4 days per week. This will be done per course, as well as per study
aggSax <- aggSax %>%
  #filter(Activity == 'L') %>%
  mutate(Counter = 1)
aggSax2 <- aggregate(aggSax$Counter, by=list(Bisoncode = aggSax$Bisoncode, Week = aggSax$Week, Year = aggSax$Year, Class = aggSax$Class), FUN = sum)

#Get the percentage of weeks for courses that span more than 4 days per week
#This number is unsurprisingly bery low, since single courses are very unlikely to be heldon each day of the week
WeeksWithMoreThan4Days = length(aggSax2$x[aggSax2$x > 4])
PercentageWeeksWithMoreThan4Days = (WeeksWithMoreThan4Days / length(aggSax2$x)) * 100
#cat("Number of weeks for students where lectures span more than 4 days (Per course): ", WeeksWithMoreThan4Days)
cat("Percentage of weeks for students where lectures span more than 4 days (Per Course): ", PercentageWeeksWithMoreThan4Days)

#Do the same summary for each study programme
aggSax <- aggregate(SaxionFulltimeStudents$Tdiff, by=list(Date = SaxionFulltimeStudents$DATE, Study = SaxionFulltimeStudents$EDUC.CODE1, Weekday = SaxionFulltimeStudents$WEEKDAY, Activity = SaxionFulltimeStudents$ACTIVITY, Week = SaxionFulltimeStudents$CALENDER_WEEK, Year = SaxionFulltimeStudents$SCHOOLYEAR, Class = SaxionFulltimeStudents$CLASS), FUN = sum)
aggSax <- aggSax %>%
  #filter(Activity == 'L') %>%
  mutate(Counter = 1)
aggSax2 <- aggregate(aggSax$Counter, by=list(Study = aggSax$Study, Week = aggSax$Week, Year = aggSax$Year, Class = aggSax$Class), FUN = sum)

##Get the percentage of weeks for study programmes that span more than 4 days per week
#This number is alot higher since its muchmore likely for study programmes to be held at each day of the week.
WeeksWithMoreThan4Days = length(aggSax2$x[aggSax2$x > 4])
PercentageWeeksWithMoreThan4Days = (WeeksWithMoreThan4Days / length(aggSax2$x)) * 100
cat("Number of weeks for students where lectures span more than 4 days (Per Study): ", WeeksWithMoreThan4Days)
cat("Percentage of weeks for students where lectures span more than 4 days (Per Study): ", PercentageWeeksWithMoreThan4Days)

#Get the college hours of the full time students at Saxion, so the time they spend there including the breaks. Check if the students
#have less than 4 college hours on any given day
SaxionStudentCollegeHours <- SaxionFulltimeStudents %>%
  group_by(DATE, EDUC.CODE1) %>%
  summarise(latestClass = max(END, na.rm = TRUE),earliestClass = min(START, na.rm = TRUE)) %>%
  mutate(collegeHours = (latestClass - earliestClass) / 3600) %>%
  mutate(lessThan4Hours = if_else(collegeHours < 4, TRUE, FALSE))

#Give the percentage of times students have less than 4 college hourson any given day with classes. 
CountStudentsLessThan4CollegeHours = length(SaxionStudentCollegeHours$lessThan4Hours[SaxionStudentCollegeHours$lessThan4Hours])
PercentageStudentsLessThan4CollegeHours = (CountStudentsLessThan4CollegeHours / length(SaxionStudentCollegeHours$lessThan4Hours)) * 100
#cat("Number of times student have less than 4 college hours out of all days where they have classes (Per Study): ", CountStudentsLessThan4CollegeHours)
cat("Percentage of times student have less than 4 college hours out of all days where they have classes (Per Study): ", PercentageStudentsLessThan4CollegeHours)

#-------------------------------------------------Saxion Part Time----------------------------------------------------------

SaxionParttimeStudents <- SaxionCopy %>%
  mutate(endFullTime = 0.75) %>%
  transform(endFullTime = hm(format(as.POSIXct(endFullTime * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M")))%>%
  transform(endFullTime = make_datetime(year(DATE), month(DATE), day(DATE), hour(endFullTime), minute(endFullTime))) %>%
  filter(END >= endFullTime) 

#Get summarised information about the length of classes only for relevant variables.
aggSaxP <- aggregate(SaxionParttimeStudents$Tdiff, by=list(Date = SaxionParttimeStudents$DATE, Bisoncode = SaxionParttimeStudents$BISONCODE, Weekday = SaxionParttimeStudents$WEEKDAY, Activity = SaxionParttimeStudents$ACTIVITY, Week = SaxionParttimeStudents$CALENDER_WEEK, Year = SaxionParttimeStudents$SCHOOLYEAR, Class = SaxionParttimeStudents$CLASS), FUN = sum)

#Add a counter to the summarised information to later use it to count and sum all classes in a course or study per week. This is needed for the check
#that classes are not spanning more than 4 days per week. This will be done per course, as well as per study
aggSaxP <- aggSaxP %>%
  #filter(Activity == 'L') %>%
  mutate(Counter = 1)
aggSaxP2 <- aggregate(aggSaxP$Counter, by=list(Bisoncode = aggSaxP$Bisoncode, Week = aggSaxP$Week, Year = aggSaxP$Year, Class = aggSaxP$Class), FUN = sum)

#Get the percentage of weeks for courses that span more than 4 days per week
#This number is unsurprisingly bery low, since single courses are very unlikely to be heldon each day of the week
WeeksWithMoreThan4Days = length(aggSaxP2$x[aggSax2$x > 4])
PercentageWeeksWithMoreThan4Days = (WeeksWithMoreThan4Days / length(aggSaxP2$x)) * 100
#cat("Number of weeks for students where lectures span more than 4 days (Per course): ", WeeksWithMoreThan4Days)
cat("Percentage of weeks for students where lectures span more than 4 days (Per Course): ", PercentageWeeksWithMoreThan4Days)

#Do the same summary for each study programme
aggSaxP <- aggregate(SaxionParttimeStudents$Tdiff, by=list(Date = SaxionParttimeStudents$DATE, Study = SaxionParttimeStudents$EDUC.CODE1, Weekday = SaxionParttimeStudents$WEEKDAY, Activity = SaxionParttimeStudents$ACTIVITY, Week = SaxionParttimeStudents$CALENDER_WEEK, Year = SaxionParttimeStudents$SCHOOLYEAR, Class = SaxionParttimeStudents$CLASS), FUN = sum)
aggSax <- aggSaxP %>%
  #filter(Activity == 'L') %>%
  mutate(Counter = 1)
aggSaxP2 <- aggregate(aggSaxP$Counter, by=list(Study = aggSaxP$Study, Week = aggSaxP$Week, Year = aggSaxP$Year, Class = aggSaxP$Class), FUN = sum)

##Get the percentage of weeks for study programmes that span more than 4 days per week
#This number is alot higher since its muchmore likely for study programmes to be held at each day of the week.
WeeksWithMoreThan4Days = length(aggSaxP2$x[aggSaxP2$x > 4])
PercentageWeeksWithMoreThan4Days = (WeeksWithMoreThan4Days / length(aggSaxP2$x)) * 100
cat("Number of weeks for students where lectures span more than 4 days (Per Study): ", WeeksWithMoreThan4Days)
cat("Percentage of weeks for students where lectures span more than 4 days (Per Study): ", PercentageWeeksWithMoreThan4Days)

#Get the college hours of the full time students at Saxion, so the time they spend there including the breaks. Check if the students
#have less than 4 college hours on any given day
SaxionParttimeStudentCollegeHours <- SaxionParttimeStudents %>%
  group_by(DATE, EDUC.CODE1) %>%
  summarise(latestClass = max(END, na.rm = TRUE),earliestClass = min(START, na.rm = TRUE)) %>%
  mutate(collegeHours = (latestClass - earliestClass) / 3600) %>%
  mutate(lessThan4Hours = if_else(collegeHours < 4, TRUE, FALSE))

#Give the percentage of times students have less than 4 college hourson any given day with classes. 
CountStudentsLessThan4CollegeHours = length(SaxionParttimeStudentCollegeHours$lessThan4Hours[SaxionParttimeStudentCollegeHours$lessThan4Hours])
PercentageStudentsLessThan4CollegeHours = (CountStudentsLessThan4CollegeHours / length(SaxionParttimeStudentCollegeHours$lessThan4Hours)) * 100
#cat("Number of times student have less than 4 college hours out of all days where they have classes (Per Study): ", CountStudentsLessThan4CollegeHours)
cat("Percentage of times student have less than 4 college hours out of all days where they have classes (Per Study): ", PercentageStudentsLessThan4CollegeHours)

#--------------------------------------------------Saxion Teachers-------------------------------------------------

#Create a dataset for teacher where entries with missing teacher data are removed and the information is aggregated by the impotant variables
#relate only to teachers
SaxionTeachers <- SaxionCopy %>%
  filter(!is.na(TEACHER))
aggSax3 <- aggregate(SaxionTeachers$Tdiff, by=list(Date = SaxionTeachers$DATE, TeacherName = SaxionTeachers$NAMEFULL), FUN = sum)

#------------------------------------------------Saxion Breaks-------------------------------------------------------------

#Make a column containing information abou the breaks a Saxion. These are caluclated by taking the difference in time
#between each study session if the next session is on the same day for the same study programme.
SaxionBreak <- SaxionCopy %>%
  arrange(START, CLASS) %>%
  arrange(DATE, EDUC.CODE1) %>%
  mutate(NextSession = lead(START, 1)) %>%
  mutate(NextDate = lead(DATE, 1)) %>%
  mutate(NextCourse = lead(EDUC.CODE1, 1)) %>%
  mutate(Break = if_else(DATE != NextDate | EDUC.CODE1 != NextCourse, 0 , abs(as.numeric(difftime(NextSession, END, units = "hours")))))

#Make a new table for breaksthat contains summarised information about the  breaks. For each study and data, the amount of breaks over
#2 hours should be shown
SaxionBreakSummary <- SaxionBreak %>%
  mutate(BreaksOver2Hours = if_else(Break >= 2, TRUE, FALSE)) %>%
  group_by(DATE, EDUC.CODE1) %>%
  summarise(timesOver2Hours = length(BreaksOver2Hours[BreaksOver2Hours]))

#Get the pecentage of times that each study has breaks that last longer than 2 hours.
MoreThan2FreeHours <- length(SaxionBreakSummary$timesOver2Hours[SaxionBreakSummary$timesOver2Hours >= 2])
percentageMoreThan2FreeHours <- (MoreThan2FreeHours / length(SaxionBreakSummary$timesOver2Hours)) * 100
breakHours <- sum(SaxionBreak$Break, na.rm = TRUE)
#cat("Number of times the students participating in a course have breaks longer than 2 hours: ", MoreThan2FreeHours)
#cat("Number of break hours the students participating in a course have: ", breakHours)
cat("Percentage of times the students participating in a course have breaks longer than 2 hours: ", percentageMoreThan2FreeHours)








#---------------------------------------------------Exploration--------------------------------------------
#Rank the study programmes by total number of college hours
RankProgrammeByCollegeHours <- UtwenteActivity %>%
  group_by(Programme) %>%
  summarise(Tdiff = sum(Tdiff)) %>%
  filter(Tdiff > 100) %>%
  drop_na() %>%
  arrange(desc(Tdiff))

#Rank the study programmes by class size
RankProgrammeByClassSize <-UtwenteActivity %>%
  group_by(Programme) %>%
  summarise(Size = sum(Size)) %>%
  filter (Size != 0) %>%
  drop_na %>%
  arrange(desc(Size))

#Rank the study programmes by total number of college hours
SaxionRankProgrammeByCollegeHours <- SaxionFulltimeStudents %>%
  group_by(EDUC.CODE1) %>%
  summarise(Tdiff = sum(Tdiff)) %>%
  filter(Tdiff > 100) %>%
  drop_na() %>%
  arrange(desc(Tdiff))

#Rank the study programmes by total number of college hours
SaxionRankProgrammeByCollegeHoursP <- SaxionParttimeStudents %>%
  group_by(EDUC.CODE1) %>%
  summarise(Tdiff = sum(Tdiff)) %>%
  filter(Tdiff > 100) %>%
  drop_na() %>%
  arrange(desc(Tdiff))




#---------------------------------------------------Time Series Analysis--------------------------------------------

#----------------------------------------------------Utwente--------------------------------------------------------

#Make a table containing information important for a time series analysis about the college hours and class sizes per month
UtwenteTimeSeries <- UtwenteStudentCollegeHours %>%
  mutate(weekDay = wday(Date)) %>%
  mutate(calendarWeek = strftime(Date, "%V")) %>%
  mutate(year = year(Date)) %>%
  drop_na() %>%
  mutate(month = format(as.Date(Date),format = "%Y/%m")) %>%
  group_by(year, month) %>%
  summarise(collegeHours = mean(collegeHours), ClassSize = mean(Size)) %>%
  arrange(year, month)

#Plot the average college hours for each month in all years available in the data set 
ggplot(subset(UtwenteTimeSeries, year == 2013), aes(month, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(UtwenteTimeSeries, year == 2014), aes(month, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(UtwenteTimeSeries, year == 2015), aes(month, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(UtwenteTimeSeries, year == 2016), aes(month, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(UtwenteTimeSeries, year == 2017), aes(month, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

#Plot the average college hours for each year available in the data set 
ggplot(UtwenteTimeSeries, aes(year, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

#Plot the average college hours for each month in all years available in the data set 
ggplot(subset(UtwenteTimeSeries, year == 2013), aes(month, ClassSize)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Class Attendance")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(UtwenteTimeSeries, year == 2014), aes(month, ClassSize)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Class Attendance")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(UtwenteTimeSeries, year == 2015), aes(month, ClassSize)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Class Attendance")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(UtwenteTimeSeries, year == 2016), aes(month, ClassSize)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Class Attendance")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(UtwenteTimeSeries, year == 2017), aes(month, ClassSize)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Class Attendance")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

#Plot the average college hours for each year available in the data set 
ggplot(UtwenteTimeSeries, aes(year, ClassSize)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Class Attendance")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

#Now also include Programme information for the time series analysis
UtwenteTimeSeries2 <- UtwenteActivity %>%
  mutate(weekDay = wday(Date)) %>%
  mutate(calendarWeek = strftime(Date, "%V")) %>%
  mutate(year = year(Date)) %>%
  drop_na(Date) %>%
  mutate(month = format(as.Date(Date),format = "%Y/%m")) %>%
  group_by(year, month, Programme) %>%
  summarise(collegeHours = mean(collegeHours), ClassSize = mean(Size)) %>%
  arrange(year, month, Programme)

#Get the top 10 programmes with the highest count of college hours
Top10ProgrammesHighestCollegeHours <- RankProgrammeByCollegeHours %>%
  top_n(n = 10, wt = Tdiff)

#Filter the time series so that only entries belonging to the top 10 programmes remain
UtwenteTimeSeriesTopCollegeHours <- UtwenteTimeSeries2 %>%   
  filter(Programme %in% Top10ProgrammesHighestCollegeHours$Programme)

#Make plots for each month in each year containing the average college hours for the top 10 programmes with the highest count in college hours
ggplot(subset(UtwenteTimeSeriesTopCollegeHours, year == 2013), aes(month, collegeHours, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(UtwenteTimeSeriesTopCollegeHours, year == 2014), aes(month, collegeHours, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(UtwenteTimeSeriesTopCollegeHours, year == 2015), aes(month, collegeHours, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(UtwenteTimeSeriesTopCollegeHours, year == 2016), aes(month, collegeHours, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(UtwenteTimeSeriesTopCollegeHours, year == 2017), aes(month, collegeHours, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

#Do the same as before, now just containing all years in one plot
ggplot(UtwenteTimeSeriesTopCollegeHours, aes(year, collegeHours, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

#Get the top 10 programmes with the biggest class sizes over all years
Top10ProgrammesHighestClassSizes <- RankProgrammeByClassSize %>%
  top_n(n = 10, wt = Size)

#Filter the time series so that only entries belonging to the top 10 programmes remain
UtwenteTimeSeriesTopClassSizes <- UtwenteTimeSeries2 %>%   
  filter(Programme %in% Top10ProgrammesHighestClassSizes$Programme) %>%
  drop_na()

#Make plots for each month in each year containing the class attendance for the top 10 programmes with the highest count in college hours
ggplot(subset(UtwenteTimeSeriesTopClassSizes, year == 2013), aes(month,ClassSize, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Class Attendance")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(UtwenteTimeSeriesTopClassSizes, year == 2014), aes(month, ClassSize, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(UtwenteTimeSeriesTopClassSizes, year == 2015), aes(month, ClassSize, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(UtwenteTimeSeriesTopClassSizes, year == 2016), aes(month, ClassSize, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(UtwenteTimeSeriesTopClassSizes, year == 2017), aes(month, ClassSize, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

#Do the same as before, now just containing all years in one plot
ggplot(UtwenteTimeSeriesTopClassSizes, aes(year, ClassSize, fill=Programme)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")


#------------------------------------------------------Saxion-------------------------------------------------

#-------------------------------------------------Full time----------------------------------------------

SaxionTimeSeries <- SaxionStudentCollegeHours %>%
  mutate(weekDay = wday(DATE)) %>%
  mutate(calendaWeek = strftime(DATE, "%V")) %>%
  mutate(year = year(DATE)) %>%
  drop_na() %>%
  mutate(month = format(as.Date(DATE), format = "%Y/%m")) %>%
  group_by(year,month) %>%
  summarise(collegeHours = mean(collegeHours))

#Plot the average college hours for each month in all years available in the data set 
ggplot(subset(SaxionTimeSeries, year == 2013), aes(month, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(SaxionTimeSeries, year == 2014), aes(month, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(SaxionTimeSeries, year == 2015), aes(month, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

#Plot the average college hours for each year available in the data set 
ggplot(SaxionTimeSeries, aes(year, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

#Now also include Programme information for the time series analysis
SaxionTimeSeries2 <- SaxionFulltimeStudents %>%
  mutate(weekDay = wday(DATE)) %>%
  mutate(calendarWeek = strftime(DATE, "%V")) %>%
  mutate(year = year(DATE)) %>%
  drop_na(DATE) %>%
  mutate(month = format(as.Date(DATE),format = "%Y/%m")) %>%
  group_by(year, month, EDUC.CODE1) %>%
  summarise(collegeHours = mean(Tdiff)) %>%
  arrange(year, month, EDUC.CODE1)



#Get the top 10 programmes with the highest count of college hours
SaxionTop10ProgrammesHighestCollegeHours <- SaxionRankProgrammeByCollegeHours %>%
  top_n(n = 10, wt = Tdiff)

#Filter the time series so that only entries belonging to the top 10 programmes remain
SaxionTimeSeriesTopCollegeHours <- SaxionTimeSeries2 %>%   
  filter(EDUC.CODE1 %in% SaxionTop10ProgrammesHighestCollegeHours$EDUC.CODE1)

#Make plots for each month in each year containing the average college hours for the top 10 programmes with the highest count in college hours
ggplot(subset(SaxionTimeSeriesTopCollegeHours, year == 2013), aes(month, collegeHours, fill=EDUC.CODE1)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(SaxionTimeSeriesTopCollegeHours, year == 2014), aes(month, collegeHours, fill=EDUC.CODE1)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(SaxionTimeSeriesTopCollegeHours, year == 2015), aes(month, collegeHours, fill=EDUC.CODE1)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

#Do the same as before, now just containing all years in one plot
ggplot(SaxionTimeSeriesTopCollegeHours, aes(year, collegeHours, fill=EDUC.CODE1)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

#----------------------------------------------------Part time------------------------------------------

SaxionTimeSeries3 <- SaxionParttimeStudentCollegeHours %>%
  mutate(weekDay = wday(DATE)) %>%
  mutate(calendaWeek = strftime(DATE, "%V")) %>%
  mutate(year = year(DATE)) %>%
  drop_na() %>%
  mutate(month = format(as.Date(DATE), format = "%Y/%m")) %>%
  group_by(year,month) %>%
  summarise(collegeHours = mean(collegeHours))

#Plot the average college hours for each month in all years available in the data set 
ggplot(subset(SaxionTimeSeries3, year == 2013), aes(month, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(SaxionTimeSeries3, year == 2014), aes(month, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ggplot(subset(SaxionTimeSeries3, year == 2015), aes(month, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

#Plot the average college hours for each year available in the data set 
ggplot(SaxionTimeSeries3, aes(year, collegeHours)) + geom_bar(stat="identity", fill="blue") +
  xlab("Month") + ylab("Average CollegeHours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

#Now also include Programme information for the time series analysis
SaxionTimeSeries4 <- SaxionParttimeStudents %>%
  mutate(weekDay = wday(DATE)) %>%
  mutate(calendarWeek = strftime(DATE, "%V")) %>%
  mutate(year = year(DATE)) %>%
  drop_na(DATE) %>%
  mutate(month = format(as.Date(DATE),format = "%Y/%m")) %>%
  group_by(year, month, EDUC.CODE1) %>%
  summarise(collegeHours = mean(Tdiff)) %>%
  arrange(year, month, EDUC.CODE1)



#Get the top 10 programmes with the highest count of college hours
SaxionTop10ProgrammesHighestCollegeHoursP <- SaxionRankProgrammeByCollegeHoursP %>%
  top_n(n = 10, wt = Tdiff)

#Filter the time series so that only entries belonging to the top 10 programmes remain
SaxionTimeSeriesTopCollegeHoursP <- SaxionTimeSeries4 %>%   
  filter(EDUC.CODE1 %in% SaxionTop10ProgrammesHighestCollegeHoursP$EDUC.CODE1)

#Make plots for each month in each year containing the average college hours for the top 10 programmes with the highest count in college hours
ggplot(subset(SaxionTimeSeriesTopCollegeHoursP, year == 2013), aes(month, collegeHours, fill=EDUC.CODE1)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(SaxionTimeSeriesTopCollegeHoursP, year == 2014), aes(month, collegeHours, fill=EDUC.CODE1)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

ggplot(subset(SaxionTimeSeriesTopCollegeHoursP, year == 2015), aes(month, collegeHours, fill=EDUC.CODE1)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")

#Do the same as before, now just containing all years in one plot
ggplot(SaxionTimeSeriesTopCollegeHoursP, aes(year, collegeHours, fill=EDUC.CODE1)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Average College Hours")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 College Hour Programmes")
