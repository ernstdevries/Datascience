library(DBI)
library(RPostgreSQL)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

#Read data files
data0 <-read_delim(file="SuperstoreSales_main.csv", delim=";", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
head(data0)
#data1 <-read_delim(file="SuperstoreSales_manager.csv", delim=";", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
#head(data1)
data2 <-read_delim(file="SuperstoreSales_returns.csv", delim=";", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
head(data2)

#Convert dates in datasets from character data type to date data type
data0$`Order Date` = dmy(data0$`Order Date`)
data0$`Ship Date` = dmy(data0$`Ship Date`)

#Create new column that checks if a product was delivered too late
data0$Late <- interval(data0$`Order Date`, data0$`Ship Date`)/ddays()
data0$Late <- if_else(data0$Late > 3, "Late", "NotLate", missing = NULL)

#Join the datasets
data0 <- data0 %>%
  full_join(data2, by = "Order ID") 

#Fill NA's
data0$Status <- data0$Status %>%
  replace_na("NotReturned")

data0$Profit <- gsub(",","",data0$Profit)
data0$Profit <- as.double(data0$Profit)

#Create Product table
Product <- data0 %>%
  select(`Product Name`, `Product Category`, `Product Sub-Category`) %>%
  rename(name = `Product Name`, category = `Product Category`, subcategory = `Product Sub-Category`) %>%
  arrange(name, category, subcategory) %>%
  group_by(name, category, subcategory) %>%
  distinct() %>%
  ungroup() %>%
  mutate(productid = row_number())

#Create Customer table
Customer <- data0 %>%
  select(`Customer Name`, Province, Region, `Customer Segment`) %>%
  rename(name = `Customer Name`, province = Province, region = Region, segment = `Customer Segment`) %>%
  arrange(name, province, region, segment) %>%
  group_by(name, province, region, segment) %>%
  distinct() %>%
  ungroup() %>%
  mutate(customerid = row_number())

#Create ReturnStatus table
ReturnStatus <- data0 %>%
  select(Status) %>%
  rename(returnvalue = Status) %>%
  arrange(returnvalue) %>%
  group_by(returnvalue) %>%
  distinct() %>%
  ungroup() %>%
  mutate(returnstatusId = row_number())

#Create Sales table
Sales <- data0

#Join Sales table with Product table
Sales <- Sales %>%
  full_join(Product, by = c("Product Name" = "name",
                            "Product Category" = "category"),
                            "Product Sub-Category" = "subcategory") %>%
  select( -`Product Name`, -`Product Category`, -`Product Sub-Category`)

#Join Sales table with Customer table
Sales <- Sales %>%
  full_join(Customer, by = c("Customer Name" = "name",
                            "Province" = "province"),
                            "Region" = "region",
                            "Customer Segment" = "segment") %>%
  select( -`Customer Name`, -Province, -Region, -`Customer Segment`)

#Join Sales table with ReturnStatus table
Sales <- Sales %>%
  full_join(ReturnStatus, by = c("Status" = "returnvalue")) 

#Create connection to database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, port = 5432, host = "bronto.ewi.utwente.nl",
                 dbname = "dab_ds19202a_40", user = "dab_ds19202a_40", password = "RlDVlyabzTIJvgHX",
                 options="-c search_path=ass3")
#Write all the tables into the database
dbWriteTable(con, "Product", value = Product, overwrite = T, row.names = F)
dbWriteTable(con, "Customer", value = Customer, overwrite = T, row.names = F)
dbWriteTable(con, "ReturnStatus", value = ReturnStatus, overwrite = T, row.names = F)
dbWriteTable(con, "Sales", value = Sales, overwrite = T, row.names = F)

#List the tables from the database
dbListTables(con)
str(dbReadTable(con,"Customer"))
str(dbReadTable(con,"Sales"))
str(dbReadTable(con,"ReturnStatus"))
str(dbReadTable(con,"Product"))

#Query tables from database
dbGetQuery(con,
           "SELECT table_name FROM information_schema.tables
            WHERE table_schema=’ass3’") ## to get the tables from schema ass2
str(dbReadTable(con, c("ass3", "Sales")))

dbDisconnect(con) 
  