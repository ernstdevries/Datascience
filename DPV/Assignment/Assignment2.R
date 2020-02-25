library(DBI)
library(RPostgreSQL)
library(readr)
library(dplyr)
library(lubridate)

#Read dataset
data0 <-read_delim(file="BI_Raw_data.csv", delim=";", col_names = TRUE, col_types = NULL, locale = locale(encoding = 'LATIN1'))
head(data0)

#Create a product table
product <- data0 %>%
  select(Product_Name, Product_Category) %>%
  rename(name = Product_Name, category = Product_Category) %>%
  arrange(name, category) %>%
  group_by(name, category) %>%
  distinct() %>%
  ungroup() %>%
  mutate(productid = row_number())

#Create a customer table
customer <- data0 %>%
  select(Customer_Name, Customer_Country) %>%
  rename(name = Customer_Name, country = Customer_Country) %>%
  arrange(name, country) %>%
  group_by(name, country) %>%
  distinct() %>%
  ungroup() %>%
  mutate(customerid = row_number())

#Create a sales table
sales <- data0 

#Join the sales table and the product table
sales <- sales %>%
  full_join(product, by = c("Product_Name" = "name",
                            "Product_Category" = "category")) %>%
  select( -Product_Name, -Product_Category)

#Join the sales table and the customer table
sales <- sales %>%
  full_join(customer, by = c("Customer_Name" = "name",
                            "Customer_Country" = "country")) %>%
  select( -Customer_Name, -Customer_Country)

#Filter out unnecessary columns from sales table
sales <- sales %>%
  select(Order_Date_Day, Order_Price_Total, productid, customerid) %>%
  rename(orderdate = Order_Date_Day, sales = Order_Price_Total)

#Create connection to database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, port = 5432, host = "bronto.ewi.utwente.nl",
                 dbname = "dab_ds19202a_40", user = "dab_ds19202a_40", password = "RlDVlyabzTIJvgHX",
                 options="-c search_path=ass2")

#Write tables into database
dbWriteTable(con, "product", value = product, overwrite = T, row.names = F)
dbWriteTable(con, "customer", value = customer, overwrite = T, row.names = F)
dbWriteTable(con, "sales", value = sales, overwrite = T, row.names = F)

#List the tables from the datbase
dbListTables(con)
str(dbReadTable(con,"customer"))
str(dbReadTable(con,"sales"))
str(dbReadTable(con,"product"))

#Query the tables from the database
dbGetQuery(con,
           "SELECT table_name FROM information_schema.tables
            WHERE table_schema=’ass2’") ## to get the tables from schema ass2
str(dbReadTable(con, c("ass2", "sales")))

dbDisconnect(con) 