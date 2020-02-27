library(DBI)
library(RPostgreSQL)
library(readr)
library(dplyr)
library(lubridate)

data <-read_delim(file = "DPV/Data/BI_Raw_data.csv",
                  delim = ";", locale = locale(encoding="ISO-8859-1"), col_names = TRUE, col_types = NULL)

product <- data %>%
  select(Product_Name, Product_Category) %>%
  rename(name = Product_Name, category = Product_Category) %>%
  arrange(name, category) %>%
  group_by(name, category) %>%
  distinct() %>%
  ungroup() %>%
  mutate(productId = row_number())

 

customer <- data %>%
  select(Customer_Name, Customer_Country) %>%
  rename(name = Customer_Name, country = Customer_Country) %>%
  arrange(name, country) %>%
  group_by(name, country) %>%
  distinct() %>%
  ungroup() %>%
  mutate(customerId = row_number())


sales <- data %>%
  select(Order_Date_Day,Product_Order_Price_Total,Product_Name,Product_Category, Customer_Country,Customer_Name)%>%
  rename(orderDate = Order_Date_Day, sales=Product_Order_Price_Total) %>%
  mutate(orderDate = mdy(orderDate)) %>%
  full_join(product, sales, by = c("Product_Name" = "name","Product_Category"="category")) %>%
  full_join(customer,sales, by = c("Customer_Name"="name","Customer_Country"="country")) 

sales <- sales %>%
  select(-one_of("Customer_Name","Customer_Country","Product_Category","Product_Name"))

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, port = 5432, host = "bronto.ewi.utwente.nl",
                 dbname = "dab_ds19202a_40", user = "dab_ds19202a_40", password = "RlDVlyabzTIJvgHX",
                 options="-c search_path=ass2")
dbWriteTable(con, "product", value = product, overwrite = T, row.names = F)
dbWriteTable(con, "customer", value = customer, overwrite = T, row.names = F)
dbWriteTable(con, "sales", value = sales, overwrite = T, row.names = F)

dbGetQuery(con,
           "SELECT table_name FROM information_schema.tables
WHERE table_schema=’ass2’") ## to get the tables from schema ass2
str(dbReadTable(con, c("ass2", "product")))
