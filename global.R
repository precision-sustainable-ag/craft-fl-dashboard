library(httr)
library(DBI)
library(dplyr)

source("secrets.R")
source("loginAPI.R")




craft_con <- dbConnect(
  RPostgres::Postgres(),
  user = dstadmin_creds$user,
  password = dstadmin_creds$password,
  host = dstadmin_creds$host,
  dbname = dstadmin_creds$dbname,
  sslmode = "require"
)


forms_con <- dbConnect(
  RPostgres::Postgres(),
  user = dstadmin_creds$user,
  password = dstadmin_creds$password,
  host = dstadmin_creds$host,
  dbname = "craft_forms",
  sslmode = "require"
)

user_base <- 
  tbl(craft_con, "access_view") %>% 
  select(user_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(password_dummy = "")