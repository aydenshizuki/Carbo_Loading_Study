 #Use this script before the others 

#Clear Work Space 
rm(list = ls())
gc()

#Set Working Directory
setwd("~/Downloads/Carbo-Loading Case Study/")

#Required Libraries 
x <- c("tidyverse", "lubridate", "data.table")
lapply(x, library, character.only = TRUE)

#Read in Lookup Tables
products <- read.csv("./data/raw_data/dh_product_lookup.csv")
stores <- read.csv("./data/raw_data/dh_store_lookup.csv")
trade_activity <- read.csv("./data/raw_data/dh_causal_lookup.csv")

#Read in Transaction Data
transactions <- read.csv("./data/raw_data/dh_transactions.csv")

#Some Assumptions:
# - Since there is a total of 728 days, then I will assume that each year has 364 days. 
# - All observations labeled with day less than 365 will be year 1 and greater than or equal to 365
# - will be year 2. 
# - All weeks labeled with less than 53 will be year 1 and greater or equal will be year 2.
# - Negative Dollar Sales are returns on products (therefore inventoried as negative)
# - Data set is "cleaned"

#Prep Data Sets to be Used

#Merge Data Sets Together 
transactions <-
  transactions %>%
  left_join(., products) %>%
  left_join(., stores) %>%
  left_join(., trade_activity) %>%
  data.table()

#Get Number of Distinct Households 
total_households <- length(unique(transactions$household))

#Get commodities 
commodities <- 
  transactions %>%
  select(commodity) %>%
  unique() %>%
  data.table()
