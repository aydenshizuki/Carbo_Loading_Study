#Required Libraries 

# 3. What drives my sales? Which brands and which customers?
#Check interaction with "sales"(money?) with other variables: coupons, brands, customers, 
#location in store, day of week, time of day  

sales_brands <- 
  transactions %>%
  filter(commodity == "pasta") %>%
  group_by(brand) %>%
  summarise(total_sales = sum(dollar_sales))

write.csv(sales_brands, "./data/total_sales_brand.csv")

sales_houses <- 
  transactions %>%
  filter(commodity == "pasta") %>%
  group_by(geography, household) %>%
  summarise(total_sales = sum(dollar_sales),
            total_units = sum(units))
write.csv(sales_houses, "./data/total_sales_house.csv")
