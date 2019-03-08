#Required Libraries 


# 1. What are the top 5 products in each commodity?
# Rate Top Product by: volume of sales, money earned, number of distinct households
# that bought the product 

averages <- NULL
for(i in 1:nrow(commodities)){
  temp <- transactions[commodity == commodities$commodity[i]]
  
  by_day_sales <- 
    temp %>%
    group_by(day, product_description) %>%
    summarise(total_dollar_sales = sum(dollar_sales, na.rm = TRUE),
              total_units = sum(units, na.rm = TRUE)) %>%
    group_by(product_description) %>%
    summarise(day_sales = mean(total_dollar_sales, na.rm = TRUE),
              day_units = mean(total_units, na.rm = TRUE)) %>%
    data.table()
  
  day_sales <- by_day_sales[order(by_day_sales$day_sales, 
                                  decreasing = TRUE)[1:5], 1:2]
  day_units <- by_day_sales[order(by_day_sales$day_units, 
                                  decreasing = TRUE)[1:5], -2]
  
  by_week_sales <- 
    temp %>%
    group_by(week, product_description) %>%
    summarise(total_dollar_sales = sum(dollar_sales, na.rm = TRUE),
              total_units = sum(units, na.rm = TRUE)) %>%
    group_by(product_description) %>%
    summarise(week_sales = mean(total_dollar_sales, na.rm = TRUE),
              week_units = mean(total_units, na.rm = TRUE))
  
  week_sales <- by_week_sales[order(by_week_sales$week_sales, 
                                    decreasing = TRUE)[1:5], 1:2]
  week_units <- by_week_sales[order(by_week_sales$week_units, 
                                    decreasing = TRUE)[1:5], -2]
  
  by_year_sales <-
    temp %>%
    mutate(year = ifelse(week <= 52, 1, 2)) %>%
    group_by(year, product_description) %>%
    summarise(total_dollar_sales = sum(dollar_sales, na.rm = TRUE),
              total_units = sum(units, na.rm = TRUE)) %>%
    group_by(product_description) %>%
    summarise(year_sales = mean(total_dollar_sales, na.rm = TRUE),
              year_units = mean(total_units, na.rm = TRUE))
  
  year_sales <- by_year_sales[order(by_year_sales$year_sales, 
                                    decreasing = TRUE)[1:5], 1:2]
  year_units <- by_year_sales[order(by_year_sales$year_units, 
                                    decreasing = TRUE)[1:5], -2]
  total <-
    temp %>%
    group_by(product_description) %>%
    summarise(total_sale = sum(dollar_sales),
              total_unit = sum(units))
  
  total_sales <- total[order(total$total_sale, 
                             decreasing = TRUE)[1:5], 1:2]
  total_units <- total[order(total$total_unit, 
                             decreasing = TRUE)[1:5], -2]
  
  average_sales <- 
    year_sales %>%
    left_join(., week_sales) %>%
    left_join(., day_sales) %>%
    left_join(., total_sales) %>%
    mutate(commodity = commodities$commodity[i])
  
  average_units <-
    year_units %>%
    left_join(., week_units) %>%
    left_join(., day_units) %>%
    left_join(., total_units) %>%
    mutate(commodity = commodities$commodity[i])
  
  averages[[i]] <- list(average_units, average_sales)
}

units <- lapply(1:length(averages), function(x){
  averages[[x]][[1]]
})

units <- rbindlist(units)

sales <- lapply(1:length(averages), function(x){
  averages[[x]][[2]]
})

sales <- rbindlist(sales)

write.csv(sales, "./data/product_sales.csv")
write.csv(units, "./data/product_units.csv")

household_aware <-
  transactions %>%
  select(commodity, product_description, household) %>%
  unique() %>%
  group_by(commodity, product_description) %>%
  summarise(unique_households = n())
  
household_aware <- household_aware[order(household_aware$commodity, 
                                  household_aware$unique_households, decreasing = TRUE),]

household_aware <- 
  household_aware %>%
  group_by(commodity) %>%
  top_n(n = 5, wt = unique_households) %>%
  mutate(percentage = unique_households/as.numeric(total_households))

write.csv(household_aware, "./data/product_household.csv")

#Returns 

returns <- 
  transactions %>%
  filter(dollar_sales < 0) %>%
  group_by(commodity, product_description) %>%
  summarise(total_sales_returns = sum(dollar_sales),
            total_return_transactions = n(),
            total_unit_returns = sum(units))

write.csv(returns, "./data/product_returns.csv")
