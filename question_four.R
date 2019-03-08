#Required Libraries

# 4. What is the repeat rate for each commodity?

#For Buying a Commodity at least twice 

#Repeat Rate by Entire 2-Year Period 
entire_period_repeat <- 
  transactions %>%
  filter(dollar_sales > 0) %>%
  group_by(commodity, household) %>%
  summarise(total_buys = n())

total <-
  entire_period_repeat %>%
  group_by(commodity) %>%
  summarise(total_houses = n())

more_than_two <-
  entire_period_repeat %>%
  filter(total_buys >1)

total_cust <-
  more_than_two %>%
  group_by(commodity) %>%
  summarise(repeat_households = n()) %>%
  left_join(., total) %>%
  mutate(repeat_rate = repeat_households/total_houses)

write.csv(total_cust, "./data/two_year_repeat.csv")

#Average Repeat Rate by Year
year_repeat <-
  transactions %>%
  filter(dollar_sales > 0) %>%
  mutate(year = ifelse(week <= 52, 1, 2)) %>%
  group_by(commodity, household, year) %>%
  summarise(total_buys = n())

total_year <-
  year_repeat %>%
  group_by(commodity, year) %>%
  summarise(total_houses = n())

more_than_two_year<-
  year_repeat %>%
  filter(total_buys > 1)

total_cust_year <-
  more_than_two_year %>%
  group_by(commodity, year) %>%
  summarise(repeat_households = n()) %>%
  left_join(., total) %>%
  mutate(repeat_rate = repeat_households/total_houses) %>%
  group_by(commodity) %>%
  summarise(average_repeat = mean(repeat_rate))

write.csv(total_cust_year, "./data/avg_one_year_repeat.csv")

#Average Repeat Rate by Week
week_repeat <-
  transactions %>%
  filter(dollar_sales > 0) %>%
  group_by(commodity, household, week) %>%
  summarise(total_buys = n())

total_week <-
  week_repeat %>%
  group_by(commodity, week) %>%
  summarise(total_houses = n())

more_than_two_week<-
  week_repeat %>%
  filter(total_buys > 1)

total_cust_week <-
  more_than_two_week %>%
  group_by(commodity, week) %>%
  summarise(repeat_households = n()) %>%
  left_join(., total_week) %>%
  mutate(repeat_rate = repeat_households/total_houses) %>%
  group_by(commodity) %>%
  summarise(average_repeat = mean(repeat_rate))

write.csv(total_cust_week, "./data/avg_week_repeat.csv")
