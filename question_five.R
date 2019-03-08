#Required Libraries
library(ggplot2)
library(broom)

# 5. How is the health of the category(commodity) -- Pasta?
#By sales? Sales over time? number of individuals who buy the brand?

pasta <- transactions[commodity == "pasta", ]
pasta <- pasta[, year := ifelse(week <= 52, 1, 2)]
  
brand_sales <-
  pasta %>%
  group_by(week, brand) %>%
  summarise(total_sales = sum(dollar_sales))

weekly_sales <-
  pasta %>%
  group_by(week) %>%
  summarise(total_sales = sum(dollar_sales))


ggplot(data = weekly_sales, aes(x = week, y = total_sales)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  ylab("Weekly Sales")


model_brand <- lm(total_sales ~ week + factor(brand), data = brand_sales)
model_weekly <- lm(total_sales ~ week, data = weekly_sales)
results_brand<- summary(model_brand)
results_weekly <- summary(model_weekly)

results_brand
results_weekly

#Part of Question Six
by_brands <- 
  pasta %>%
  group_by(brand, week) %>%
  summarise(total_sales = sum(dollar_sales))

brands <- select(pasta, brand) %>% unique()

coefs <- NULL
for(i in 1:nrow(brands)) {
  
  data <- filter(by_brands, brand == brands$brand[i])
  
  model <- lm(total_sales ~ week, data = data)
  
  results <- tidy(model)
  
  ggplot(data = data, aes(x = week, y = total_sales)) +
    geom_point() + 
    geom_smooth(method = "lm") +
    ylab("Weekly Sales") +
    ggtitle(brands$brand[i])
  
  results$brand <- brands$brand[i]
  
  ggsave(paste0("./data/processed_data/", brands$brand[i],".png"))
  
  coefs <- rbind(coefs, results)
}
