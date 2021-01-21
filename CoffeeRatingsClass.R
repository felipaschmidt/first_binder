library(tidyverse)
library(Hmisc)
library(performance)
library(car)
library(olsrr)
library(MASS)

coffee_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

view(coffee_ratings)

coffee_filtered <- filter(coffee_ratings, total_cup_points > 0)


ggplot(coffee_filtered, aes(x = aroma, y = flavor)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(coffee_filtered, aes(x = aftertaste, y = flavor)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(coffee_filtered, aes(x = acidity, y = flavor)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(coffee_filtered, aes(x = body, y = flavor)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(coffee_filtered, aes(x = balance, y = flavor)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(coffee_filtered, aes(x = sweetness, y = flavor)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(coffee_filtered, aes(x = uniformity, y = flavor)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(coffee_filtered, aes(x = clean_cup, y = flavor)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(coffee_filtered, aes(x = moisture, y = flavor)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

model0 <- lm(flavor ~ 1, data = coffee_filtered)
model1 <- lm(flavor ~ aroma + acidity + body + balance +
              uniformity + clean_cup, data = coffee_filtered)
anova(model0, model1)

check_model(model1)

vif(model1)

pmodel <- ols_step_forward_p(model1)
pmodel

model2 <- lm(flavor ~ aroma + acidity + body + balance +
              uniformity + clean_cup, data = coffee_ratings)

new.cofee <- data.frame(
  aroma = c(7), 
  acidity = c(7), 
  body = c(5), 
  balance = c(9), 
  uniformity = c(6), 
  clean_cup = c(9)
)

predict(model1, newdata = new.cofee)
