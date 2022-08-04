
rm(list = ls())

# Data Preprocessing

# Importing the dataset
dataset = read.csv('insurance.csv')
View(dataset)

#load library
library(tidyverse)
library(dplyr)

library(ggplot2)
library(GGally)



# To view summary 
summary(dataset)
dim(dataset)
str(dataset)

# Check null observations
colSums(is.na(dataset))


dataset <- dataset %>% mutate_if(~is.character(.), ~as.factor(.))


#coorelations - scatter plot

plot(y= dataset$charges, x = dataset$bmi, ylab = "charges",
     xlab = "BMI", main = "charges vs bmi")

plot(y= dataset$charges, x = dataset$age, ylab = "charges",
     xlab = "AGE", main = "charges vs age")

hist(dataset$charges)
hist((dataset$age))

plot(y= dataset$charges, x = dataset$sex, ylab = "charges",
     xlab = "Gender", main = "charges vs sex")




region_per <- dataset %>%
  group_by(region) %>%
  summarise(count = n()) %>%
  mutate(percent = round(100 * count / sum(count), 0))
region_desc


sex_per <- dataset %>%
  group_by(sex) %>%
  summarise(count = n()) %>%
  mutate(percent = round(100 * count / sum(count), 0))
sex_per

smoker_per <- dataset %>%
  group_by(smoker) %>%
  summarise(count = n()) %>%
  mutate(percent = round(100 * count / sum(count), 0))
smoker_per 

children_per <- dataset %>%
  group_by(children) %>%
  summarise(count = n()) %>%
  mutate(percent = round(100 * count / sum(count), 0))
children_per

# We'll create a new column based on BMI
dataset <- dataset %>% 
  mutate(bmi_cat = case_when(
    bmi < 18.5 ~ "underweight",
    bmi < 25 ~ "healthy weight",
    bmi < 30 ~ "overweight",
    bmi >= 30 ~ "obesity"
  ))

dataset %>% 
  ggplot(aes(x = age, y = charges, color = bmi_cat)) +
  geom_point() +
  labs(title = "Charges v Age",
       x = "Age",
       y = "Charges ($)")

dataset %>% 
  ggplot(aes(x = age, y = charges, color = smoker)) +
  geom_point() +
  labs(title = "Charges v Age",
       x = "Age",
       y = "Charges ($)")

ggcorr(dataset %>% mutate_if(is.factor, as.numeric), label = TRUE)


# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)




# Fitting Simple linear regression
model_1 <- lm(charges ~ age, data = training_set)
summary(model_1)


# Multiple linear regression

model_all <- lm(charges~ . , data = training_set)
summary(model_all)

model_2 <- lm( dataset$charges ~ dataset$age + dataset$bmi + dataset$smoker + dataset$sex + dataset$children + dataset$region, training_set)
summary(model_2)

model_3 <- lm(dataset$charges ~ dataset$age + dataset$bmi + dataset$smoker + dataset$children, data = training_set)
summary(model_3)


# prediction model_1
predict1 <- predict(model_1, newdata = test_set)


# prediction model_2
predict2 <- predict(model_2, newdata = test_set)



