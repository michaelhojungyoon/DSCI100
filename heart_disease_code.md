#Load packages
library(dplyr)
library(ggplot2)
library(tidymodels)


#Load processed Cleveland heart disease dataset from database via URL
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
download.file(url, destfile =  "data/heartdiseasecleveland.csv")
heartdisease_cleveland <- read.csv("data/heartdiseasecleveland.csv", header = FALSE)
heartdisease_cleveland <- as_tibble(heartdisease_cleveland)
head(heartdisease_cleveland)

#Replace column names
names(heartdisease_cleveland) <- c("Age", "Sex", "Cp", "Trestbps", "Chol", "Fbs", "Restecg", "Thalach", "Exang", "Oldpeak", "Slope", "Ca", "Thal", "Num")
head(heartdisease_cleveland)

#Select columns: Age, Chol, Num
heartdisease_cleveland <- heartdisease_cleveland %>%
  select(Age, Chol, Num)
head(heartdisease_cleveland)

#Convert outcome (Num) to categorical
heartdisease_cleveland$Num[which(heartdisease_cleveland$Num > 0)] <- c(">50% diameter narrowing")
heartdisease_cleveland$Num[which(heartdisease_cleveland$Num == 0)] <- c("<50% diameter narrowing")
heartdisease_cleveland$Num <- as.factor(heartdisease_cleveland$Num)
class(heartdisease_cleveland$Num) 

#Create training dataset and test dataset
  split <- initial_split(heartdisease_cleveland, prop = 0.75, strata = Num)
  train <- training(split)
  test <- testing(split)
  glimpse(train)
  glimpse(test)

#Standardize training data
train_recipe <- recipe(Num ~ Age + Chol, data = train) %>%
    step_scale(all_predictors()) %>%
    step_center(all_predictors()) %>%
    prep()
train_recipe

train <- bake(train_recipe, train)
train

#Summarize training data into table 
train_summary <- train %>%
    mutate(Add = 1) %>%
    mutate(Total = sum(Add)) %>%
    group_by(Num) %>%
    summarize(Mean_Age = mean(Age, na.rm = TRUE),
              Min_Age = min(Age, na.rm = TRUE),
              Max_Age = max(Age, na.rm = TRUE),
              Mean_Chol = mean(Chol, na.rm = TRUE),
              Min_Chol = min(Chol, na.rm = TRUE),
              Max_Chol = max(Chol, na.rm = TRUE),
              Heart_Disease_Num = sum(Add),
              Percent_of_Total = (Heart_Disease_Num/Total[1:1])*100
             )
train_summary

#Create scatterplot showing age vs chol colored by heart disease status
options(repr.plot.width = 12, repr.plot.height = 9) 

age_chol_plot <- ggplot(heartdisease_cleveland, aes(x = Age, y = Chol, color = Num)) + 
    geom_point() + 
    labs(x = "Age (Years)", y = "Cholesterol Level (mg/dL)", fill = "Num") +
    ggtitle("Heart Disease Status based on Age (Years) vs Cholesterol Level (mg/dL)") +
    theme_bw()
age_chol_plot
