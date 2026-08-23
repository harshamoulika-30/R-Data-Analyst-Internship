# Week 1 - Titanic Data Cleaning and Preliminary Analysis in R

# Packages
library(tidyverse)

# 1. Load the dataset
titanic <- read.csv("train.csv", stringsAsFactors = FALSE)

# 2. Inspect structure and missing values
str(titanic)
summary(titanic)
colSums(is.na(titanic))

# 3. Clean missing values
titanic_clean <- titanic %>%
  mutate(
    Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age),
    Embarked = ifelse(is.na(Embarked),
                      names(sort(table(Embarked), decreasing = TRUE))[1],
                      Embarked),
    CabinKnown = ifelse(is.na(Cabin), 0, 1),
    FamilySize = SibSp + Parch + 1,
    IsAlone = ifelse(FamilySize == 1, "Yes", "No"),
    SurvivedLabel = ifelse(Survived == 1, "Yes", "No")
  )

# 4. Convert selected fields to categorical variables
titanic_clean$Pclass <- as.factor(titanic_clean$Pclass)
titanic_clean$Sex <- as.factor(titanic_clean$Sex)
titanic_clean$Embarked <- as.factor(titanic_clean$Embarked)

# 5. Check that missing values were handled
colSums(is.na(titanic_clean))

# 6. Descriptive statistics
summary(titanic_clean[, c("Age", "Fare", "FamilySize")])

# 7. Preliminary analysis
titanic_clean %>%
  group_by(Sex) %>%
  summarise(
    Passengers = n(),
    SurvivalRate = mean(Survived) * 100
  )

titanic_clean %>%
  group_by(Pclass) %>%
  summarise(
    Passengers = n(),
    SurvivalRate = mean(Survived) * 100
  )

# 8. Visualisations
ggplot(titanic_clean, aes(x = Age)) +
  geom_histogram(bins = 20) +
  labs(title = "Age Distribution", x = "Age", y = "Count")

ggplot(titanic_clean, aes(x = Sex, y = Survived)) +
  stat_summary(fun = mean, geom = "col") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Survival Rate by Sex", y = "Survival Rate")

ggplot(titanic_clean, aes(x = Pclass, y = Survived)) +
  stat_summary(fun = mean, geom = "col") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Survival Rate by Passenger Class", y = "Survival Rate")
