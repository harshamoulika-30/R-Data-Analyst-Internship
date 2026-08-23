# Week 2: Data Visualization and Insight Communication

library(tidyverse)

titanic <- read.csv("train.csv", stringsAsFactors = FALSE)

titanic_clean <- titanic %>%
  mutate(
    Age = ifelse(is.na(Age), median(Age, na.rm=TRUE), Age),
    Embarked = ifelse(is.na(Embarked),
                      names(sort(table(Embarked), decreasing=TRUE))[1],
                      Embarked),
    FamilySize = SibSp + Parch + 1,
    IsAlone = ifelse(FamilySize == 1, "Yes", "No"),
    SurvivedLabel = ifelse(Survived == 1, "Survived", "Not Survived")
  )

# 1. Bar chart
ggplot(titanic_clean, aes(x=Sex, y=Survived)) +
  stat_summary(fun=mean, geom="col") +
  scale_y_continuous(labels=scales::percent_format()) +
  labs(title="Survival Rate by Sex", y="Survival Rate")

# 2. Grouped bar chart
ggplot(titanic_clean, aes(x=factor(Pclass), y=Survived, fill=Sex)) +
  stat_summary(fun=mean, geom="col", position="dodge") +
  scale_y_continuous(labels=scales::percent_format()) +
  labs(title="Survival Rate by Class and Sex", x="Passenger Class")

# 3. Histogram
ggplot(titanic_clean, aes(x=Age)) +
  geom_histogram(bins=20) +
  labs(title="Passenger Age Distribution", x="Age", y="Count")

# 4. Scatter plot
ggplot(titanic_clean, aes(x=Age, y=Fare, color=SurvivedLabel)) +
  geom_point(alpha=0.5) +
  labs(title="Age vs Fare by Survival Outcome", color="Outcome")

# 5. Line chart
family_summary <- titanic_clean %>%
  filter(FamilySize <= 7) %>%
  group_by(FamilySize) %>%
  summarise(SurvivalRate=mean(Survived))

ggplot(family_summary, aes(x=FamilySize, y=SurvivalRate)) +
  geom_line() + geom_point() +
  scale_y_continuous(labels=scales::percent_format()) +
  labs(title="Survival Rate by Family Size", y="Survival Rate")

# 6. Age-group comparison
titanic_clean %>%
  mutate(AgeGroup=cut(Age, breaks=c(0,12,18,30,50,80),
                      labels=c("0-12","13-18","19-30","31-50","51-80"))) %>%
  group_by(AgeGroup) %>%
  summarise(SurvivalRate=mean(Survived)) %>%
  ggplot(aes(x=AgeGroup, y=SurvivalRate)) +
  geom_col() +
  scale_y_continuous(labels=scales::percent_format()) +
  labs(title="Survival Rate by Age Group", y="Survival Rate")
