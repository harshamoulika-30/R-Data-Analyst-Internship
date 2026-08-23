# Week 3: Statistical Analysis and Predictive Modeling

library(tidyverse)
library(caret)
library(pROC)

titanic <- read.csv("train.csv", stringsAsFactors=FALSE)

titanic_clean <- titanic %>%
  mutate(
    Age=ifelse(is.na(Age), median(Age, na.rm=TRUE), Age),
    Embarked=ifelse(is.na(Embarked),
                    names(sort(table(Embarked), decreasing=TRUE))[1],
                    Embarked),
    FamilySize=SibSp+Parch+1,
    IsAlone=ifelse(FamilySize==1,"Yes","No"),
    Survived=factor(Survived, levels=c(0,1),
                    labels=c("NotSurvived","Survived"))
  ) %>%
  select(Survived,Pclass,Sex,Age,SibSp,Parch,Fare,Embarked,FamilySize,IsAlone)

# Hypothesis test: survival is independent of sex
sex_table <- table(titanic_clean$Sex, titanic_clean$Survived)
chisq.test(sex_table)

# Two-sample comparison of survival indicator by sex
t.test(as.numeric(titanic_clean$Survived)-1 ~ titanic_clean$Sex)

# Train/test split
set.seed(42)
idx <- createDataPartition(titanic_clean$Survived, p=0.80, list=FALSE)
train <- titanic_clean[idx,]
test <- titanic_clean[-idx,]

# Logistic regression
model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch +
             Fare + Embarked + FamilySize + IsAlone,
             data=train, family=binomial)

summary(model)

# Predictions and confusion matrix
prob <- predict(model, newdata=test, type="response")
pred <- ifelse(prob >= 0.5, "Survived", "NotSurvived")
pred <- factor(pred, levels=levels(test$Survived))
confusionMatrix(pred, test$Survived)

# ROC-AUC
roc_obj <- roc(test$Survived, prob, levels=c("NotSurvived","Survived"))
auc(roc_obj)

# 5-fold cross-validation
ctrl <- trainControl(method="cv", number=5, classProbs=TRUE,
                     summaryFunction=twoClassSummary)
cv_model <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch +
                  Fare + Embarked + FamilySize + IsAlone,
                  data=titanic_clean, method="glm", family=binomial,
                  trControl=ctrl, metric="ROC")
cv_model
