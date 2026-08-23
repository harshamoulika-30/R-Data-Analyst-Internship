# Week 4: Comprehensive Titanic Analysis - Integrated R Workflow

library(tidyverse)
library(caret)
library(pROC)

# 1. Load and prepare data
titanic <- read.csv("train.csv", stringsAsFactors=FALSE)

titanic_clean <- titanic %>%
  mutate(
    Age=ifelse(is.na(Age), median(Age, na.rm=TRUE), Age),
    Embarked=ifelse(is.na(Embarked),
                    names(sort(table(Embarked), decreasing=TRUE))[1],
                    Embarked),
    CabinKnown=ifelse(is.na(Cabin),0,1),
    FamilySize=SibSp+Parch+1,
    IsAlone=ifelse(FamilySize==1,"Yes","No"),
    SurvivedLabel=ifelse(Survived==1,"Survived","Not Survived")
  )

# 2. Visualization
ggplot(titanic_clean, aes(x=Sex, y=Survived)) +
  stat_summary(fun=mean, geom="col") +
  scale_y_continuous(labels=scales::percent_format())

ggplot(titanic_clean, aes(x=factor(Pclass), y=Survived, fill=Sex)) +
  stat_summary(fun=mean, geom="col", position="dodge") +
  scale_y_continuous(labels=scales::percent_format())

ggplot(titanic_clean, aes(x=Age, y=Fare, color=SurvivedLabel)) +
  geom_point(alpha=0.5)

# 3. Statistical analysis
chisq.test(table(titanic_clean$Sex, titanic_clean$Survived))
t.test(titanic_clean$Survived ~ titanic_clean$Sex)

# 4. Logistic regression with cross-validation
model_data <- titanic_clean %>%
  mutate(
    Survived=factor(Survived, levels=c(0,1),
                    labels=c("NotSurvived","Survived"))
  ) %>%
  select(Survived,Pclass,Sex,Age,SibSp,Parch,Fare,Embarked,FamilySize,IsAlone)

set.seed(42)
idx <- createDataPartition(model_data$Survived,p=.80,list=FALSE)
train <- model_data[idx,]
test <- model_data[-idx,]

model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch +
             Fare + Embarked + FamilySize + IsAlone,
             data=train, family=binomial)

summary(model)

prob <- predict(model,test,type="response")
pred <- factor(ifelse(prob>=.5,"Survived","NotSurvived"),
               levels=levels(test$Survived))
confusionMatrix(pred,test$Survived)

roc_obj <- roc(test$Survived,prob,levels=c("NotSurvived","Survived"))
auc(roc_obj)

# 5-fold cross-validation
ctrl <- trainControl(method="cv",number=5,classProbs=TRUE,
                     summaryFunction=twoClassSummary)
cv_model <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch +
                  Fare + Embarked + FamilySize + IsAlone,
                  data=model_data,method="glm",family=binomial,
                  trControl=ctrl,metric="ROC")
cv_model
