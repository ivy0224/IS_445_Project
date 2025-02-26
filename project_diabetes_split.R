library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
options(scipen = 999, digits = 3)

diabetes <- read.csv("diabetes_01_split.csv")
str(diabetes)
summary(diabetes)

### find missing values --> no missing values
na.rows <- diabetes %>% 
  filter(if_any(everything(), is.na))

## partition data train 70% + valid 30%
set.seed(1234)
train.index <- sample(1:nrow(diabetes), nrow(diabetes) * 0.7)
train.df <- diabetes[train.index, ]
valid.df <- diabetes[-train.index, ]

### build logistic regression
## use glm() run model based on train.df
# target: Diabetes_binary
# predictors: all other variables
logit.reg <- glm(Diabetes_binary ~., data = train.df, family = "binomial")
summary(logit.reg)
## predict based on valid.df, use cutoff value = 0.5
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
pred <- ifelse(logit.reg.pred > 0.5, 1, 0)
## measure accuracy of logistic regression
confusionMatrix(factor(pred), factor(valid.df$Diabetes_binary), positive = "1")

## predict based on valid.df, use cutoff value = 0.5
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
pred <- ifelse(logit.reg.pred > 0.5, 1, 0)
## measure accuracy of logistic regression
confusionMatrix(factor(pred), factor(valid.df$Diabetes_binary), positive = "1")


### build classification tree
## transfer the target into Yes/No
diabetes$Diabetes_binary <- ifelse(diabetes$Diabetes_binary == 1, "Yes", "No")
diabetes$Diabetes_binary <- factor(diabetes$Diabetes_binary)

## partition data with same index
train.df <- diabetes[train.index, ]
valid.df <- diabetes[-train.index, ]

## default classification tree (5 nodes)
# target: Diabetes_binary (Yes/No)
# predictors: all other variables
default.ct <- rpart(Diabetes_binary ~., data = train.df, method = "class")
rpart.plot(default.ct, type = 1, extra = 1)

## controlled classification tree (3 nodes)
# with maxdepth: max depth of any node
# and minbucket: min number of observations
control.ct <- rpart(Diabetes_binary~., data = train.df, method = "class", 
                    control = rpart.control(maxdepth = 3, minbucket = 30))
rpart.plot(control.ct, type = 1, extra = 1)

# predict based on default tree on valid.df
default.ct.pred <- predict(default.ct, valid.df, type = "class")
# measure accuracy of default tree
confusionMatrix(default.ct.pred, factor(valid.df$Diabetes_binary), positive = "Yes")

# predict based on controlled tree on valid.df
control.ct.pred <- predict(control.ct, valid.df, type = "class")
# measure accuracy of controlled tree
confusionMatrix(control.ct.pred, factor(valid.df$Diabetes_binary), positive = "Yes")


