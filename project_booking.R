library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
options(scipen = 999, digits = 3)

# load data
booking <- read.csv("booking.csv")
str(booking)
summary(booking)

# factor categorical data 
booking$type.of.meal <- factor(booking$type.of.meal)
booking$room.type <- factor(booking$room.type)
booking$market.segment.type <- factor(booking$market.segment.type)
booking$booking.status <- factor(booking$booking.status)
str(booking)

# check missing values
na.rows <- booking %>% 
  filter(if_any(everything(), is.na))

# separate date --> day + month + year
# deal with format M/D/YYYY
mdy.df <- booking %>%
  separate(date.of.reservation, into = c("month", "day", "year"), 
           sep = "/", remove = FALSE)

# get 37 rows unmatched with M/D/YYYY but YYYY-M-D
ymd <- mdy.df %>%
  filter(if_any(everything(), is.na)) %>%
  pull(Booking_ID)

# split into 2 dataset
# get M/D/YYYY first
mdy.df <- mdy.df %>%
  filter(if_all(everything(), ~!is.na(.)))

# deal with YYYY-M-D then
ymd.df <- booking %>%
  filter(Booking_ID %in% ymd) %>%
  separate(date.of.reservation, into = c("year", "month", "day"),
           sep = "-", remove = FALSE)

# combine 2 dataset
booking <- bind_rows(mdy.df, ymd.df)

# transform from char to num
str(booking)
booking$month <- as.numeric(booking$month)
booking$day <- as.numeric(booking$day)
booking$year <- as.numeric(booking$year)

# set base level for type.of.meal to Not_Selected
booking$type.of.meal <- relevel(booking$type.of.meal, ref = "Not Selected")

# partition: 70% train, 30% validation
set.seed(1234)
train.index <- sample(1:nrow(booking), nrow(booking) * 0.7)
train.df <- booking[train.index, ]
valid.df <- booking[-train.index, ]


# Model 1: logistic regression
# target: booking.status
# predictors: all variables exclude Booking_ID and date.of.reservation
logit.reg <- glm(booking.status~., data = train.df[, -c(1, 16)], family = "binomial")
summary(logit.reg)

# Warning message:
#   glm.fit: fitted probabilities numerically 0 or 1 occurred 
library(car)
vif(logit.reg)

## predict based on valid.df, use cutoff value = 0.56 (catch more canceled)
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
pred <- ifelse(logit.reg.pred > 0.56, "Not_Canceled", "Canceled")

## measure accuracy of logistic regression
levels(booking$booking.status)
logit.cm <- confusionMatrix(factor(pred), factor(valid.df$booking.status), positive = "Not_Canceled")
logit.cm

## improved logistic regression
# warning message --> multicollinearity
# remove market.segment.type = Complementary
# since it's free, no one canceled
complementary <- booking %>% 
  filter(market.segment.type == "Complementary")

booking <- booking %>%
  filter(market.segment.type != "Complementary")

# remove room type as we do not have detailed info
# of each room type
booking <- booking %>%
  select(-room.type)

train.index.v2 <- sample(1:nrow(booking), nrow(booking) * 0.7)
train.df.v2 <- booking[train.index.v2, ]
valid.df.v2 <- booking[-train.index.v2, ]

logit.reg.v2 <- glm(booking.status~., data = train.df.v2[, -c(1, 15)], family = "binomial")
summary(logit.reg.v2)

## predict based on valid.df, use cutoff value = 0.59
logit.reg.v2.pred <- predict(logit.reg.v2, valid.df.v2, type = "response")
pred.v2 <- ifelse(logit.reg.v2.pred > 0.59, "Not_Canceled", "Canceled")

## measure accuracy of logistic regression
levels(booking$booking.status)
logit.cm.v2 <- confusionMatrix(factor(pred.v2), factor(valid.df.v2$booking.status), 
                               positive = "Not_Canceled")
logit.cm.v2

### Model 2-1: classification tree
## default classification tree (5 nodes)
# target: booking_binary (Yes/No)
# predictors: all other variables
default.ct <- rpart(booking.status ~., data = train.df.v2[, -c(1, 15)], method = "class")
rpart.plot(default.ct, type = 1, extra = 1)

# predict based on default tree on valid.df
default.ct.pred <- predict(default.ct, valid.df, type = "class")
# measure accuracy of default tree
default.ct.cm <- confusionMatrix(default.ct.pred, factor(valid.df$booking.status), 
                                 positive = "Not_Canceled")
default.ct.cm

## Model 2-2: controlled classification tree (3 nodes)
# with maxdepth: max depth of any node
# and minbucket: min number of observations
control.ct <- rpart(booking.status~., data = train.df.v2[, -c(1, 15)], method = "class",
                    control = rpart.control(maxdepth = 3, minbucket = 30))
rpart.plot(control.ct, type = 1, extra = 1)

# predict based on controlled tree on valid.df
control.ct.pred <- predict(control.ct, valid.df, type = "class")
# measure accuracy of controlled tree
control.ct.cm <- confusionMatrix(control.ct.pred, factor(valid.df$booking.status),
                                 positive = "Not_Canceled")
control.ct.cm


# Model 3: randomForest ensemble of decision tree
rf <- randomForest(booking.status~., data = train.df.v2[, -c(1, 15)], 
                   ntree = 500, mtry = sqrt(ncol(train.df.v2) - 2), 
                   importance = TRUE)
importance(rf)
varImpPlot(rf)

# predict based on valid.df.v2
rf.pred <- predict(rf, valid.df.v2, type = "prob")[,2]
head(rf.pred)

# set cutoff value = 0.5
rf.pred <- ifelse(rf.pred > 0.5, "Not_Canceled", "Canceled")

# measure accuracy of randomForest decision tree
rf.cm <- confusionMatrix(factor(rf.pred), valid.df.v2$booking.status, 
                         positive = "Not_Canceled")
rf.cm

### compare performance of three models
accuracy_comparison <- data.frame(
  Model = c("Logistic Regression", "Default Tree", "Controlled Tree", "Random Forest"),
  Accuracy = c(logit.cm$overall["Accuracy"], 
               default.ct.cm$overall["Accuracy"], 
               control.ct.cm$overall["Accuracy"], 
               rf.cm$overall["Accuracy"]),
  Sensitivity = c(logit.cm$byClass["Sensitivity"], 
                  default.ct.cm$byClass["Sensitivity"], 
                  control.ct.cm$byClass["Sensitivity"], 
                  rf.cm$byClass["Sensitivity"]),
  Specificity = c(logit.cm$byClass["Specificity"], 
                  default.ct.cm$byClass["Specificity"], 
                  control.ct.cm$byClass["Specificity"], 
                  rf.cm$byClass["Specificity"]),
  Balanced_Accuracy = c(logit.cm$byClass["Balanced Accuracy"], 
                        default.ct.cm$byClass["Balanced Accuracy"], 
                        control.ct.cm$byClass["Balanced Accuracy"], 
                        rf.cm$byClass["Balanced Accuracy"])
)

accuracy_comparison

### visualize comparison
# convert data to long format for ggplot
accuracy_long <- tidyr::pivot_longer(accuracy_comparison, cols = -Model, 
                                     names_to = "Metric", values_to = "Value")

# plot accuracy, sensitivity, specificity
ggplot(accuracy_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Model Performance Comparison") +
  ylab("Score") + xlab("Model") +
  theme_minimal() +
  scale_fill_manual(values = c("Accuracy" = "blue", "Sensitivity" = "green", 
                               "Specificity" = "red", "Balanced_Accuracy" = "yellow"))


