##### I S 445 Project - Student Performance Factors #####
##### Team SQL Squad #####
library(dplyr)
library(ggplot2)
library(caret)
options(scipen = 999, digits = 3)

#### load data
factors.df <- read.csv("Students_Performance.csv", stringsAsFactors = TRUE)
str(factors.df)

#### data cleaning and pre-processing
### check missing values
na.df <- factors.df %>%
  filter(if_any(everything(), is.na))

### check white space
space.df <- factors.df %>%
  filter(if_any(everything(), ~. == ""))

### transfer "Average_class_attendance" from character to numeric
factors.df$Average_class_attendance <- as.numeric(factors.df$Average_class_attendance)
str(factors.df)

### align "Health.issues" to 2 levels "Yes", "No"
levels(factors.df$Health_issues) 
# [1] "N"   "no"  "No"  "Yes"
factors.df$Health_issues <- as.character(factors.df$Health_issues)
factors.df$Health_issues <- ifelse(factors.df$Health_issues == "N" | factors.df$Health_issues == "no", 
                                    "No", factors.df$Health_issues)
factors.df$Health_issues <- factor(factors.df$Health_issues)
str(factors.df)

### align "Relationship.status" to 4 levels "Single", "Relationship", "Engaged", "Married"
levels(factors.df$Relationship_status) 
# [1] "Engaged" "In a relationship" "Married" "Relationship" "Single"  
factors.df$Relationship_status <- as.character(factors.df$Relationship_status)
factors.df$Relationship_status <- ifelse(factors.df$Relationship_status == "In a relationship", 
                                    "Relationship", factors.df$Relationship_status)
factors.df$Relationship_status <- factor(factors.df$Relationship_status)
str(factors.df)


### drop "Program" since its the same for all students
factors.df <- factors.df %>%
  select(-"Program")

### categorize and cluster skills???

write.csv(file = "StudentsPerformance_cleaned.csv", factors.df)


### mlr model
factors.mlr.v1 <- lm(Current_CGPA~., data = factors.df[, -1])
summary(factors.mlr.v1)
factors.mlr.v2 <- lm(Current_CGPA~., data = factors.df[, -c(1, 20, 22)])
summary(factors.mlr.v2)
stepwise_model <- step(lm(Current_CGPA~., data = factors.df[, -c(1, 5, 20, 22)]), direction = "both")
summary(stepwise_model)




