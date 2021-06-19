library(readr)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)
library(lift)

# load data set
churn_bigml_80 <- read_csv("churn-bigml-80.csv")
churn_bigml_20 <- read_csv("churn-bigml-20.csv")
raw_data <- rbind(churn_bigml_80, churn_bigml_20)

# check NA value
sapply(raw_data, function(x) sum(is.na(x)))

# remove unnecessary column
data <- select(raw_data, -`Account length`, -`Area code`)
data$Churn <- factor(data$Churn)
data$`International plan` <- factor(data$`International plan`)
data$`Voice mail plan` <- factor(data$`Voice mail plan`)

# summary data
summary(raw_data)

# hold-out method
ind <- sample(nrow(data), 0.3*(nrow(data)))
data_train <- data[-ind,]
data_test <- data[ind,]

# explore data train and test
summary(data_train)
summary(data_test)

table(data_train$Churn)
table(data_test$Churn)

# make model for prediction
model <- rpart(Churn ~ ., data = data_train)

# plot tree
rpart.plot(model)

# predict model
res <- predict(model, data_test, type = "class")
head(res)

# variable importance
model$variable.importance

# Confusion matrix
confusionMatrix(res, 
                data_test$Churn, 
                positive = "TRUE", 
                mode = "prec_recall")

# lift analysis
res.p <- predict(model, data_test)[, "TRUE"]

pred <- prediction(res.p, data_test$Churn, label.ordering = c("FALSE", "TRUE"))
pref_lift <- performance(pred, "lift", "rpp")
plot(pref_lift)

# lift at 10 %
TopDecileLift(res.p, as.integer(data_test$Churn) - 1)




