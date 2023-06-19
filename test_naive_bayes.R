source("naive_bayes.R")
library(psych)
library(caret)

data <- read.csv("drug200.csv")
View(head(data))
pairs.panels(data[-ncol(data)])

train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

nb_model <- naive_bayes_train(train_data, "Drug")
predictions <- naive_bayes_predict(test_data, nb_model)
print(confusionMatrix(factor(predictions), factor(test_data$Drug)))






