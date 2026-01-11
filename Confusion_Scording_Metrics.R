library(caret)
library(rpart)
library(tidyverse)
library(rattle)
library(ggplot2)
library(pROC)

# Load the dataset
data("Sacramento")
head(Sacramento)

set.seed(123)
# 70-30 train-test split
index = createDataPartition(y=Sacramento$type, p=0.7, list=FALSE)
train_sc = Sacramento[index,]
test_sc = Sacramento[-index,]

# Decision Tree with cross-validation
train_control = trainControl(method = "cv", number = 10)
tree <- train(type ~., data = train_sc, method = "rpart", trControl = train_control)
tree

# Confusion Matrix
pred_sc <- predict(tree, test_sc)
cm <- confusionMatrix(test_sc$type, pred_sc)
cm

# Scoring Metrics
# Store the byClass object of confusion matrix as a dataframe
metrics <- as.data.frame(cm$byClass)
metrics

# Get the precision value for each class
metrics %>% select(c(Precision))

# Get the recall value for each class
metrics %>% select(c(Recall))

# Get the specificity value for each class
metrics %>% select(c(Specificity))

# Get the F1 score value for each class
metrics %>% select(c(F1))

# Get the balanced accuracy value for each class
metrics %>% select(c(`Balanced Accuracy`))

# ROC curves
library(mlbench)
data("PimaIndiansDiabetes")
head(PimaIndiansDiabetes)

# Check target class and make sure it has 2 levels
str(PimaIndiansDiabetes$diabetes)

# Partition the data
index = createDataPartition(y=PimaIndiansDiabetes$diabetes, p=0.7, list=FALSE)
train_pima = PimaIndiansDiabetes[index,]
test_pima = PimaIndiansDiabetes[-index,]

# kNN with Cross-Validation
train_control = trainControl(method = "cv", number = 10)
knn <- train(diabetes ~., data = train_pima, method = "knn", trControl = train_control, tuneLength = 20)
knn

# Evaluate the fit with a confusion matrix
pred_pima <- predict(knn, test_pima)
confusionMatrix(test_pima$diabetes, pred_pima)

library(pROC)
# Get class probabilities for KNN
pred_prob <- predict(knn, test_pima, type = "prob")
head(pred_prob)

# create an ROC curve for our model.
roc_obj <- roc((test_pima$diabetes), pred_prob[,1])
plot(roc_obj, print.auc=TRUE)

# Decision tree with cv
train_control = trainControl(method = "cv", number = 10)
dtree <- train(diabetes ~., data = train_pima, method = "rpart", trControl = train_control)
dtree

# Evaluate the fit with a confusion matrix
pred_pima2 <- predict(dtree, test_pima)
# Confusion Matrix
confusionMatrix(test_pima$diabetes, pred_pima2)

# ROC for Decision Tree
# Get class probabilities for decision tree model
pred_prob2 <- predict(dtree, test_pima, type = "prob")
head(pred_prob2)

roc_obj2 <- roc((test_pima$diabetes), pred_prob2[,1])
plot(roc_obj2, print.auc=TRUE)






