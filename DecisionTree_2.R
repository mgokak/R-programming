library(tidyverse)
library(caret)
library(ggplot2)
library(rpart)
library(rattle)

head(diamonds)
# no of rows/entries
nrow(diamonds)

# building tree model
# rpart1SE (1 Standard Deviation  Error) is used to build a model. it builds multiple models and picks 
# the smallest one within the boundry of 1 standard error
# algorithm doesnt prune automatically using the cp (complexity parameter), we can do it
set.seed(94)
train_control = trainControl(method = "cv", number = 10)

# convert target variable to factor
diamonds$cut <- as.factor(diamonds$cut)

rm(tree1)
# fit the model
tree1 <- train(cut ~., data = diamonds, method = "rpart1SE", trControl = train_control)
tree1

# Evaluate the fir with confusion matrix
pred_tree <- predict(tree1, diamonds)
confusionMatrix(diamonds$cut, pred_tree)

# Visualize
fancyRpartPlot(tree1$finalModel, caption = "")

# Now to compare, use rpart
tree2 <- train(cut ~., data = diamonds, method = "rpart", trControl = train_control)
tree2

# fit this model
tree2 <- train(cut ~., data = diamonds, control = rpart.control(maxdepth = 3), 
               trControl = train_control, method = "rpart1SE")
tree2

# Evaluate with confusion
pred_tree2 <- predict(tree2, diamonds)
confusionMatrix(diamonds$cut, pred_tree2)

# Visualize
fancyRpartPlot(tree2$finalModel, caption = "")

# Set HYPERPARAMETERS
# minsplit = min number of observations that must exist in the node for the split to be attempted
# minbucket = min number of observations that must be present in the terminal node, if the next
#   split results a child node having less observation than minbucket then the split is not allowed
# maxdepth =  limits how many levels deep the tree can grow. helps prevent complex trees
hypers <- rpart.control(minsplit = 5000, maxdepth = 4, minbucket = 2500)

# cp (Complexity Parameter) = ontrols the growth and pruning of a decision tree
# methos rpart1SE doesnot automatically tune cp
tree2 <- train(cut ~., data = diamonds, control = hypers, trControl = train_control, method = "rpart1SE")

pred_tree2 = predict(tree2, diamonds)
confusionMatrix(diamonds$cut, pred_tree2)
# here the accuracy went down

fancyRpartPlot(tree2$finalModel, caption = "")

# TRAIN-TEST partition
index = createDataPartition(y=diamonds$cut, p = 0.7, list = FALSE)
train_set = diamonds[index,]
test_set = diamonds[-index,]

# build model using the training set, using defaukt hyper-parameters
tree1 <- train(cut ~., data = train_set, method = "rpart1SE", trControl = train_control)

# evaluate using confusion matrix
pred_tree <- predict(tree1, test_set)
confusionMatrix(test_set$cut, pred_tree)

fancyRpartPlot(tree1$finalModel, caption = "")
# here accuracy score is close to the training set, so may be no overfitting

# now with setting hyperparameter
hypers <- rpart.control(minsplit = 5000, maxdepth = 4, minbucket = 2500)

tree2 <- train(cut ~., data = train_set, control = hypers, method = "rpart1SE", trControl = train_control)

pred2 <- predict(tree2, test_set) #generate predictions from a fitted classification model, tree2 
confusionMatrix(test_set$cut, pred_tree) #compares the actual values to model's predictions

fancyRpartPlot(tree2$finalModel, caption = "")

# General Model Comparison and Visualization
# Initialize cross validation
train_control = trainControl(method = "cv" , number = 10)
# Tree 1
hypers = rpart.control(minsplit =
                         2
                       , maxdepth =
                         1
                       , minbucket =
                         2)
tree1 <- train(cut ~., data = train_set, control=hypers, trControl=train_control, method="rpart1SE")
# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree1, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$cut, pred_tree)
# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree1, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$cut, pred_tree)
# Get training accuracy
a_train <- cfm_train$overall[
  1
]
# Get testing accuracy
a_test <- cfm_test$overall[
  1
]
# Get number of nodes
nodes <- nrow(tree1$finalModel$frame)
# Form the table
comp_tbl <- data.frame(
  "Nodes"
  = nodes,
  "TrainAccuracy"
  = a_train,
  "TestAccuracy"
  = a_test,
  "MaxDepth"
  =
    1
  ,
  "Minsplit"
  =
    2
  ,
  "Minbucket"
  =
    2
)

# Tree 2
hypers = rpart.control(minsplit =
                         5
                       , maxdepth =
                         2
                       , minbucket =
                         5
)
tree2 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method =
                  "rpart1SE"
)
# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree2, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$cut, pred_tree)
# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree2, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$cut, pred_tree)
# Get training accuracy
a_train <- cfm_train$overall[
  1
]
# Get testing accuracy
a_test <- cfm_test$overall[
  1
]
# Get number of nodes
nodes <- nrow(tree2$finalModel$frame)
# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test,2,5,5))

# Tree 3
hypers = rpart.control(minsplit =
                         50
                       , maxdepth =
                         3
                       , minbucket =
                         50
)
tree4 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method =
                  "rpart1SE"
)
# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree4, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$cut, pred_tree)
# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree4, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$cut, pred_tree)
# Get training accuracy
a_train <- cfm_train$overall[
  1
]
# Get testing accuracy
a_test <- cfm_test$overall[
  1
]
# Get number of nodes
nodes <- nrow(tree4$finalModel$frame)
# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test,
                                    3
                                    ,
                                    50
                                    ,
                                    50
))

# Tree 4
hypers = rpart.control(minsplit =
                         100
                       , maxdepth =
                         4
                       , minbucket =
                         100
)
tree7 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method =
                  "rpart1SE"
)
# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree7, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$cut, pred_tree)
# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree7, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$cut, pred_tree)
# Get training accuracy
a_train <- cfm_train$overall[
  1
]
# Get testing accuracy
a_test <- cfm_test$overall[
  1
]
# Get number of nodes
nodes <- nrow(tree7$finalModel$frame)
# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test,
                                    4
                                    ,
                                    100
                                    ,
                                    100
))

# Tree 5
hypers = rpart.control(minsplit =
                         1000
                       , maxdepth =
                         4
                       , minbucket =
                         1000
)
tree8 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method =
                  "rpart1SE"
)
# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree8, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$cut, pred_tree)
# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree8, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$cut, pred_tree)
# Get training accuracy
a_train <- cfm_train$overall[
  1
]
# Get testing accuracy
a_test <- cfm_test$overall[
  1
]
# Get number of nodes
nodes <- nrow(tree8$finalModel$frame)
# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test,
                                    4
                                    ,
                                    1000
                                    ,
                                    1000
))

# Tree 6
hypers = rpart.control(minsplit =
                         5000
                       , maxdepth =
                         8
                       , minbucket =
                         5000
)
tree10 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method =
                   "rpart1SE"
)
# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree10, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$cut, pred_tree)
# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree10, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$cut, pred_tree)
# Get training accuracy
a_train <- cfm_train$overall[
  1
]
# Get testing accuracy
a_test <- cfm_test$overall[
  1
]
# Get number of nodes
nodes <- nrow(tree10$finalModel$frame)
# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test,
                                    8
                                    ,
                                    5000
                                    ,
                                    5000
))

# Tree 7
hypers = rpart.control(minsplit =
                         10000
                       , maxdepth =
                         25
                       , minbucket =
                         10000
)
tree11 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method =
                   "rpart1SE"
)
# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$cut, pred_tree)
# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$cut, pred_tree)
# Get training accuracy
a_train <- cfm_train$overall[
  1
]
# Get testing accuracy
a_test <- cfm_test$overall[
  1
]
# Get number of nodes
nodes <- nrow(tree11$finalModel$frame)
# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test,
                                    25
                                    ,
                                    10000
                                    ,
                                    10000
))

# Display complete table
comp_tbl

# Visualize with scatter plot
ggplot(comp_tbl, aes(x=Nodes)) + geom_point(aes(y = TrainAccuracy), color = "red") + 
  geom_point(aes(y = TestAccuracy), color = "blue") + 
  ylab("Accuracy")

# Visualize with a line plot
ggplot(comp_tbl, aes(x=Nodes)) + geom_line(aes(y = TrainAccuracy), color = "red") + 
  geom_line(aes(y = TestAccuracy), color= "blue") + 
  ylab("Accuracy")

# FEATURE SELECTION
# Relevance Analysis (Variable Importance Score)
colnames(diamonds)
# train model
tree1 <- train(cut ~., data = train_set, method = "rpart1SE", trControl = train_control)

# view variable importance score
var_imp <- varImp(tree1, scale = FALSE)
var_imp

# Visualise the importance using plot
plot(var_imp)
