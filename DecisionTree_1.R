library(tidyverse)
library(caret)
library(ggplot2)
library(rpart)

# Load the dataset
# data(Sacramento) or do the below
load("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week6/Sacramento.RData")
head(Sacramento)

# gg = Grammar of Graphiscs
ggplot(Sacramento, aes(beds)) + geom_histogram(binwidth = 1) 
# look at the distribution of beds, aes: aesthetics

# Remove 6-8 bedrooms due to heavy imbalance
Sacramento <- Sacramento %>% filter(beds < 6) #8 beds is small compared to 3 or 4 so filter < 6

Sacramento$beds <- as.factor(Sacramento$beds)
ggplot(Sacramento, aes(beds)) + geom_bar()

# Building a DECISION TREE

# Now we are using rest of the indepndent variables(X) to predict out target (IV = y) i.e. beds
# Evaluation method
train_control = trainControl(method = "cv", number  = 10) # 10-fold cv

# Fit the model, using rpart to do the decision tree
tree1 <- train(beds ~., data = Sacramento, method = "rpart", trControl = train_control)
tree1
# gives traiing accuracy
# cp: complexity parameter: best way to control the optimal tree growth
# kappa: normalized at the based line of random chance

# CLASSIFICATION on the test dataset

# Classification here is similar to svm and visualised using confusion matrix
pred_tree <- predict(tree1, Sacramento)
# Generate confusion matrix for the test set pred_tree
confusionMatrix(Sacramento$beds, pred_tree)
# gives testing accuracy

# Sensitivity: model's capability to predict true positives of each available categories
# Specificity: model's ability to predict true negatives of each available categories

# VISUALIZATION
install.packages("rattle")
library(rattle)

# Visualize decision tree
fancyRpartPlot(tree1$finalModel, caption = "")

# IF-THEN statements of the tree
library(rpart.plot)

tree1_rpart <- tree1$finalModel
rpart.plot::rpart.plot(tree1_rpart)

rules <- asRules(tree1_rpart)

paths <- path.rpart(tree1_rpart, nodes = rownames(tree1_rpart$frame))
for (i in seq_along(paths)) {
  cat(paste("Rule", i, ": IF", paste(paths[[i]], collapse = " AND "), "THEN",
            "beds =", tree1_rpart$frame$yval[i], "\n"))
}









