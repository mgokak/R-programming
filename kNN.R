library(tidyverse)
library(caret)
library(ggplot2)

install.packages("mlbench")
library(mlbench) # For the dataset

data("PimaIndiansDiabetes")
head(PimaIndiansDiabetes)

set.seed(123)

# Scaling is crucial in kNN
ctrl = trainControl(method = "cv", number = 10)

# preProcess is used to normalise all the data
knnFit <- train(diabetes ~ ., data = PimaIndiansDiabetes, 
                method = "knn",
                trControl = ctrl,
                preProcess = c("center", "scale"))
knnFit

plot(knnFit)

# trueLength: function runs for 15 random values of k instead of the default values of 3,5,7
knnFir1 <- train(diabetes ~ ., data = PimaIndiansDiabetes, 
                 method = "knn",
                 trControl = ctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 15)
plot(knnFir1)

# Distance Functions
install.packages("kknn")
library(kknn)

tuneGrid <- expand.grid(kmax = 3:7, # test k values from 3-7
                        kernel = c("rectangular", "cos"),
                        distance = 1:3) # Minkowski distance power:1-3, power:1 is manhatten, 2:euclidean

# Tune and fir the model with 10-fold cross-validation,
# standardisation and specialized tune grid
kknn_fit <- train(diabetes ~ .,
                  data = PimaIndiansDiabetes,
                  method = "kknn",
                  trControl = ctrl,
                  preProcess = c('center', 'scale'),
                  tuneGrid = tuneGrid)
kknn_fit

# Applying the model
# Predict
pred_knn <- predict(kknn_fit, PimaIndiansDiabetes)

# Generate confusion matrix
confusionMatrix(PimaIndiansDiabetes$diabetes, pred_knn)

# Extracting the result table
knn_results = kknn_fit$results   # gives just the table of results by parameter
head(knn_results)

# group by k and distance function
knn_results <- knn_results %>%
  group_by(kmax, kernel) %>%
  mutate(avgacc = mean(Accuracy))
head(knn_results)

# plot aggregated (over Minkowski power) accuracy per k, split by distance function
ggplot(knn_results, aes(x=kmax, y=avgacc, color=kernel)) + 
  geom_point(size=3) + geom_line()

