# Regression: Method to predict a numerical value using ML method 

cars <- read.csv("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week3/cars.csv")
head(cars)

plot(cars$weight, cars$mpg)

# We are predicting mpg, hence its Dependent Variable (y), all others are Independent (X)

plot(cars$horsepower, cars$mpg)
plot(cars) #Scatter plot matrix

cars[1,] # 1st row, all column
cars[,1] # all rows, 1st column
cars[,2:7] # all rows, 2-7 columns

# LINEAR MODEL
help(lm)
model <- lm(cars$mpg ~ cars$displacement) # ~ is read as given, using displacement to predict mpg
model
summary(model)

model <- lm(mpg ~ displacement, data = cars) # Same model just calling it different
model

coefficients(model)
coef(model) # same as above

fitted(model) #Returns the predicted values (Ŷ) from your model.
#For each observation in your dataset, it tells you what the model thinks the value should be.

residuals(model) #Returns the residuals, i.e., the difference between the actual observed values (Y) and 
# the fitted (predicted) values (Ŷ). These measure how far off the model’s predictions are.
resid(model) # same as above

# Building the next model
model2 <- lm(cars$mpg ~ cars$horsepower)
summary(model2)
# Good value to predict mpg as r-squared is 0.6049

model3 <- lm(cars$mpg ~ cars$weight)
summary(model3)
# Better than horsepower to predict mpg as r-squared is 0.691. Best performing out of all to predict mpg

model4 <- lm(cars$mpg ~ cars$acceleration)
summary(model4)
# gives r-squared as 0.174 hence not a good value to predict mpg

model5 <- lm(cars$mpg ~ cars$cylinders)
summary(model5)
# Similar to horsepower

