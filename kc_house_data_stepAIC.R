kc_house_data <- read.csv("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week4/kc_house_data.csv", header=TRUE, stringsAsFactors=TRUE)
# stringsAsFactors is used to convert char data to factors, header=TRUE means the first row should be treatd as column names
head(kc_house_data) # columns and first few rows
str(kc_house_data) # structure of data set
summary(kc_house_data) #gives quarters, mean, median

#manually leaving out columns(features) that don't help with the prediction and creating new data frame
d <- kc_house_data[,-c(1,2)] #include all rows and columns except the 1st and 2nd column
head(d)
str(d)

plot(kc_house_data$long, kc_house_data$lat)

#lat and long can also be skipped in the dataset
d <- kc_house_data[,-c(1,2,18,19)]
str(d)

model <- lm(price ~ ., data = d) # . means predict price with all the other features
summary(model)

#now skip zipcode(17) as it is not significant
d <- kc_house_data[,-c(1,2,17,18,19)]
str(d)

model <- lm(price ~ ., data = d)
summary(model)

#skip sqft_basement(14) aswell
d <- kc_house_data[,-c(1,2,14,17,18,19)]
str(d)

model <- lm(price ~ ., data = d)
summary(model)

#skip sqft_lot(7) aswell
d <- kc_house_data[,-c(1,2,7,14,17,18,19)]
str(d)

model <- lm(price ~ ., data = d)
summary(model)

#skip sqft_above(13) aswell
d <- kc_house_data[,-c(1,2,7,13,14,17,18,19)]
str(d)

model <- lm(price ~ ., data = d)
summary(model)

#as we are predicting Price, check how it is correlated with all
cor(d)  #Pearson correlation, diagonal is 1 and should be symmetric
# values should be from -1 to +1. -1: perfectly negatively correlated, +1: perfectly positively correlated, 0: no cor

# yr_built and condition are close to 0, so try to remove them

#removing yr_built(15) first and then condition(11)
d1 <- d
d1 <- kc_house_data[,-c(1,2,7,13,14,15,17,18,19)]
str(d1)
model1 <- lm(price ~ ., data = d1)
summary(model1)

d1 <- kc_house_data[,-c(1,2,7,11,13,14,15,17,18,19)]
str(d1)
model1 <- lm(price ~ ., data = d1)
summary(model1)
# not useful, both reduce the adjusted R-squared

str(d)
dim(d) #row, column

#scatterplot
plot(d)

#sample: takes a random sample of the specifies size from the elements of x either using with or withour replacement
dsample <- d[sample(1:nrow(d), 1000, replace=FALSE),]
summary(dsample) #for the 1000 chosen data points
#replace=FALSE, 100th row will not go back into the dataset, it will be taken off
#sampling 1st row to 21613(total rows) and going to select only 1000 data points on random

dim(kc_house_data)
dim(d)
dim(dsample)

plot(dsample)

#sqft_living has high correlation, hence using this for the second-order term
d <- kc_house_data[,-c(1,2,7,13,14,17,18,19)]
d$sqft_livingSQ <- (d$sqft_living)^2
dim(d)

model <- lm(price ~ ., data = d)
summary(model)
#sqft_living is significant and adjusted R-square increased

d$sqft_lotSQ <- (d$sqft_lot15/1000)^2 #as sqft_lot is a big number, squaring it tields a huge number, hence diving be 1000 to fit in
d$sqfSQ <- NULL # to remove a partivular row. rm is for the whole object removal
dim(d)
str(d)

model <- lm(price ~ ., data = d)
summary(model)
# adjusted R-square is still the same so this can be removed

#on intution, an interaction term of waterview with bedroom is created
d$wat_bed <- d$waterfront*d$bedrooms
dim(d)
str(d)

model <- lm(price ~ ., data = d)
summary(model)  #adjusted R-squared has increased a bit but can be skipped aswell

# used in Train/Test Split
#The runif() function in R generates random numbers from a uniform distribution. 
#This means that every value within a specified range has an equal probability of being generated.
# runif(n, min = 0, max = 1) n: observations
runif(1)

# set.seed() function is used to ensure the reproducibility of results that involve random number generation. 
#When you perform operations that rely on random numbers, such as sampling or simulating data, 
#the results would typically vary each time you run the code. By setting a seed, you fix the starting point of 
#the pseudo-random number generator, guaranteeing that the same sequence of "random" numbers is produced 
#every time the code is executed with that specific seed. Seed can be any random number
set.seed(123)
runif(1)

d <- kc_house_data[,-c(1,2)]
head(d)
dim(d)

partition <- sample(2, nrow(d), replace = TRUE, prob = c(0.80,0.20))
#2 partitions, nrow(d) means all rows, replace is true and probability is 80% in partition 1(train) and 20% in the other(test)
head(partition)
# 80% is partition 1 and 20% is partition2

head(partition == 1)  #rows 1 2 5 6
head(partition == 2)  # rows 3 4

#partition is used to take data from data frame and put into training or testing
train <- d[partition == 1,]   #now 80% is in training data frame
head(train)
dim(train) #columns don't change but only 80% of the 21613 rows is taken here

test <- d[partition == 2,]
head(test)  #has the remaining 20% of the data
dim(test)

# training model
model <- lm(price ~ ., data = train)
summary(model)

# applying training data to test model prediction
prediction <- predict(model, test) # applying the model on test dataset, this is an r object
head(prediction)
# displays the first 6 data points that went into the testing dataset and the model's price prediction for each point

actual = test$price
head(actual)  #now displying the actual price we already have in our dataset
# Difference between the axctual and prediction is called Residuals

cor(prediction, actual) #0.827 good positive correlation as the values are close for the actual for few points

plot(prediction, actual)

# "Data Analysis and Graphics." It is a collection of functions and data sets
install.packages("DAAG")
library(DAAG)

#another way to split data to split into different folds using cross-validation
out <- cv.lm(data = d, form.lm = formula(price ~ .), plotit = "Observed", m = 3)
summary(out)

# VARIABLE SCREENING: stepwise regression (to remove variables one by one after seeing the summary of the model)
# "Modern Applied Statistics in S" S ia programming language that got renamed to R
install.packages("MASS")
library(MASS)

#The stepAIC() function in R is a tool for stepwise model selection based on the Akaike Information Criterion (AIC)
#stepwise has backward elimination and forward selection methods

# Backward Elimination: creates a model and then eliminated one variable at a time
d <- kc_house_data[,-c(1,2)]
model_full <- lm(price ~ ., data=d)
summary(model_full)

step <- stepAIC(model_full, direction = "backward") #removed sqft-basement feature
summary(step)
# think of this as an additional elimination step only and not as the final list
# elimination based on intuition can be done on this to increase adjusted R-squared
# run this on the previous model that has all significant features only to improve the accuracy

# Forward selection: model should be empty and each feature should be added step by step
model_empty <- lm(price ~ 1, data=d)
summary(model_empty)

step <- stepAIC(model_empty, direction = "forward", scope = list(upper=model_full, lower=model_empty))
summary(step) #this has also left out sqft-basement and then given the summary of model with best features only

# all subset regression

