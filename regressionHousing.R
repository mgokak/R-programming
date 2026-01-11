housing <- read.csv("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week3/housing.csv")
str(housing) # str is structure displaying the internal structure of the data frame
summary(housing)

# ocean proximity has char values, it should be converted to factors (num values) to use it
housing$ocean_proximity <- as.factor(housing$ocean_proximity)
str(housing)

#dependent variable(y) = median_house_value

#histogram
hist(housing$median_house_value)
hist(housing$housing_median_age, breaks = 20) # breaks = bins
hist(housing$total_bedrooms)

#correlation
# cor(housing) will give error: x must be numeric
# this is because ocean proximity is not numeric

# Dependent variable (y) should be highly correlated with the independent variable (x)
# if no correlation, then is it worth using the independent variable to predict the dependent

# Include all row, leave out column 1 (logitude), 2 (latitude) and 10 (ocean proximity)
cor(housing[,3:9]) # all rows, 3-9 column
cor(housing$median_house_value, housing$median_income)

# Correlation: done using the Pearson correlation formula and matrix, only works with numeric datatypes, values -1 - +1, 0 no correlation
# diagonal of the matrix will always be 1 as the variables correlate between themselves
# matrix is always symmetrix, upper and lower half are same

# To leave out data points with NA and use only complete values,
cor(housing[,3:9], use = "complete.obs")    # removes the NA values, the whole row is removed
plot(housing$housing_median_age, housing$median_house_value)
plot(housing$total_rooms, housing$median_house_value)
plot(housing$total_bedrooms, housing$median_house_value)
plot(housing$households, housing$median_house_value)
plot(housing$median_income, housing$median_house_value) #diagoal as income increases, the house value increases. cor also shows the same

# MODEL BUILDING: FIRST-ORDER MODELS
m1 <- lm(median_house_value ~ housing_median_age, data = housing) # simple linear regression model
# Trying to predict median_house_value (DV) using the housing_median_age (IV) variable
summary(m1)

m2 <- lm(median_house_value ~ total_rooms, data = housing)
summary(m2)

m3 <- lm(median_house_value ~ total_bedrooms, data = housing)
summary(m3)

m4 <- lm(median_house_value ~ population, data = housing)
summary(m4)

m5 <- lm(median_house_value ~ median_income, data = housing)
summary(m5)

m6 <- lm(median_house_value ~ housing_median_age + population + households + median_income, data = housing)
summary(m6)

# INTERACTION MODEL: introducing 2 interactive terms (here income * population)
m7 <- lm(median_house_value ~ housing_median_age + population + households + median_income + population*median_income, data = housing)
summary(m7)

# SECOND-ORDER MODEL
m8 <- lm(median_house_value ~ housing_median_age + population + households + poly(median_income, 2), data = housing)
summary(m8)   # Let's not use this method now

# using this now (second order model), introducing a new column mi2 into the data frame
housing$mi2 <- housing$median_income^2
str(housing)

m9 <- lm(median_house_value ~ housing_median_age + population + households + median_income + mi2, data = housing)
summary(m9)

# using Categorical Variable
# ocean_proximity can be used even though its char as its converted to a factor(num values), now the model won't give error
m10 <- lm(median_house_value ~ housing_median_age + population + households + median_income + ocean_proximity, data = housing)
summary(m10)
