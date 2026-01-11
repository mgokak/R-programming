# RESIDUAL ANALYSIS: the process of evaluating the differences between observed and predicted values from a model 
# to assess its validity and accuracy

kc_house_data <- read.csv("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week4/kc_house_data.csv")
str(kc_house_data)
dim(kc_house_data)
summary(kc_house_data)

model = lm(price ~ bedrooms + bathrooms + sqft_living + view + yr_built, data = kc_house_data)
summary(model)

#extracts the residuals from a statistical model, which represent the differences between the observed and 
#predicted values for each data point
residuals = model$residuals
head(residuals)
# +ve value: model underestimated the actual value. -Ve: model overestimated the actual value
# Small magnitude (e.g., close to zero): The model's prediction was very close to the actual observed value
# Large magnitude (e.g., -277855.02): The model's prediction for this observation was a poor fit, 
# meaning it was very far from the actual observed value. The value -277855.02 for observation 6 is a 
# particularly large error, indicating that the model made a significant overestimation for that data point. 
# Model is imperfect, the fit varies, has potential outliers or model issues

sum = sum(model$residuals) #sum of all residuals we have
sum

hist(model$residuals, breaks = 100) #close to a good normal distribution
# 100 = no of bins (equal-sized intervals or ranges into which the full range of a data set is divided)

mean = mean(model$residuals)
mean

sd = sd(model$residuals) # standard deviation
sd

# z-score normalization: resulting resid_zscore variable will be a new set of data points with a mean of 0 
# and a standard deviation of 1. This process is also known as standardization
resid_zscore = (model$residuals - mean)/sd
head(resid_zscore)

hist(resid_zscore, breaks = 100)
# histogram remains the same, only the x-axis changes

# The Durbin-Watson statistic is a test for autocorrelation in the residuals of a regression analysis. 
# The Durbin-Watson statistic will always have a value ranging between 0 and 4. A value of 2.0 indicates 
# there is no autocorrelation detected in the sample. Values from 0 to less than 2 point to positive autocorrelation, 
# and values from 2 to 4 mean negative autocorrelation.

# Autocorrelation: statistical concept that assesses the degree of correlation between the values of variable 
# at different time points
library(car)
durbinWatsonTest(model)
# the values show that there might be slight dependence and the values are not independent
# Durbin-Watson statistic is very close to 2 and the p-value (0.062) is greater than 0.05, 
# you can conclude that there is no statistically significant evidence of first-order autocorrelation

# RESIDUAL PLOTS
# there shouldn't be a pattern when plotting IV(y) with residuals, should be random. 
# If there's a pattern, the model wouldn't patter well
plot(kc_house_data$bedrooms, model$residuals)
# all points are lining on each other hence dark lines but not really showing any pattern

plot(kc_house_data$bedrooms, resid_zscore)
# looks same like befpre, only the x-axis is changed, most between -1 and +3

plot(kc_house_data$sqft_living, resid_zscore)
# no exact pattern, most in -3 to +3

plot(kc_house_data$view, resid_zscore)
# most in -3 to +3

plot(kc_house_data$yr_built, resid_zscore)
# most between -3 to +3

plot(model)

# Log transforming a price dependent variable makes its distribution more normal and can handle outliers
model2 <- lm(log(price) ~ bedrooms + bathrooms + sqft_living + view + yr_built, data = kc_house_data)
summary(model2)

plot(model2)
# transformation has changed a bit and become better after making it log
