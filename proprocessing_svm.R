# Week 2 Tutorial: Data Exploration, Visualization, Integration, and Preprocessing
# Converted from week2_Tutorial.pdf
#
# ------------------------------------------------------------
# SECTION 1: DATA VISUALIZATION WITH GGPLOT2
# ------------------------------------------------------------

library(ggplot2)
library(tidyverse)

# Line Graph: GDP vs Time
library(astsa)
df <- as.data.frame(gdp)
df$quarter <- seq(1947.25, 2018.75, by = 0.25)
ggplot(df, aes(x = quarter, y = x)) +
  geom_line() +
  xlab('Time') +
  ylab('GDP') +
  ggtitle('GDP Time Series')

# ------------------------------------------------------------
# Bar Charts
# ------------------------------------------------------------
ggplot(diamonds, aes(x = clarity)) + geom_bar()

ggplot(diamonds, aes(x = clarity, fill = cut)) +
  geom_bar(position = 'stack')

ggplot(diamonds, aes(x = clarity, y = price, fill = cut)) +
  geom_col()

# ------------------------------------------------------------
# Histograms
# ------------------------------------------------------------
ggplot(mtcars, aes(hp)) + geom_histogram(binwidth = 60)

ggplot(mtcars, aes(hp)) +
  geom_histogram(binwidth = 30) +
  facet_wrap(~cyl)

# ------------------------------------------------------------
# Boxplots
# ------------------------------------------------------------
ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) +
  geom_boxplot() +
  xlab('cyl')

# ------------------------------------------------------------
# Scatter Plots
# ------------------------------------------------------------
ggplot(mtcars, aes(mpg, hp)) + geom_point()

ggplot(mtcars, aes(mpg, hp)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm)

# ------------------------------------------------------------
# SECTION 2: DATA INTEGRATION AND CLEANING
# ------------------------------------------------------------
library(hflights)
data(hflights)
flights <- hflights
summary(flights$DepTime)
sum(is.na(flights$DepTime))

# Handling Missing Values
clean_flights <- flights %>% drop_na(DepTime)

# Mean Imputation Example
clean_flights$TaxiIn <- clean_flights$TaxiIn %>%
  replace_na(mean(clean_flights$TaxiIn, na.rm = TRUE))

# ------------------------------------------------------------
# Sampling
# ------------------------------------------------------------
set.seed(19)
mtcars_indexed <- mtcars %>% mutate(id = row_number())
train <- mtcars_indexed %>% sample_frac(0.75)
test <- anti_join(mtcars_indexed, train, by = 'id')

# ------------------------------------------------------------
# Normalization
# ------------------------------------------------------------
library(caret)
preproc1 <- preProcess(mtcars, method = c('center', 'scale'))
norm1 <- predict(preproc1, mtcars)
summary(norm1)

preproc2 <- preProcess(mtcars, method = 'range')
norm2 <- predict(preproc2, mtcars)
summary(norm2)

# ------------------------------------------------------------
# Binning and Smoothing
# ------------------------------------------------------------
mycars <- mtcars %>%
  mutate(hpfactor = cut(hp, breaks = c(-Inf, 120, 200, Inf),
                        labels = c('low', 'medium', 'high')))

# ------------------------------------------------------------
# Feature Extraction and PCA
# ------------------------------------------------------------
storm <- storms %>% select(-name) %>% na.omit()
dummy <- dummyVars(category ~ ., data = storm)
dummies <- as.data.frame(predict(dummy, newdata = storm))

storm.pca <- prcomp(dummies)
summary(storm.pca)
screeplot(storm.pca, type = 'l')

preProc <- preProcess(dummies, method = 'pca', pcaComp = 2)
storm.pc <- predict(preProc, dummies)
storm.pc$category <- storm$category

# ------------------------------------------------------------
# Classification with SVM
# ------------------------------------------------------------
library(e1071)
train_control <- trainControl(method = 'cv', number = 5)
svm_storm <- train(category ~ ., data = dummies %>% mutate(category = storm$category),
                   method = 'svmLinear', trControl = train_control)
svm_storm

svm_storm2 <- train(category ~ ., data = storm.pc,
                    method = 'svmLinear', trControl = train_control)
svm_storm2