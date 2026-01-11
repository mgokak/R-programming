#Missing Values

library(hflights)
data(hflights)
flights = hflights
summary(flights$DepTime)

sum(is.na(flights$DepTime)) #sum for checking na values of an individual col

#joining

authors <- data.frame(
  name = c('Turkey', 'Venables', 'Tierney', 'Ripley', 'McNeil', 'Gurkan'),
  nationality = c('US', 'Australia', 'US', 'UK', 'Australia', 'Turkey'),
  retired = c('Yes', rep('No', 4), 'Yes'))

books <- data.frame(
  name = c('Turkey', 'Venables', 'Tierney', 'Ripley', 'McNeil', 'Gurkan'),
  title = c('Exploratory Data Analytics',
            'Modern Applied Statistics..',
            'LISP-STAT',
            'Spatial Statistics', 'Stochastic Simulation',
            'Interactive Data Analysis'),
  other.author = c(NA, 'Ripley', NA, NA, NA, NA))

head(authors)
head(books)

library(tidyverse)
authors %>% inner_join(books, by = 'name')

authors %>% left_join(books, by = 'name')

#Missing values, Data Integrity, and Normalization

summary(flights$DepTime)

clean_flights <- flights %>% drop_na(DepTime)
summary(clean_flights$DepTime) #after removal

summary(flights$TaxiIn)

#replacing na's with mean

clean_flights$TaxiIn <- clean_flights$TaxiIn %>%
  replace_na(as.integer(round(mean(clean_flights$TaxiIn, na.rm = TRUE))))


summary(clean_flights$TaxiIn)


#Sampling

set.seed(19) # to stop randonmizer from picking diff subsets everytime

mtcars_indexed <- mtcars %>% mutate(id = row_number()) # randomizer requires id no for rows

train <- mtcars_indexed %>% sample_frac(.75) #training set

test <- anti_join(mtcars_indexed, train, by = 'id') #test set

nrow(mtcars) #comparing the size

nrow(test)

#Normalization

library(caret)

preproc1 <- preProcess(mtcars, method = c('center', 'scale')) #center scale allows to standardize the data

norm1 <- predict(preproc1, mtcars) #predict : to fit our data based on preprocessing

summary(norm1) #standardized version 

# min - max scaling

preproc2 <- preProcess(mtcars, method = c('range')) #range to min max scaler
norm2 <- predict(preproc2, mtcars)
summary(norm2)

# BINNING AND SMOOTHING

mycars <- mtcars
mycars %>%
  mutate(hpfactor = cut(hp,
                        breaks = c(-Inf, 120, 200, Inf),
                        labels = c('low', 'medium', 'high'))) %>%
  head()

#SMOOTHING 

mycars <- mtcars %>%
  mutate(hpfactor = cut(hp, breaks = 3, #gives 3 equal width bins
                        labels = c('low', 'medium', 'high')))
head(mycars)

#mutate and store each

low <- mycars %>%
  filter(hpfactor == 'low') %>%
  mutate(hp = mean(hp, na.rm = T))

medium <- mycars %>%
  filter(hpfactor == 'medium') %>%
  mutate(hp = mean(hp, na.rm = T))

high <- mycars %>%
  filter(hpfactor == 'high') %>%
  mutate(hp = mean(hp, na.rm = T))

bind_rows(list(low, medium, high)) #bind_rows combines separate sets