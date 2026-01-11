cars <- read.csv("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week1/cars.csv")
head(cars) #displays the first 6 rows
head(cars $model)
head(cars $mpg)
head(cars $mpg, na.rm = TRUE) #na.rm = TRUE is to remove missing values
mean(cars $horsepower, na.rm = TRUE)
summary(cars)
help(mean)
boxplot(cars $horsepower, main = "Horsepower")
boxplot(cars $weight, main = "Weight")
hist(cars $horsepower)
hist(cars $mpg, breaks = 20, main = "Cars", xlab = "MPG", freq = FALSE) #breaks is bins
mean <- mean(cars $mpg, na.rm = TRUE)
mean
sd <- sd(cars $mpg, na.rm = TRUE) #standard deviation
sd
curve(dnorm(x, mean, sd), add = TRUE, col = "darkblue", lwd = 2) #density curve of a normal distribution, lwd is the line width, x is the histogram

head(mtcars)
summary(mtcars)
names(mtcars)
mtcars $cyl

#creating a new dataframe, mycars and assigning values from the existing dataset
mycars <- mtcars[mtcars$cyl == 4 & mtcars$mpg > 18, c("cyl", "mpg", "hp", "wt")] #the new df should have cylinder = 4 and mpg (miles per gallons) greater than 18
head(mycars)

#index by columns and row numbers
mtcars[1:6,1:6] #rows 1 to 6 and columns 1 to 6

#engineering a new feature or columns
mycars$mpgPerWeight = mycars$mpg / mycars$wt #mpgPerWeight is the new column name
head(mycars)

mean(mtcars$cyl)
median(mtcars$cyl)
sd(mtcars$cyl)
#Inter Quartile Range:the middle 50% of a dataset by subtracting the first quartile (Q1) from the third quartile (Q3)
IQR(mtcars$cyl)

install.packages(psych)
library(psych)
ggplot2::`%+%`()
ggplot2::alpha()
describe(mtcars)
boxplot(mtcars)

#histogram for each column, nClass is the number of bins
hist(mtcars$cyl, nclass = 4)

#Tidyverse
library(tidyverse)
mycars <- filter(mtcars, cyl == 4 & mpg > 18)
mycars <- select(mycars, cyl, mpg, hp, wt)
mycars <- mutate(mycars, mpgPerWeight = mpg/wt) #mutating is changing and adding a new column
head(mycars)

#Piping: x %>% f(a,b) same as f(x,a,b)
# x %>% f(a, b) %>% g(c,d) same as g(f(x,a,b), c, d)
# "Hello" %,% "World" output: "Hello, World"
mycars <- mtcars %>% #this gets fed into the filter
  filter(cyl == 4 & mpg > 18) %>%  #output of filter gets fed into select
  select(mycars, cyl, mpg, hp, wt) %>% #output of select gets fed/piped into mutate
  mutate(mpgPerWeight = mpg/wt)
head(mycars)

# Rename_with accepts a function and applies it on all column names
mycars %>% rename_with(tolower) #everything is converted to lower case

#relocate(move) the column position
mycars %>% relocate(mpgPerWeight, .after = mpg)

# GROUPING and SUMMARIZING
install.packages("hflights")
library(hflights)
data(hflights)
flights = hflights #recreating another version of the dataset, copying

flights %>%
  group_by(Dest) %>% #group by destination and pipe to next
  # n() gives you the counts; "count =" is used to rename the column
  summarise("count" = n()) 

flights %>%
  group_by(Dest) %>%
  # na.rm parameter here handles the missing values
  summarize(avg_delay = mean(ArrDelay, na.rm=TRUE))

# for each destination, show the number of cancelled and non-cancelled flights
flights %>%
  group_by(Dest) %>%
  select(Dest, Cancelled) %>%
  table() %>%
  head()

flights %>% filter(Dest == "AEX") %>% #destination with AEX
  rowwise() %>%
  mutate(TotalDelay = sum(ArrDelay, DepDelay)) %>%
  summarise(TotalDelay)

# PIVOTING : changing structure of the data frame, rows into columns and columns into rows4
arrest <- USArrests[,1:3] # take first three columns for the demo, nothing before comma means take all rows

# Notice
arrest <- arrest %>%
  rownames_to_column(var = "State") %>%
  pivot_longer(cols = c("Murder", "Assault"), names_to="Crime", values_to="value")
head(arrest)

install.packages("astsa")
library(astsa)
head(gdp)
data("gdp")

#convert to data frame
df <- as.data.frame(gdp)
#add a cell representing year and quarter
df$quarter <- seq(1947.25, 2018.75, by=0.25) #start,end,interval
head(df)
df

install.packages("ggplot2")
library(ggplot2)

#convert ggplot object
plt <- ggplot(df, aes(x=quarter, y=x)) #aes is aesthetics
# Add a line layer
plt + geom_line() #geom is geometry, adding a geometric line to ggplot object
ggplot(df, aes(x = quarter, y = x)) +
  geom_line() +
  xlab("Time") +
  ylab("GDP") +
  ggtitle("GDP time series")
  
#Bar chart
head(diamonds)
summary(diamonds)
ggplot(diamonds, aes(x=clarity)) + geom_bar()

#stack is default, hence it returns a graph like before
p <- ggplot(diamonds, aes(x=clarity, fill = cut))
p + geom_bar(position="stack") #stacked bar plot

#geom_col with y
p <- ggplot(diamonds, aes(x=clarity, y=price, fill = cut)) #saving plot as object p
p + geom_col()

#histogram
ggplot(mtcars, aes(hp)) + geom_histogram(binwidth = 60)

p <- ggplot(data=mtcars, aes(x=hp)) +
  geom_histogram(binwidth = 30) +  
facet_wrap(~cyl)  
p

#Boxplot
#by default the y-axis is in terms of count but can be redefined
ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) +
  geom_boxplot() + 
  xlab("cyl")

#Scatterplot
ggplot(mtcars, aes(mpg, hp)) + geom_point() +
  geom_smooth(method = lm) #lm = linear model to add regression line

ggplot(mtcars, aes(mpg,hp)) + geom_point(alpha=0.5) + geom_smooth(method = lm)

#Contigency plots
#color is weight of the car
ggplot(mtcars, aes(x=cyl, y=gear, fill=wt)) + geom_tile()

#QQ plot: to check normslity of ddata.. multiple info in the same plot
ggplot(mtcars, aes(sample = hp)) +
  stat_qq() +
  stat_qq_line(col = "red")

#JOINING and FINDING ERRORS
#missing values
library(hflights)
data("hflights")
flights = hflights #creating a copy
summary(hflights$DepTime)
 #sum function of na also gives total missing values
sum(is.na(flights$DepTime))
