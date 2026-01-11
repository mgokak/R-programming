# MULTICOLLINEARITY: independent variables are highly correlated with each other, 
# making it difficult to determine the individual effect of each variable on the dependent variable
# multicollinearity is 1 to many and Pearson correlation is 1 tot 1

# bodyfat : dependent variable
bodyfat <- read.csv("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week5/bodyfat.csv")
str(bodyfat)
dim(bodyfat)
summary(bodyfat)

bodyfat$Density <- NULL
bodyfat$Age <- NULL
bodyfat$Height <- NULL
bodyfat$Neck <- NULL
bodyfat$Knee <- NULL
bodyfat$Ankle <- NULL
bodyfat$Forearm <- NULL
bodyfat$Wrist <- NULL
dim(bodyfat)
head(bodyfat)

plot(bodyfat)

cor(bodyfat) # there is a lot of multi-collinarity between variables,
#hence we got to do a few stuff before

model <- lm(BodyFat ~ Weight + Chest + Abdomen + Hip + Thigh + Biceps, data = bodyfat)
summary(model)

# Weight and hip are highly correlated with ~93%, build two models, one without weight and 
# one without hip

model1 <- lm(BodyFat ~ Chest + Abdomen + Hip + Thigh + Biceps, data = bodyfat)
summary(model)

model2 <- lm(BodyFat ~ Weight + Chest + Abdomen + Thigh + Biceps, data = bodyfat)
summary(model2)
#---using Weight gives a better model than using Hip but skip hip

# now chest and abdomen, corr = 91%, now remove one and try with the other
model3 <- lm(BodyFat ~ Weight + Abdomen + Hip + Thigh + Biceps, data = bodyfat)
summary(model3)

model4 <- lm(BodyFat ~ Weight + Chest + Hip + Thigh + Biceps, data = bodyfat)
summary(model4)
# without abdomen, adjusted R-square reduces drastically, so not a good one
#---instead, drop Chest and use Abdomen

#use weight, abdomen, thigh and drop biceps
model5 <- lm(BodyFat ~ Weight + Abdomen + Thigh, data = bodyfat)
summary(model5)
#---this is least significant of all

#leave thigh, add biceps
model6 <- lm(BodyFat ~ Weight + Abdomen + Biceps, data = bodyfat)
summary(model6)
#---biceps can be dropped aswell

model7 <- lm(BodyFat ~ Weight + Abdomen, data = bodyfat)
summary(model7)


# VARIATION INFLATION FACTOR (VIF): statistical measure used in multiple regression to 
# identify and quantify multicollinearity
install.packages("car")
library(car)

model <- lm(BodyFat ~ Weight + Chest + Abdomen + Hip + Thigh + Biceps, data = bodyfat)
summary(model)

vif(model) #no vif for dependent variable(BodyFat). vif is one to many
# IV(independent variables), vif > 10 then the IV is >90% correlated with the other variables
# so both weight and hip

#weight has highest vif so skipping it
model8 <- lm(BodyFat ~ Chest + Abdomen + Hip + Thigh + Biceps, data = bodyfat)
vif(model8)

#VARIABLE TRANSFORMATION
url <- "http://lib.stat.cmu.edu/datasets/CPS_85_Wages"
destination_file <- "M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week5/Wages.txt"
download.file(url, destination_file, method = "auto")
names <- c(
  "EDUCATION: Number of years of education",
  "SOUTH: 1=Person lives in South, 0=Person lives elsewhere",
  "SEX: 1=Female, 0=Male",
  "EXPERIENCE: Number of years of work experience",
  "UNION: 1=Union member, 0=Not union member",
  "WAGE: Wage (dollars per hour)",
  "AGE: years",
  "RACE: 1=Other, 2=Hispanic, 3=White",
  "OCCUPATION: 1=Management, 2=Sales, 3=Clerical, 4=Service, 5=Professional, 6=Other",
  "SECTOR: 0=Other, 1=Manufacturing, 2=Construction",
  "MARR: 0=Unmarried, 1=Married"
)
short_names <- sapply(strsplit(names, ":"), `[`, 1)

Wages <- read.table("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week5/Wages.txt", skip = 27, 
                    header = FALSE, sep = "", fill = TRUE)
colnames(Wages) <- short_names
str(Wages)
dim(Wages)
summary(Wages)

#  converting char to numeric and removing NAs
Wages[] <- lapply(Wages, function(x) as.numeric(as.character(x)))
sapply(Wages, function(x) sum(is.na(as.numeric(as.character(x)))))
head(Wages[!complete.cases(Wages), ])
Wages[] <- lapply(Wages, function(x) {
  x <- trimws(x)                       # remove spaces
  x <- gsub("[$,]", "", x)             # remove symbols
  as.numeric(x)
})
str(Wages)
sum(is.na(Wages))
cor(Wages, use = "complete.obs")

plot(Wages)

Wages$RACE <- as.factor(Wages$RACE)
Wages$OCCUPATION <- as.factor(Wages$OCCUPATION)
Wages$SECTOR <- as.factor(Wages$SECTOR)
# union, sex and probably South can also be represented as factor as they are categorical
str(Wages)

model <- lm(WAGE ~ EDUCATION + SOUTH + SEX + EXPERIENCE + UNION + AGE + RACE +
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)

hist(Wages$WAGE, breaks = 20)
# its right skews so transform to log, something like income and price always fit for log transformation
hist(log(Wages$WAGE), breaks = 20)

model <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX + EXPERIENCE + UNION + AGE + RACE +
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)

# experience is also a good candidate for log. In most wage or earnings datasets, years of 
# experience doesn’t increase wages linearly. At early stages, every extra year of experience 
# can raise wages sharply. But after many years, the effect tapers off — each additional year 
# adds less to the wage.A log transformation on EXPERIENCE makes that relationship more linear
model <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX + log(EXPERIENCE) + UNION + AGE + RACE +
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)

# we get error as experience can be 0 and log cant be done on that, hence add 1
model <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX + log(EXPERIENCE + 1) + UNION + AGE + RACE +
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)

# AGE and EDUCATION usually have a nonlinear effect on outcomes such as WAGE, INCOME, or PRODUCTIVITY.
# Early changes (e.g., from age 20 → 30) have a bigger impact than later ones (e.g., 50 → 60).
# Similarly, increasing education from 8 → 12 years has a stronger impact on wages than 16 → 20 years.
# So the raw linear model might not fit well because the relationship “flattens out.”
# The log transformation helps make that curve more linear:

model <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX + log(EXPERIENCE + 1) + UNION + log(AGE) + RACE +
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)

# with just log of age, it didnt change much so remove that and add log on education
model <- lm(log(WAGE) ~ log(EDUCATION) + SOUTH + SEX + log(EXPERIENCE + 1) + UNION + AGE + RACE +
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)
# both didnt help so no need to transform

# try remove race
model <- lm(log(WAGE) ~ log(EDUCATION) + SOUTH + SEX + log(EXPERIENCE + 1) + UNION + AGE +
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)

# remove marriage, as its hightest p-value
model <- lm(log(WAGE) ~ log(EDUCATION) + SOUTH + SEX + log(EXPERIENCE + 1) + UNION + AGE +
              OCCUPATION + SECTOR, data = Wages)
summary(model)

# remove age now, intutively shouldnt discriminate with age
model <- lm(log(WAGE) ~ log(EDUCATION) + SOUTH + SEX + log(EXPERIENCE + 1) + UNION +
              OCCUPATION + SECTOR, data = Wages)
summary(model)







