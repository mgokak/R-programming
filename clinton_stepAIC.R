clinton <- read.csv("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week4/clinton.csv")

str(clinton)

model1 <- lm(Percent.voting.for.Clinton.in.1992 ~ Median.Age + Mean.Savings + PerCapita.Income + 
           Percent.in.Poverty + Percent.Veterans + Percent.Female + Population.Density + 
           Percent.in.Nursing.Homes + Crime.Index, data=clinton)
summary(model1)

model3 <- lm(Percent.voting.for.Clinton.in.1992 ~ Mean.Savings + Percent.in.Poverty + Percent.Veterans + 
           Percent.Female + Population.Density, data=clinton)
summary(model3)

sum(is.na(clinton$Mean.Savings))

clinton$MS2 <- (clinton$Mean.Savings/1000) * (clinton$Mean.Savings/1000)
clinton$PIP2 <- clinton$Percent.in.Poverty * clinton$Percent.in.Poverty
clinton$PV2 <- clinton$Percent.Veterans * clinton$Percent.Veterans
clinton$PF2 <- clinton$Percent.Female * clinton$Percent.Female
clinton$PD2 <- clinton$Population.Density * clinton$Population.Density
clinton$Mean.Savings <- as.numeric(clinton$Mean.Savings)
clinton$MS2 <- clinton$Mean.Savings * clinton$Mean.Savings

model4 <- lm(Percent.voting.for.Clinton.in.1992 ~ Mean.Savings + Percent.in.Poverty + Percent.Veterans + 
           Percent.Female + Population.Density + MS2 + PIP2 + PV2 + PF2 + PD2, data=clinton)
summary(model4)

set.seed(123)
partition <- sample(2, nrow(clinton), replace=TRUE, prob = c(0.80, 0.20))
train <- clinton[partition==1 ,]
test <- clinton[partition==2 ,]
dim(train)
dim(test)

m1 <- lm(Percent.voting.for.Clinton.in.1992 ~ Median.Age + Mean.Savings + PerCapita.Income + 
           Percent.in.Poverty + Percent.Veterans + Percent.Female + Population.Density + 
           Percent.in.Nursing.Homes + Crime.Index, data=clinton1992)
summary(m1)

m1.train <- lm(Percent.voting.for.Clinton.in.1992 ~ Median.Age + Mean.Savings + PerCapita.Income + 
                 Percent.in.Poverty + Percent.Veterans + Percent.Female + Population.Density + 
                 Percent.in.Nursing.Homes + Crime.Index, data=train)
summary(m1.train)

prediction <- predict(m1, test)
actual = test$Percent.voting.for.Clinton.in.1992
cor(prediction,actual)

m4 <- lm(Percent.voting.for.Clinton.in.1992 ~ Median.Age + Mean.Savings + PerCapita.Income + 
           Percent.in.Poverty + Percent.Veterans + Percent.Female + Population.Density + 
           Percent.in.Nursing.Homes + Crime.Index + MS2 + PIP2 + PV2 + PF2 + PD2, data=clinton)

library(MASS)
help(stepAIC)

step <- stepAIC(m4, direction="backward")

clinton1992_clean <- na.omit(clinton1992)
m4_clean <- lm(Percent.voting.for.Clinton.in.1992 ~ Median.Age + Mean.Savings + PerCapita.Income + 
                 Percent.in.Poverty + Percent.Veterans + Percent.Female + Population.Density + 
                 Percent.in.Nursing.Homes + Crime.Index + MS2 + PIP2 + PV2 + PF2 + PD2, data=clinton1992_clean)
step <- stepAIC(m4_clean, direction="backward")

m5 <- lm(Percent.voting.for.Clinton.in.1992 ~ Median.Age + Mean.Savings + PerCapita.Income + 
           Percent.in.Poverty + Percent.Veterans + Percent.Female + Population.Density + 
           Percent.in.Nursing.Homes + Crime.Index + MS2 + PIP2 + PV2 + PF2 + PD2, data=clinton)

m6 <- lm(Percent.voting.for.Clinton.in.1992 ~ 1, data=clinton)
summary(m6)

step <- stepAIC(m6,direction="forward", scope=list(upper=m5,lower=m6))
