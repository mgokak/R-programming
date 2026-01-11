bears <- read.csv("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week3/bears1985.csv")
head(bears)
summary(bears)

bears$Ht <- bears$HtFt * 12 + bears$HtIn
bears$Ht
head(bears)

plot(bears$Ht, bears$Weight, main="1985 Bears", xlab="Height", ylab="Weight")

model <- lm(bears$Weight ~ bears$Ht)
summary(model)

model <- lm(Weight ~ Ht, data = bears)
summary(model)

Bears1985.pred <- predict(model, bears)
Bears1985.pred
Bears1985.pred[1]

bears$Weight[1]
bears$Weight[1] - Bears1985.pred[1]

model$residuals
bears[bears$LastName == "Perry", c("FirstName", "LastName", "HtFt", "HtIn", "Weight")]
bears$Weight[33] - Bears1985.pred[33]

Bears1985.pred <- predict(model, bears, interval="confidence")
Bears1985.pred

#-------------------------------------------------------------------------------------------------
Monet <- read.csv("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week3/Monet.csv")
head(Monet)
summary(Monet)

model <- lm(PRICE ~ HEIGHT, data = Monet)

model <- lm(PRICE ~ HEIGHT + WIDTH, data = Monet)
summary(model)

Monet$H_W <- Monet$HEIGHT * Monet$WIDTH
model <- lm(PRICE ~ HEIGHT + WIDTH + H_W, data=Monet)
summary(model)

Monet$Hsq <- Monet$HEIGHT * Monet$HEIGHT
Monet$Wsq <- Monet$WIDTH * Monet$WIDTH
model <- lm(PRICE ~ HEIGHT + WIDTH + Hsq + Wsq, data=Monet)
summary(model)

model <- lm(PRICE ~ HEIGHT + WIDTH + SIGNED, data=Monet)
summary(model)

Monet$H1 <- (Monet$HOUSE == 1) * 1
Monet$H2 <- (Monet$HOUSE == 2) * 1
model <- lm(PRICE ~ HEIGHT + WIDTH + H1 + H2, data=Monet)
summary(model)


