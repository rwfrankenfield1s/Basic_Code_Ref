#Set Directory
setwd("C://Users/rwfra/Documents/MSBDA/BDAT 640/Week 4/Data")

data <- read.csv('boston.csv')

install.packages('ISLR')
install.packages('MASS')
install.packages('splines')
install.packages('boot')
install.packages('caret')
install.packages('ggplot2')
install.packages('glmnet')
install.packages('gam')

library(readr)
library(ISLR)
library(MASS)
library(splines)
library(boot)
library(caret)
library(ggplot2)
library(glmnet)
library(gam)

set.seed(101092)

cubeFit <- lm(nox ~ poly(dis, 3), data = data)
coef(summary(cubFit))
dislims <- range(data$dis)
disGrid <- seq(from = dislims[1], to = dislims[2])

cubicPred <- predict(cubeFit, newdata = list(dis = disGrid), se = TRUE)

se_bands <- cbind(cubicPred$fit + 2*cubicPred$se.fit, 
                  cubicPred$fit - 2*cubicPred$se.fit)

par(mar = c(4.5,4.5,1,1), oma = c(0,0,2,0))

plot(data$dis, data$nox, xlim = dislims, col = "blue", xlab = "dis", ylab = "nox")

lines(disGrid, cubicPred$fit, lwd = 2, col = "red")

matlines(disGrid, se_bands, lwd = 3, col = "orange", lty = 3)




View(data)
names(data)

#poly

lm2 <- lm(dis ~ poly(nox,2), data = data)
summary (lm2)

lm3 <- lm(dis ~ poly(nox,3), data = data)
summary (lm3)

lm4 <- lm(dis ~ poly(nox,4), data = data)
summary (lm4)

lm5 <- lm(dis ~ poly(nox,5), data = data)
summary (lm5)


train_control <- trainControl(method='CV', number=10)


#Select smallest test error 
cv2 <- train(dis ~ poly(nox,2), data=data, trControl=train_control, method="lm")
print(cv2)

cv3 <- train(dis ~ poly(nox,3), data=data, trControl=train_control, method="lm")
print(cv3)

cv4 <- train(dis ~ poly(nox,4), data=data, trControl=train_control, method="lm")
print(cv4)

cv5 <- train(dis ~ poly(nox,5), data=data, trControl=train_control, method="lm")
print(cv5)


##GAM MODEL Soothing Spline


gam1 <- gam(nox ~ s(dis, 3) + s(medv, 2), data = data)
summary(gam1)

gam2 <- gam(nox ~ s(dis, 2) + s(medv, 1), data = data)
summary(gam2)


##Anova test best model
gam1 <- gam(nox ~ s(dis, 3) + s(medv, 2), data = data)
gam2 <- gam(nox ~ s(dis, 2) + s(medv, 1), data = data)
anova(gam1,gam2)
