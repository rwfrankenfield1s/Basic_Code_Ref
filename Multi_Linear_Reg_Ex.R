# Set Directory
setwd('C://Users/rwfra/Documents/MSBDA/BDAT 640/Week 2/Data')

# Install packages for commands
install.packages('readxl')
install.packages('MASS')
install.packages('ggplot2')
install.packages('lattice')

# Install libraries to apply
library(readxl)
library(MASS)
library(ggplot2)
library(lattice)

# Load db
data <- read_excel('mtcars.xlsx')

summary(data)


# lm() on mpg and hp (simple regression)
modelFit <- lm(formula=mpg ~ hp, data=data)
summary(modelFit)

###Since the p-value is higher than .05 there is no statistical significance and the model does not fit the data well.

data$mpg
#



# Since the coefficient of hp is negative, so is the relationship

#calcualte strength of relationship (RSE / mean(mpg)). Can also use R-squared to show percentage of reason
summary(data$mpg) #to find mean
3.5/20.15 # 17.369% error
#60% = #rSquared/ 60% #explains mpg to hp


# Predicted confidence values of mpg 
predict(modelFit, data.frame(hp = 100), interval = "confidence")

# Predicted prediction values of mpg 
predict(modelFit, data.frame(hp = 100), interval = "prediction")

# Plot response and predictor

plot(data$hp, data$mpg, main = "mpg vs. hp", xlab = "hp", ylab="mpg", col = "black")
abline(modelFit, col = "green")

# Multiple linear regression 

names(data)

fit <- lm(mpg ~ cyl+disp+hp+wt+vs+gear, data = data)
summary(fit)
##Hp and wt are statisticly significant

#column 1 left out because it is not numeric
cor(data[2:11])

fitfit <- lm(mpg ~ cyl*disp + am*gear, data = data )
summary(fitfit)
#Yes this interaction appears to be significant. cyl and disp

##The p value in comparison to the f statistic shows that there is a relationship between all of the 
##predictors and the response. Cyl and Disp are both significant out of the predictors when testing the p value against the t statistic.



