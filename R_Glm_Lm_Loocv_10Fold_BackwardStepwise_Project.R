#Set Directory
setwd('C://Users/rwfra/Documents/MSBDA/BDAT 640/Week 3/Data')

#Install Packages for Commands
install.packages('readxl')
install.packages('MASS')
install.packages('ggplot2')
install.packages('lattice')
install.packages('caret')
install.packages('bestglm')


#Install Libraries to Apply
library(readxl)
library(MASS)
library(ggplot2)
library(lattice)
library(caret)
library(bestglm)

#Load DB
data <- read_excel('mtcars.xlsx')

#Look at Data and Change AM to Factor
data <- as.data.frame(data)
data
#Observe the data and what the columns are classfied as
str(data)
#check the AM column classification
levels(data$am)
#Make AM a Factor
data$am <- factor(data$am)
levels(data$am)
#Make Sure That It Worked
str(data)

#Split Data into Training and Test Set
training_set <- data[1:35,]
str(training_set)
test_set <- data[36:42,]
str(test_set)

#GLM Log Model with AM as the response and MPG, CYL, HP, WT as predictors
model <- glm(am ~ mpg + cyl + hp + wt, data=data, family = "binomial")
model
summary(model)

#Use the Predict Function against the model and test_set
p <- predict(model, test_set, type = "response")
summary(p)

# use ifelse to assign column and a table to see output
pp <- ifelse(p > .5, "M","A")
table(pp, test_set$am)

#Check out the confusion matrix
table(test_set$am, p)

#--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--/--#

#Load DB
data1 <- read.csv('Bike.csv')

#LM Seperate
lm(formula = count ~ season, data = data)
lm(formula = count ~ holiday, data = data)
lm(formula = count ~ workingday, data = data)
lm(formula = count ~ weather, data = data)
lm(formula = count ~ registered, data = data)

#LM Together
lm(formula = count ~ season + holiday + workingday + weather + registered , data = data)

#Best fit GLM subset BIC

set.seed(1)

data1 = data1[sample(nrow(data1), 1000),c("season", "holiday", "workingday", "weather", "registered")]

bestglm(data1, IC="BIC")

#loocv
train_control <- trainControl(method='LOOCV')
model1 <- train(season~ workingday + weather , data= data1, trControl=train_control, method='lm')
summary(model1)

#10-fold CV
train_control1 <- trainControl(method='CV', number=10)
model2 <- train(season~ workingday + weather , data= data1, trControl=train_control1, method='lm')
summary(model2)

#Subset cv
bestglm(data1, IC="CV")

#Backward stepwise selection
model3 <- lm(season ~., data=data1)
step.model <- stepAIC(model3, direction = "backward", trace = FALSE)
summary(step.model)
