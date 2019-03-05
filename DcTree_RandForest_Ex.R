#Install load Packages

install.packages('caret')
install.packages('tree')
install.packages('partykit')
install.packages('party')
install.packages('randomForest')
library(caret)
library(tree)
library(partykit)
library(party)
library(randomForest)

#Load Data Set Directory

setwd("C://Users/rwfra/Documents/MSBDA/BDAT 640/Week 3/Data")
data <- read.csv('Bike.csv')
str(data)
#Set Seet Split Data: 66/34 aka 2/3 to 1/3

set.seed(101092)

training_data <- data[1:7185,]
test_data <- data[7186:10886,]

#Tree() function

treeBike <- tree(count ~ season + holiday + workingday + temp + atemp + humidity + windspeed + casual + 
                   registered, data = training_data)
summary(treeBike)

#CV on tree() function

cvTreeBike <- cv.tree(treeBike)

#Plot Results on cv.tree()- 8 has the lowest deviance

plot(cvTreeBike)

#Prune Tree prune.tree()

prune.treeBike <- prune.tree(treeBike, best=4)

plot(prune.treeBike)

text(prune.treeBike, pretty=0)

#Predict Test Error on test set

pred1 <- predict(prune.treeBike, test_data$c)
summary(pred1)

plot(pred1, test_data$registered)

abline(0,1, col="blue")

###########Build Random Forest

rfBike <- randomForest(count ~ season + holiday + workingday + temp + atemp + humidity + windspeed + casual + 
       registered, data = training_data)

#Predict Test Error on test set

pred2 <- predict(rfBike, test_data)
summary(pred2)

plot(pred2, test_data$ci)

abline(0,1, col="green")

#Extract Importance with importance()

importance(rfBike)

#Plot Importance: Top 2 Important Preds are Casual and Registered

varImpPlot(rfBike)





