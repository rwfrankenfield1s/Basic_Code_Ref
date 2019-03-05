#Install load Packages
install.packages('e1071')

library(e1071)

#Load Data Set Directory

setwd("C://Users/rwfra/Documents/MSBDA/BDAT 640/Week 6/Data")

data <- read.csv('Bike.csv')
View(data)
set.seed(101092)
data$holiday <- factor(data$holiday)

############train
train <- sample(1:nrow(data), nrow(data)/3) 


############SVM

svm.fit <- svm(holiday ~ season+workingday+casual+registered, data= data[train, ], kernal="radial", gamma= 10, cost= 100)
summary(svm.fit)

#### Tune function for grid search

tune.out <- tune(svm, holiday ~ season+workingday+casual+registered, data= data[train, ], kernel="radial", 
               ranges= list(cost = c(1,10,50,100), gamma= c(1,3, 5)))
summary(tuneIT)

#Forecast with predict function

pred <- predict(tune.out$best.model, newdata = data[-train, ])
tObserve <- data[-train, "holiday"]


cf <- table(tObserve, pred)

print(cf)


