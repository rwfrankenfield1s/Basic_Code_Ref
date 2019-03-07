install.packages('neuralnet')
install.packages('boot')
install.packages('plyr')

library(MASS)
library(neuralnet)
library(boot)
library(plyr)

#important to set seed and keep things the same
set.seed(4321)

##
#### GOAL ::: predict the median value of owner-occupied homes (medv) using all the other continuous variables available.
##

#pull necessary data in
data <- Boston
View(data)

#Check to make sure no data points are missing. If so fix the dataset accordingly 
apply(data,2,function(x) sum(is.na(x)))


#index and splint data into train and test
index <- sample(1:nrow(data),round(0.75*nrow(data)))

train <- data[index,]
test <- data[-index,]


#fit linear regression model
lm.fit <- glm(medv~., data=train)
summary(lm.fit)


#Since this is a regression problem: use the mean squared error (MSE) as a measure of how much our predictions are far away from the real data
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

MSE.lm


#first step, address data preprocessing
#It is good practice (essential) to normalize the data before training neural network
#many methods for normalizing: Min - Max method is used below
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]


# Neaural Parameters - fitting the net
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

plot(nn)


#Predicting medv using the neural network
#Remember that the net will output a normalized prediction, so we need to scale it back in order to make a meaningful comparison (or just a simple prediction).
pr.nn <- compute(nn,test_[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)


#Then compare the two MSEs
print(paste(MSE.lm,MSE.nn))
#Apparently, the net is doing a better work than the linear model at predicting medv. Once again, be careful because this result depends on the train-test split performed above.


#A first visual approach to the performance of the network and the linear model (MSE.nn and MSE.lm) on the test set is plotted below
par(mfrow=c(1,2))
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
#the predictions made by the neural network are (in general) more concetrated around the line (a perfect alignment with the line would indicate a MSE of 0 and thus an ideal perfect prediction) than those made by the linear model.


#Cross validation is another very important step of building predictive models
set.seed(4321)
#10 fold cross-validated MSE for the linear model
lm.fit <- glm(medv~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]


#Now the net. 
#Note splitting the data in this way: 90% train set and 10% test set in a random way for 10 times
set.seed(4321)
cv.error <- NULL
k <- 10
pbar <- create_progress_bar('text') 
pbar$init(k)
for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)   
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)   
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)    
  pbar$step()
}

mean(cv.error)
cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)








































