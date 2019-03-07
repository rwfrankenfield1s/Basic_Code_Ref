install.packages("e1071")
install.packages("printr")

library(e1071)
library(printr)

data <- iris

str(data)

train.indicies <- sample(1:nrow(data),100)

iris.train <- iris[train.indicies,]
iris.test <- iris[-train.indicies,]

model <- svm(Species ~ ., data = iris.train)

results <- predict(object = model, newdata = iris.test, type = "class")

table(results, iris.test$Species)







