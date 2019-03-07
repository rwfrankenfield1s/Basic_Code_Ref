install.packages("mclust")
install.packages("printr")


library(MASS)
library(mclust)
library(printr)


iris


model <- Mclust(subset(iris, select = -Species))


table(model$classification, iris$Species)

plot(model)






