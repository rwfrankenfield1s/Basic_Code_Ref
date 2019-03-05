#Load and Install Packages
install.packages("cluster")
install.packages("factoextra")
library(cluster)
library(factoextra)

#Load Data Set Directory Minus Cust_ID

setwd("C://Users/rwfra/Documents/MSBDA/BDAT 640/Week 7/Data")
data <- read.csv('CreditCards.csv', row.names=1)
View(data)


set.seed(101092)
fviz_nbclust(data, kmeans, method = "gap_stat")


#2 cluster
km.res <- kmeans(data, 2, nstart = 25)
fviz_cluster(km.res, data= data)


















