# Set Directory

setwd('C://Users/rwfra/Documents/MSBDA/BDAT 640/Week 1/Data')

# Install packages for commands

install.packages('lattice')
install.packages('ggplots')
install.packages('plyr')

# Install libraries to apply
library(lattice)
library(ggplot2)
library(MASS)
library(plyr)

# Load db
data <- read.csv('Boston.csv')

# Observe dataframe 500 rows 14 columns
nrow(Boston)
ncol(Boston)
dim(Boston)

Boston[c(1, 100, 500),c('tax', 'medv')]

# Observing correlation 
cor(Boston, use="complete.obs", method="pearson")

# Visualize pairwise on called variables
pairs(Boston[c(1,9,10,13,2)], pch = 21)

# Histogram for Crime + Range
hist(Boston$crim)
range(Boston$crim)

# Number of houses bound by river
nRiver = table(Boston$chas)
nRiver

# Median and range of pupil-teach ration by town
summary(Boston$ptratio)

# Observe room count
nRooms = table(Boston$rm)
nRooms

# Round to nearest whole number
rrms = ceiling(Boston$rm)
rrms
count(rrms)

# Suburbs containing 9 or more
df <- data.frame(Boston)
df
df$rm <- ceiling(Boston$rm)
df

df[df$rm == "9",]

# Convert to chas to factor
str(Boston$chas)

df <- data.frame(Boston)
df
levels(Boston$chas)

df$chas <- factor(Boston$chas)
levels(df$chas)


boxplot(medv~chas, data=Boston, Main="Does Charles River Affect Home Price", xlab="1 Means Househeld Near River", ylab="Median Value of Househeld ($1000's)")






























































