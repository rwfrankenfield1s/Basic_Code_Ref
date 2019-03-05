install.packages("leaps")

FuelEff <- read.csv("C:/Users/rwfra/Documents/MSBDA/BDAT 625/FuelEfficiency.csv")
FuelEff

plot(GPM~WT,data=FuelEff)
plot(GPM~DIS,data=FuelEff)

m1=lm(GPM~.,data=FuelEff)
summary(m1)

cor(FuelEff)

library(leaps)
X=FuelEff[,2:7]
y=FuelEff[,1]
out=summary(regsubsets(X,y,nbest=2,nvmax=ncol(X)))
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab

m2=lm(GPM~WT,data=FuelEff)
summary(m2)





