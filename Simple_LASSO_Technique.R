install.packages("lars")

prostate <- read.csv("C:/Users/rwfra/Documents/MSBDA/BDAT 625/prostate.csv")
prostate[1:3,]

m1=lm(lcavol~.,data=prostate)
summary(m1)

x <- model.matrix(lcavol~age+lbph+lcp+gleason+lpsa,data=prostate)
x=x[,-1]  

library(lars)  

lasso <- lars(x=x,y=prostate$lcavol,trace=TRUE)

plot(lasso)
lasso

coef(lasso,s=c(.25,.50,0.75,1.0),mode="fraction")

cv.lars(x=x,y=prostate$lcavol,K=10)

MSElasso25=dim(10) 
MSElasso50=dim(10) 
MSElasso75=dim(10) 
MSElasso100=dim(10) 
set.seed(1)
for(i in 1:10){
  train <- sample(1:nrow(prostate),80)
  lasso <- lars(x=x[train,],y=prostate$lcavol[train])
  MSElasso25[i]= 
    mean((predict(lasso,x[-train,],s=.25,mode="fraction")$fit-prostate$lcavol[-train])^2)
  MSElasso50[i]=  
    mean((predict(lasso,x[-train,],s=.50,mode="fraction")$fit-prostate$lcavol[-train])^2)
  MSElasso75[i]=
    mean((predict(lasso,x[-train,],s=.75,mode="fraction")$fit-prostate$lcavol[-train])^2)
  MSElasso100[i]=
    mean((predict(lasso,x[-train,],s=1.00,mode="fraction")$fit-prostate$lcavol[-train])^2)
}
mean(MSElasso25)
mean(MSElasso50)
mean(MSElasso75)
mean(MSElasso100)

boxplot(MSElasso25,MSElasso50,MSElasso75,MSElasso100,ylab="MSE", sub="LASSO model",xlab="s=0.25 s=0.50 s=0.75 s=1.0(LS)")

