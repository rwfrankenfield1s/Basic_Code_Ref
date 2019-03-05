install.packages('textir')
install.packages('class')

library(textir)
library(class)

credit <- read.csv("C:/Users/rwfra/Documents/MSBDA/BDAT 625/germancredit.csv")
credit

credit$Default <- factor(credit$Default)

credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

credit <- credit[,c("Default","duration","amount","installment","age",                    "history", "purpose","foreign","rent")]
credit[1:3,]
summary(credit) # check out the data

x=credit[,c(2,3,4)]
x[,1]=(x[,1]-mean(x[,1]))/sd(x[,1])
x[,2]=(x[,2]-mean(x[,2]))/sd(x[,2])
x[,3]=(x[,3]-mean(x[,3]))/sd(x[,3])

x[1:3,]

set.seed(1)

train <- sample(1:1000,900)
xtrain <- x[train,]
xnew <- x[-train,]
ytrain <-credit$Default[train]
ynew <- credit$Default[-train]

library(class)

nearest1 <- knn(train=xtrain, test=xnew, cl=ytrain, k=1)
nearest3 <- knn(train=xtrain, test=xnew, cl=ytrain, k=3)
data.frame(ynew,nearest1,nearest3)[1:10,]

pcorrn1=100*sum(ynew==nearest1)/100
pcorrn3=100*sum(ynew==nearest3)/100

pcorrn1
pcorrn3

plot(xtrain[,c("amount","duration")],col=c(4,3,6,2)[credit[train,"installment"]],pch=c(1,2)[as.numeri0c(ytrain)],main="Predicted default, by 3 nearest neighbors",cex.main=.95)
points(xnew[,c("amount","duration")],bg=c(4,3,6,2)[credit[train,"installment"]],pch=c(21,24)[as.numeric(nearest3)],cex=1.2,col=grey(.7))
legend("bottomright",pch=c(1,16,2,17),bg=c(1,1,1,1),legend=c("data 0","pred 0","data 1","pred 1"),title="default",bty="n",cex=.8)
legend("topleft",fill=c(4,3,6,2),legend=c(1,2,3,4),title="installment %",horiz=TRUE,bty="n",col=grey(.7),cex=.8)

pcorr=dim(10)
for (k in 1:10) {
  pred=knn.cv(x,cl=credit$Default,k)
  pcorr[k]=100*sum(credit$Default==pred)/1000
}
pcorr

