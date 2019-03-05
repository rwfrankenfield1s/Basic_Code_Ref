install.packages("car")

library(car)
set.seed(1)

del <- read.csv("C:/Users/rwfra/Documents/MSBDA/BDAT 625/FlightDelays.csv")
del[1:3,]

del$sched=factor(floor(del$schedtime/100))
table(del$sched)
table(del$carrier)
table(del$dest)
table(del$origin)
table(del$weather)
table(del$dayweek)
table(del$daymonth)
table(del$delay)

del$delay=recode(del$delay,"'delayed'=1;else=0")
del$delay=as.numeric(levels(del$delay)[del$delay])
table(del$delay)

del$dayweek=recode(del$dayweek,"c(1,7)=1;else=0")
table(del$dayweek)

del=del[,c(-1,-3,-5,-6,-7,-11,-12)]
del[1:3,]
n=length(del$delay)
n
n1=floor(n*(0.6))
n1
n2=n-n1
n2

train=sample(1:n,n1)

Xdel <- model.matrix(delay~.,data=del)[,-1] 
Xdel[1:3,]
xtrain <- Xdel[train,]
xnew <- Xdel[-train,]
ytrain <- del$delay[train]
ynew <- del$delay[-train]
m1=glm(delay~.,family=binomial,data=data.frame(delay=ytrain,xtrain))
summary(m1)

ptest <- predict(m1,newdata=data.frame(xnew),type="response")
data.frame(ynew,ptest)[1:10,]


plot(ynew~ptest)

gg1=floor(ptest+0.5)	## floor function; see help command
ttt=table(ynew,gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error

gg2=floor(ptest+0.7)
ttt=table(ynew,gg2)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error

bb=cbind(ptest,ynew)
bb
bb1=bb[order(ptest,decreasing=TRUE),]
bb1

xbar=mean(ynew)
xbar

axis=dim(n2)
ax=dim(n2)
ay=dim(n2)
axis[1]=1
ax[1]=xbar
ay[1]=bb1[1,2]
for (i in 2:n2) {
  axis[i]=i
  ax[i]=xbar*i
  ay[i]=ay[i-1]+bb1[i,2]
}

aaa=cbind(bb1[,1],bb1[,2],ay,ax)
aaa[1:100,]

plot(axis,ay,xlab="number of cases",ylab="number of successes",main="Lift: Cum successes sorted by pred val/success prob")
points(axis,ax,type="l")

