dpen <- read.csv("C:/Users/rwfra/Documents/MSBDA/BDAT 625/DeathPenalty.csv")
dpen[1:4,]

m1=glm(Death~VRace+Agg,family=binomial,data=dpen)
m1
summary(m1)

exp(m1$coef[2])
exp(m1$coef[3])

fitBlack=dim(501)
fitWhite=dim(501)
ag=dim(501)
for (i in 1:501) {
  ag[i]=(99+i)/100
  fitBlack[i]=exp(m1$coef[1]+ag[i]*m1$coef[3])/(1+exp(m1$coef[1]+ag[i]*m1$coef[3]))
  fitWhite[i]=exp(m1$coef[1]+m1$coef[2]+ag[i]*m1$coef[3])/(1+exp(m1$coef[1]+m1$coef[2]+ag[i]*m1$coef[3]))
}
plot(fitBlack~ag,type="l",col="black",ylab="Prob[Death]",xlab="Aggravation",ylim=c(0,1),main="red line for white victim; black line for black victim")
points(fitWhite~ag,type="l",col="red")




dpenother <- read.csv("C:/Users/rwfra/Documents/MSBDA/BDAT 625/DeathPenalty.csv")
dpenother
m1=glm(Death~VRace+Agg,family=binomial,weights=VRace,data=dpenother)
m1
summary(m1)

exp(m1$coef[2])
exp(m1$coef[3])

