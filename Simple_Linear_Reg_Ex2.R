toyota <- read.csv("C:/Users/rwfra/Documents/MSBDA/BDAT 625/ToyotaCorolla.csv")
toyota[1:3,]
summary(toyota)
hist(toyota$Price)

v1=rep(1,length(toyota$FuelType))
v2=rep(0,length(toyota$FuelType))
toyota$FuelType1=ifelse(toyota$FuelType=="CNG",v1,v2)
toyota$FuelType2=ifelse(toyota$FuelType=="Diesel",v1,v2)
auto=toyota[-4]
auto[1:3,]

plot(Price~Age,data=auto)
plot(Price~KM,data=auto)
plot(Price~HP,data=auto)

m1=lm(Price~.,data=auto)
summary(m1)

auto$Age2=auto$Age^2
auto$KM2=auto$KM^2
m11=lm(Price~Age+KM,data=auto)
summary(m11)
m12=lm(Price~Age+Age2+KM+KM2,data=auto)
summary(m12)
m13=lm(Price~Age+Age2+KM,data=auto)
summary(m13)

plot(m11$res~m11$fitted)
hist(m11$res)
plot(m12$res~m12$fitted)
