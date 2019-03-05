library(lattice)
library(nutshell)
data(births2006.smpl)
births2006.smpl[1:5,]

dim(births2006.smpl)

births.dow=table(births2006.smpl$DOB_WK)
births.dow

barchart(births.dow,ylab="Day of Week", col="black")

dob.dm.tbl=table(WK=births2006.smpl$DOB_WK,
            
                      MM=births2006.smpl$DMETH_REC)
dob.dm.tbl

dob.dm.tbl=dob.dm.tbl[,-2]
dob.dm.tbl

trellis.device()
barchart(dob.dm.tbl,ylab="Day of Week")
barchart(dob.dm.tbl,horizontal=FALSE,groups=FALSE,
xlab="Day of Week",col="black")

histogram(~DBWT|DPLURAL,data=births2006.smpl,layout=c(1,5),
col="black")
histogram(~DBWT|DMETH_REC,data=births2006.smpl,layout=c(1,3),
col="black")

densityplot(~DBWT|DPLURAL,data=births2006.smpl,layout=c(1,5),
plot.points=FALSE,col="black")
densityplot(~DBWT,groups=DPLURAL,data=births2006.smpl,
plot.points=FALSE)

dotplot(~DBWT|DPLURAL,data=births2006.smpl,layout=c(1,5),
plot.points=FALSE,col="black")

xyplot(DBWT~DOB_WK,data=births2006.smpl,col="black")
xyplot(DBWT~DOB_WK|DPLURAL,data=births2006.smpl,layout=c(1,5),
col="black")

xyplot(DBWT~WTGAIN,data=births2006.smpl,col="black")
xyplot(DBWT~WTGAIN|DPLURAL,data=births2006.smpl,layout=c(1,5),
col="black")

smoothScatter(births2006.smpl$WTGAIN,births2006.smpl$DBWT)

boxplot(DBWT~APGAR5,data=births2006.smpl,ylab="DBWT",
xlab="AGPAR5")

boxplot(DBWT~DOB_WK,data=births2006.smpl,ylab="DBWT",
xlab="Day of Week")

bwplot(DBWT~factor(APGAR5)|factor(SEX),data=births2006.smpl,
xlab="AGPAR5")

bwplot(DBWT~factor(DOB_WK),data=births2006.smpl,
xlab="Day of Week")

fac=factor(births2006.smpl$DPLURAL)
res=births2006.smpl$DBWT
t4=tapply(res,fac,mean,na.rm=TRUE)
t4

t5=tapply(births2006.smpl$DBWT,INDEX=list(births2006.smpl$DPLURAL,
births2006.smpl$SEX),FUN=mean,na.rm=TRUE)
t5

barplot(t4,ylab="DBWT")
barplot

t5=table(births2006.smpl$ESTGEST)
t5

new=births2006.smpl[births2006.smpl$ESTGEST != 99,]
t51=table(new$ESTGEST)
t51

t6=tapply(new$DBWT,INDEX=list(cut(new$WTGAIN,breaks=10),
cut(new$ESTGEST,breaks=10)),FUN=mean,na.rm=TRUE)
t6
levelplot(t6,scales = list(x = list(rot = 90)))

contourplot(t6,scales = list(x = list(rot = 90)))

