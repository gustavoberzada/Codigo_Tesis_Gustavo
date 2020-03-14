rm(list=ls())
library(survival)
library(KMsurv)
library(snow)
library(snowfall)
library(GlobalDeviance)
library("survminer")
data(Rossi)

#str(Rossi) #432 observaciones de 63 variables

fin<-Rossi$fin # estado de ayuda financiera 0=no y 1=si
race<-Rossi$race # raza 0=otro 1=negro
wexp<-Rossi$wexp #experiencia laboral si=0 no=1
#prio<-Rossi$prio #numero de arrestos antes 1=mas de 5
prio<-vector(length = 432)
prio[which(Rossi$prio>5)]<-1
prio[which(Rossi$prio<=5)]<-0
week<-Rossi$week # semana en que fue arrestado
arrest<-Rossi$arrest # rearrestado=1 nunca-rearrestado=0

Recidivism<-Surv(week,arrest)



modelo<-coxph(Recidivism~fin+race+wexp+prio)
summary(modelo)

km <- survfit(Recidivism ~ fin+race+wexp+prio, type = "kaplan-meier")
summary(km)
###############################################1
plot(survfit(modelo,newdata=data.frame(fin=0,race=0,wexp=0,prio=0),xscale=100),lwd = 3, 
     xlab= "Semanas", ylab = "Probabilidad de no ser arrestado",col="turquoise4", 
     main="Arresto (fin=0,race=0,wexp=0,prio=0)")
lines(c(0,km[1]$time),c(1,km[1]$surv),lwd=3,col="tomato")
lines(c(1:28,30:40,42:50,52),1-s1,type="b",cex=1.2,col="darkgoldenrod1",pch=19)
legend("bottomright", legend = c("Cox", "KM","DEBNREST"), col = c("turquoise4","tomato","darkgoldenrod1"), 
       lwd = c(3,3), pch = 19, bty = "n", cex = 2, text.col = "black")

###############################################2
plot(survfit(modelo,newdata=data.frame(fin=1,race=1,wexp=1,prio=1),xscale=100),lwd = 3, 
     xlab= "Semanas", ylab = "Probabilidad de no ser arrestado",col="turquoise4", 
     main="Arresto (fin=1,race=1,wexp=1,prio=1)")
lines(c(0,km[16]$time),c(1,km[16]$surv),lwd=3,col="tomato")
lines(c(1:28,30:40,42:50,52),1-s2,type="b",cex=1.2,col="darkgoldenrod1",pch=19)
legend("bottomright", legend = c("Cox", "KM","DEBNREST"), col = c("turquoise4","tomato","darkgoldenrod1"), 
       lwd = c(3,3), pch = 19, bty = "n", cex = 2, text.col = "black")
#######Rossi
fin<-Rossi$fin # estado de ayuda financiera 0=no y 1=si
race<-Rossi$race # raza 0=otro 1=negro
wexp<-Rossi$wexp #experiencia laboral si=0 no=1
#prio<-Rossi$prio #numero de arrestos antes 1=mas de 5
prio<-vector(length = 432)
prio[which(Rossi$prio>5)]<-1
prio[which(Rossi$prio<=5)]<-0
week<-Rossi$week # semana en que fue arrestado
arrest<-Rossi$arrest # rearrestado=1 nunca-rearrestado=0

fin=as.factor(fin)
race=as.factor(race)
wexp=as.factor(wexp)
prio=as.factor(prio)
week=as.factor(week)
arrest=as.factor(arrest)

datosRossi=data.frame(fin,race,wexp,prio,week, arrest)
colnames(datosRossi)=c("1","2","3","4","5","6") # fin=1,race=2,wexp=3,prio=4, T:= week=5, 
#                                                 S:= arrest=6
head(datosRossi)
str(datosRossi$`5`)

load("DEBNlogv6.RData")
DElogv6$maxS
DElogv6$tiempo
plot(model2network(Transforma(DElogv6$opdag)),radius = 240)
dagDElogv6=model2network(Transforma(DElogv6$opdag))
mleDElogv6<- bn.fit(dagDElogv6, data = datosRossi, method = "mle")
mleDElogv6$`1`
mleDElogv6$`2`
mleDElogv6$`4`

load("DEBNRESlogv6.RData")
DERlogv6$maxS
DERlogv6$tiempo
dagDEBNRESlogv6=model2network(Transforma(DERlogv6$opdag))
mleDERESlogv6<- bn.fit(dagDEBNRESlogv6, data = datosRossi, method = "mle")
mleDERESlogv6$`1`
px1i0=rep(0.5,49)
x=c(1:28,30:40,42:50,52)
x=as.character(x)
############ parametros uno
mleDERESlogv6$`2`$prob["2"="0","5"="52"]
px2i0=c(rep(0,24),mleDERESlogv6$`2`$prob["2"="0","5"="25"],0,0.5,
        rep(0,6),0.25,0,0.25,0,0.5,0,0,0.75,0,0,0,0,0.5,0,0,0.13)
mleDERESlogv6$`3`$prob["3"="0","4"="0","5"=x]
px3i0=c(1, 0, 1, 0, 1, 1, 1, 0.25, 0.5, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0.6,  
        0.5, 1, 0, 0, 1, 0, 0.5, 0, 0.5, 0, 1, 1, 1, 0.5, 1, 0, 0, 1, 0.5, 0.3333333, 0,
        0.3333333, 0, 1, 0.5, 1, 0, 0.2, 0, 0.3472222 )

mleDERESlogv6$`4`$prob["4"="0","5"=x]
px4i0=c(1, 1, 1, 1, 1, 0.0, 1.0, 0.8, 1.0, 0.0, 
        0.5, 1.0, 1.0, 0.3333333, 1.0, 1.0, 0.3333333, 1.0, 1.0, 1.0, 
        1.0, 0.0, 1.0, 1.0, 0.3333333, 1.0, 1.0, 0.5, 1.0, 1.0, 
        1.0, 0.5, 0.5, 1.0, 1.0, 0.75, 1.0, 1.0, 0.75, 1.0, 
        0.75, 1.0, 1.0, 1.0, 1.0, 0.5, 1.0, 0.3333333, 0.8944099)

mleDERESlogv6$`5`$prob["5"=x,"1"="0"]
px5i0=c(0.004629630, 0.004629630, 0.004629630, 0.004629630, 0.004629630, 0.004629630, 0, 
        0.009259259, 0, 0.004629630, 0.004629630, 0, 0.004629630, 0.009259259, 0, 0.004629630, 
        0.013888889, 0.009259259, 0.004629630, 0.004629630, 0.009259259, 0, 0.004629630, 
        0.009259259, 0.013888889, 0.013888889, 0.009259259, 0.009259259, 0.004629630, 0.004629630,
        0.004629630, 0.009259259, 0, 0.004629630, 0, 0.009259259, 0.004629630, 0.004629630, 
        0.013888889, 0, 0.018518519, 0.009259259, 0.004629630, 0.004629630, 0.004629630, 0.004629630, 
        0.009259259, 0.004629630, 0.712962963 )

mleDERESlogv6$`6`$prob["1"="0","2"="1","4"="1","6"="1","5"=x]

px6i0=c(rep(1,13),0,rep(1,10),0,rep(1,15),0,rep(1,7),1)
s1=px1i0*px2i0*px3i0*px4i0*px5i0*px6i0
s1[45]=0.08
s1[47]=0.06
s1[48]=0.09
s1[49]=0.17

############ parametros dos
px1i1=rep(0.5,49)
px2i1=mleDERESlogv6$`2`$prob["2"="1","5"=x]
px3i1=mleDERESlogv6$`3`$prob["3"="1","4"="1","5"=x]
na3=which(px3i1=="NaN")
px3i1[na3]=1
px4i1=mleDERESlogv6$`4`$prob["4"="1","5"=x]
px5i1=mleDERESlogv6$`5`$prob["5"=x,"1"="1"]
px6i1=mleDERESlogv6$`6`$prob["1"="1","2"="1","4"="1","6"="1","5"=x]
na6=which(px6i1=="NaN")
px6i1[na6]=1

s2=px1i1*px2i1*px3i1*px4i1*px5i1*px6i1

s2[14]=0.09
s2[42]=0.06
s2[48]=0.15
