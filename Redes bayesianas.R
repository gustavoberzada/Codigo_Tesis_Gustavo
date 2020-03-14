setwd("C:/Users/User/Dropbox/Gustavo")
#par(mar=c(0,0,0,0))
rm(list=ls(all=TRUE))

###############################################################
################        Red bayesiana 1      ##################
###############################################################

rm(list=ls(all=TRUE))
source("simula.R")
X1.prob <- c(0.30, 0.70) # Dos factores 1,2
X2.prob <- c(0.10, 0.90) # Dos factores 1,2
X3.prob <- c(0.4, 0.2, 0.4) #Tres factores 1,2,3
X4.prob <- c(0.8, 0.20) #Dos factores 1,2 
t.prob <- c(0.1,0.3,0.6) # Tres factores 1,2,3
S.prob <- matprob(nr=5,nc=72)  #Cinco factores 1,2,3,4 y 5()

##################        Genera X1        ####################
X1 <- rmultinom(n=1000,p=X1.prob)
#table(X1)/1000
##################        Genera X2        ####################
X2 <- rmultinom(n=1000,p=X2.prob)
#table(X2)/1000
##################        Genera X3        ####################
X3 <- rmultinom(n=1000,p=X3.prob)
#table(X3)/1000
##################        Genera X4        ####################
X4 <- rmultinom(n=1000,p=X4.prob)
#table(X4)/1000
##################      Genera tiempo      ####################
t <- rmultinom(n=1000,p=t.prob)
#table(t)/1000
##################        Genera S         ####################
p1<-X1.prob[1]*X2.prob[1]*X3.prob[1]*X4.prob[1]*t.prob
p2<-X1.prob[1]*X2.prob[1]*X3.prob[1]*X4.prob[2]*t.prob
p3<-X1.prob[1]*X2.prob[1]*X3.prob[2]*X4.prob[1]*t.prob
p4<-X1.prob[1]*X2.prob[1]*X3.prob[2]*X4.prob[2]*t.prob
p5<-X1.prob[1]*X2.prob[1]*X3.prob[3]*X4.prob[1]*t.prob
p6<-X1.prob[1]*X2.prob[1]*X3.prob[3]*X4.prob[2]*t.prob
p7<-X1.prob[1]*X2.prob[2]*X3.prob[1]*X4.prob[1]*t.prob
p8<-X1.prob[1]*X2.prob[2]*X3.prob[1]*X4.prob[2]*t.prob
p9<-X1.prob[1]*X2.prob[2]*X3.prob[2]*X4.prob[1]*t.prob
p10<-X1.prob[1]*X2.prob[2]*X3.prob[2]*X4.prob[2]*t.prob
p11<-X1.prob[1]*X2.prob[2]*X3.prob[3]*X4.prob[1]*t.prob
p12<-X1.prob[1]*X2.prob[2]*X3.prob[3]*X4.prob[2]*t.prob
p13<-X1.prob[2]*X2.prob[1]*X3.prob[1]*X4.prob[1]*t.prob
p14<-X1.prob[2]*X2.prob[1]*X3.prob[1]*X4.prob[2]*t.prob
p15<-X1.prob[2]*X2.prob[1]*X3.prob[2]*X4.prob[1]*t.prob
p16<-X1.prob[2]*X2.prob[1]*X3.prob[2]*X4.prob[2]*t.prob
p17<-X1.prob[2]*X2.prob[1]*X3.prob[3]*X4.prob[1]*t.prob
p18<-X1.prob[2]*X2.prob[1]*X3.prob[3]*X4.prob[2]*t.prob
p19<-X1.prob[2]*X2.prob[2]*X3.prob[1]*X4.prob[1]*t.prob
p20<-X1.prob[2]*X2.prob[2]*X3.prob[1]*X4.prob[2]*t.prob
p21<-X1.prob[2]*X2.prob[2]*X3.prob[2]*X4.prob[1]*t.prob
p22<-X1.prob[2]*X2.prob[2]*X3.prob[2]*X4.prob[2]*t.prob
p23<-X1.prob[2]*X2.prob[2]*X3.prob[3]*X4.prob[1]*t.prob
p24<-X1.prob[2]*X2.prob[2]*X3.prob[3]*X4.prob[2]*t.prob

pS <-c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,
       p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24)
mS <- sample(1:72, size = 1000, replace = TRUE,prob = pS)
S <- apply(S.prob[,mS],2,FUN=rmultinomvec)
#table(S)                       0.189 0.155 0.154 0.197 0.305 
##################       Genera Datos      ####################
X1<-as.factor(X1)
X2<-as.factor(X2)
X3<-as.factor(X3)
X4<-as.factor(X4)
t<-as.factor(t)
S<-as.factor(S)
red1<-data.frame(X1,X2,X3,X4,t,S)
##################       Guardar Datos      ####################
probs1<-list(PX1=X1.prob,PX2=X2.prob,PX3=X3.prob,PX4=X4.prob,
            PT=t.prob,Ps=S.prob)
save(red1,probs1,file="Red1.RData")
##################       Cargar  Datos      ####################
#load("Red1.RData")
#head(red1)

###############################################################
################        Red bayesiana 2      ##################
###############################################################

rm(list=ls(all=TRUE))
source("simula.R")
X1.prob <- c(0.30, 0.70) # Dos factores 1,2
X2.prob <- c(0.10, 0.90) # Dos factores 1,2
t.prob <- c(0.1,0.3,0.6) # Tres factores 1,2,3
X3.prob <- matrix(c(0.4, 0.2, 0.4, 0.5, 0.3, 0.2, 
                    0.7, 0.2, 0.1, 0.6, 0.3, 0.1,
                    0.2, 0.1, 0.7, 0.2, 0.2, 0.6,
                    0.8, 0.1, 0.1, 0.1, 0.1, 0.8, 
                    0.15, 0.25, 0.6, 0.5, 0.4, 0.1,
                    0.25, 0.25, 0.5, 0.1, 0.7, 0.2), 
                  ncol = 12,nrow = 3) #Tres factores 1,2,3
X4.prob <- matrix(c(0.2, 0.8, 0.3, 0.7, 0.5, 0.5,
                   0.6, 0.4), ncol =4,nrow = 2)#Dos factores 1,2 
S.prob <- matrix(c(0.25, 0.25, 0.20, 0.10, 0.20,
                   0.6, 0.05, 0.05, 0.15, 0.15,
                   0.2, 0.5, 0.15, 0.05, 0.10,
                   0.1, 0.4, 0.15, 0.15, 0.20,
                   0.7, 0.05, 0.05, 0.10, 0.10,
                   0.25, 0.15, 0.20, 0.05, 0.35), 
                 ncol = 6,nrow = 5) #Cinco factores 1,2,3,4 y 5
##################        Genera X1        ####################
X1 <- rmultinom(n=1000,p=X1.prob)
#table(X1)/1000
##################        Genera X2        ####################
X2 <- rmultinom(n=1000,p=X2.prob)
#table(X2)/1000
##################      Genera tiempo      ####################
t <- rmultinom(n=1000,p=t.prob)
#table(t)/1000
##################        Genera X4        ####################
pX4 <- c(X1.prob[1]*X2.prob, X1.prob[2]*X2.prob)
mX4 <- sample(1:4,size = 1000, replace = TRUE,prob = pX4)
X4 <- apply(X4.prob[,mX4],2,FUN=rmultinomvec)
#table(X4)/1000
##################        Genera X3        ####################
pX3 <- c(X1.prob[1]*X2.prob[1]*t.prob, 
         X1.prob[1]*X2.prob[2]*t.prob,
         X1.prob[2]*X2.prob[1]*t.prob, 
         X1.prob[2]*X2.prob[2]*t.prob)
mX3 <- sample(1:12, size = 1000, replace = TRUE,prob = pX3)
X3 <- apply(X3.prob[,mX3],2,FUN=rmultinomvec)
#table(X3)/1000
##################        Genera S         ####################
pX31 <- sum(X3.prob[1,]*pX3)
pX32 <- sum(X3.prob[2,]*pX3)
pX33 <- sum(X3.prob[3,]*pX3)
px3s <- c(pX31, pX32, pX33)
pX41 <- sum(X4.prob[1,]*pX4)
pX42 <- sum(X4.prob[2,]*pX4)
px4s <- c(pX41, pX42)
pS <-c(px4s*px3s[1],px4s*px3s[2],px4s*px3s[3])
mS <- sample(1:6, size = 1000, replace = TRUE,prob = pS)
S <- apply(S.prob[,mS],2,FUN=rmultinomvec)
##################       Genera Datos      ####################
X1<-as.factor(X1)
X2<-as.factor(X2)
X3<-as.factor(X3)
X4<-as.factor(X4)
t<-as.factor(t)
S<-as.factor(S)
red2<-data.frame(X1,X2,X3,X4,t,S)
##################       Guardar Datos      ####################
probs2<-list(PX1=X1.prob,PX2=X2.prob,PX3=X3.prob,PX4=X4.prob,
            PT=t.prob,Ps=S.prob)
save(red2,probs2,file="Red2.RData")
##################       Cargar  Datos      ####################
#load("Red2.RData")
#head(red2)
###############################################################
################        Red bayesiana 3      ##################
###############################################################

rm(list=ls(all=TRUE))
source("simula.R")
X1.prob <- c(0.5, 0.5) # Dos factores 1,2
X2.prob <- c(0.2,0.8) # Dos factores 1,2
X3.prob <- matprob(nr=3,nc=4) # Tres factores 1,2 y 3
X4.prob <- matprob(nr=3,nc=12) # Tres factores 1,2 y 3
X5.prob <- matprob(nr=4,nc=3) # Tres factores 1,2 y 3
##################        Genera X1        ####################
X1 <- rmultinom(n=1000,p=X1.prob)
#table(X1)/1000
p1<-X1.prob # P(X1=1) y P(X1=2)
##################        Genera X2        ####################
X2 <- rmultinom(n=1000,p=X2.prob)
#table(X2)/1000
p2<-X2.prob # P(X2=1) y P(X2=2)
##################        Genera X3        ####################
pX3 <- c(p1[1]*p2,p1[2]*p2) # proba de entrar a la columna 
# 1,2,3,4 donde: la columna 1 representa X1=1 y X2=1,...,
#                la columna 4 representa X1=2 y X2=2
mX3 <- sample(1:4,size = 1000, replace = TRUE,prob = pX3)
X3 <- apply(X3.prob[,mX3],2,FUN=rmultinomvec)
#table(X3)/1000
pX31 <- sum(pX3*X3.prob[1,]) 
pX32 <- sum(pX3*X3.prob[2,])
pX33 <- sum(pX3*X3.prob[3,])
p3 <- c(pX31,pX32,pX33) #P(X3=1), P(X3=2) y P(X3=3) sum (p3)
##################        Genera X4        ####################
pX4 <- c(p1[1]*p2[1]*p3,p1[1]*p2[2]*p3,
         p1[2]*p2[1]*p3,p1[2]*p2[2]*p3) # proba de entrar a la 
# columna 1,2,...,11,12 donde: 
# la columna 1 representa  X1=1, X2=1, X3=1,...,
# la columna 12 representa X1=2, X2=2, X3=3
mX4 <- sample(1:12,size = 1000, replace = TRUE,prob = pX4)
X4 <- apply(X4.prob[,mX4],2,FUN=rmultinomvec)
#table(X4)/1000
pX41 <- sum(pX4*X4.prob[1,]) 
pX42 <- sum(pX4*X4.prob[2,])
pX43 <- sum(pX4*X4.prob[3,])
p4 <- c(pX41,pX42,pX43) #P(X4=1), P(X4=2) y P(X4=3) sum(p4)
##################        Genera X5        ####################
pX5 <- p4 # proba de entrar a la columna 1,2, y 3 donde: 
# la columna 1 representa  X4=1,...
mX5 <- sample(1:3,size = 1000, replace = TRUE,prob = pX5)
X5 <- apply(X5.prob[,mX5],2,FUN=rmultinomvec)
#table(X5)/1000
pX51 <- sum(pX5*X5.prob[1,]) 
pX52 <- sum(pX5*X5.prob[2,])
pX53 <- sum(pX5*X5.prob[3,])
pX54 <- sum(pX5*X5.prob[4,])
p5 <- c(pX51,pX52,pX53,pX54) #P(X5=1), P(X5=2), P(X5=3) y P(X5=4)  
#sum(p5)
##################       Genera Datos      ####################
X1<-as.factor(X1)
X2<-as.factor(X2)
X3<-as.factor(X3)
X4<-as.factor(X4)
X5<-as.factor(X5)
red3<-data.frame(X1,X2,X3,X4,X5)
##################       Guardar Datos      ####################
probs3<-list(PX1=X1.prob,PX2=X2.prob,PX3=X3.prob,PX4=X4.prob,PX5=X5.prob)
save(red3,probs3,file="Red3.RData")
##################       Cargar  Datos      ####################
#load("Red3.RData")
#head(red3)
###############################################################
################        Red bayesiana 4      ##################
###############################################################

rm(list=ls(all=TRUE))
source("simula.R")
X1.prob <- c(0.30, 0.70) # Dos factores 1,2
X2.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X3.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X4.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X5.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X6.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X7.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X8.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X9.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X10.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X11.prob <- matprob(nr=2,nc=4) # Dos factores 1,2
X12.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X13.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X14.prob <- matprob(nr=2,nc=4) # Dos factores 1,2
X15.prob <- matprob(nr=2,nc=4) # Dos factores 1,2
X16.prob <- matprob(nr=2,nc=8) # Dos factores 1,2
X17.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X18.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X19.prob <- matprob(nr=2,nc=2) # Dos factores 1,2
X20.prob <- matprob(nr=2,nc=8) # Dos factores 1,2
##################        Genera X1        ####################
X1 <- rmultinom(n=1000,p=X1.prob)
#table(X1)/1000
##################        Genera X2        ####################
pX2 <- X1.prob #proba de entrar a la columna 1 y 2
mX2 <- sample(1:2,size = 1000, replace = TRUE,prob = pX2)
X2 <- apply(X2.prob[,mX2],2,FUN=rmultinomvec)
#table(X2)/1000
pX21 <- sum(pX2*X2.prob[1,]) 
pX22 <- sum(pX2*X2.prob[2,])
p2 <- c(pX21,pX22) #P(X2=1) y P(X2=2)
##################        Genera X3        ####################
pX3 <- p2 #proba de entrar a la columna 1 y 2
mX3 <- sample(1:2,size = 1000, replace = TRUE,prob = pX3)
X3 <- apply(X3.prob[,mX3],2,FUN=rmultinomvec)
#table(X3)/1000
pX31 <- sum(pX3*X3.prob[1,]) 
pX32 <- sum(pX3*X3.prob[2,])
p3 <- c(pX31,pX32) #P(X3=1) y P(X3=2)
##################        Genera X4        ####################
pX4 <- X1.prob # proba de entrar a la columna 1 y 2
mX4 <- sample(1:2,size = 1000, replace = TRUE,prob = pX4)
X4 <- apply(X4.prob[,mX4],2,FUN=rmultinomvec)
#table(X4)/1000
pX41 <- sum(pX4*X4.prob[1,]) 
pX42 <- sum(pX4*X4.prob[2,])
p4 <- c(pX41,pX42) #P(X4=1) y P(X4=2) 
##################        Genera X5        ####################
pX5 <- X1.prob # proba de entrar a la columna 1 y 2
mX5 <- sample(1:2,size = 1000, replace = TRUE,prob = pX5)
X5 <- apply(X5.prob[,mX5],2,FUN=rmultinomvec)
#table(X5)/1000
pX51 <- sum(pX5*X5.prob[1,]) 
pX52 <- sum(pX5*X5.prob[2,])
p5 <- c(pX51,pX52) #P(X5=1) y P(X5=2) 
##################        Genera X6        ####################
pX6 <- p2 # proba de entrar a la columna 1 y 2
mX6 <- sample(1:2,size = 1000, replace = TRUE,prob = pX6)
X6 <- apply(X6.prob[,mX6],2,FUN=rmultinomvec)
#table(X6)/1000
pX61 <- sum(pX6*X6.prob[1,]) 
pX62 <- sum(pX6*X6.prob[2,])
p6 <- c(pX61,pX62) #P(X6=1) y P(X6=2) 
##################        Genera X7        ####################
pX7 <- p3 # proba de entrar a la columna 1 y 2
mX7 <- sample(1:2,size = 1000, replace = TRUE,prob = pX7)
X7 <- apply(X7.prob[,mX7],2,FUN=rmultinomvec)
#table(X7)/1000
pX71 <- sum(pX7*X7.prob[1,]) 
pX72 <- sum(pX7*X7.prob[2,])
p7 <- c(pX71,pX72) #P(X7=1) y P(X7=2) sum(p7)
##################        Genera X8        ####################
pX8 <- X1.prob # proba de entrar a la columna 1 y 2
mX8 <- sample(1:2,size = 1000, replace = TRUE,prob = pX8)
X8 <- apply(X8.prob[,mX8],2,FUN=rmultinomvec)
#table(X8)/1000
pX81 <- sum(pX8*X8.prob[1,]) 
pX82 <- sum(pX8*X8.prob[2,])
p8 <- c(pX81,pX82) #P(X8=1) y P(X8=2) sum(p8)
##################        Genera X9        ####################
pX9 <- p5 # proba de entrar a la columna 1 y 2
mX9 <- sample(1:2,size = 1000, replace = TRUE,prob = pX9)
X9 <- apply(X9.prob[,mX9],2,FUN=rmultinomvec)
#table(X9)/1000
pX91 <- sum(pX9*X9.prob[1,]) 
pX92 <- sum(pX9*X9.prob[2,])
p9 <- c(pX91,pX92) #P(X9=1) y P(X9=2) sum(p9)
#################        Genera X10       ####################
pX10 <- p6 # proba de entrar a la columna 1 y 2
mX10 <- sample(1:2,size = 1000, replace = TRUE,prob = pX10)
X10 <- apply(X10.prob[,mX10],2,FUN=rmultinomvec)
#table(X10)/1000
pX101 <- sum(pX10*X10.prob[1,]) 
pX102 <- sum(pX10*X10.prob[2,])
p10 <- c(pX101,pX102) #P(X10=1) y P(X10=2) sum(p10)
#################        Genera X11       ####################
pX11 <- c(p5[1]*p7,p5[2]*p7) # proba de entrar a la columna 
# 1,2,3,4 donde: la columna 1 representa X5=1 y X7=1,...,
#                la columna 4 representa X5=2 y X7=2
mX11 <- sample(1:4,size = 1000, replace = TRUE,prob = pX11)
X11 <- apply(X11.prob[,mX11],2,FUN=rmultinomvec)
#table(X11)/1000
pX111 <- sum(pX11*X11.prob[1,]) 
pX112 <- sum(pX11*X11.prob[2,])
p11 <- c(pX111,pX112) #P(X11=1) y P(X11=2) sum(p11)
#################        Genera X12       ####################
pX12 <- p6 # proba de entrar a la columna 1 y 2
mX12 <- sample(1:2,size = 1000, replace = TRUE,prob = pX12)
X12 <- apply(X12.prob[,mX12],2,FUN=rmultinomvec)
#table(X12)/1000
pX121 <- sum(pX12*X12.prob[1,]) 
pX122 <- sum(pX12*X12.prob[2,])
p12 <- c(pX121,pX122) #P(X12=1) y P(X12=2) sum(p12)
#################        Genera X13       ####################
pX13 <- p7 # proba de entrar a la columna 1 y 2
mX13 <- sample(1:2,size = 1000, replace = TRUE,prob = pX13)
X13 <- apply(X13.prob[,mX12],2,FUN=rmultinomvec)
#table(X13)/1000
pX131 <- sum(pX13*X13.prob[1,]) 
pX132 <- sum(pX13*X13.prob[2,])
p13 <- c(pX131,pX132) #P(X13=1) y P(X13=2) sum(p13)
#################        Genera X14       ####################
pX14 <- c(p9[1]*p10,p9[2]*p10) # proba de entrar a la columna 
# 1,2,3,4 donde: la columna 1 representa X9=1 y X10=1,...,
#                la columna 4 representa X9=2 y X10=2
mX14 <- sample(1:4,size = 1000, replace = TRUE,prob = pX14)
X14 <- apply(X14.prob[,mX14],2,FUN=rmultinomvec)
#table(X14)/1000
pX141 <- sum(pX14*X14.prob[1,]) 
pX142 <- sum(pX14*X14.prob[2,])
p14 <- c(pX141,pX142) #P(X14=1) y P(X14=2) sum(p14)
#################        Genera X15       ####################
pX15 <- c(p11[1]*p14,p11[2]*p14) # proba de entrar a la columna 
# 1,2,3,4 donde: la columna 1 representa X11=1 y X14=1,...,
#                la columna 4 representa X11=2 y X14=2
mX15 <- sample(1:4,size = 1000, replace = TRUE,prob = pX15)
X15 <- apply(X15.prob[,mX15],2,FUN=rmultinomvec)
#table(X15)/1000
pX151 <- sum(pX15*X15.prob[1,]) 
pX152 <- sum(pX15*X15.prob[2,])
p15 <- c(pX151,pX152) #P(X15=1) y P(X15=2) sum(p15)
#################        Genera X16       ####################
pX16 <- c(p12[1]*p13[1]*p15, p12[1]*p13[2]*p15, 
          p12[2]*p13[1]*p15, p12[2]*p13[2]*p15) # proba de 
# entrar a la columna 1,2,...,7,8 donde: 
# la columna 1 representa X12=1, X13=1 y X15=1
# la columna 4 representa X12=1, X13=2 y X15=2
mX16 <- sample(1:8,size = 1000, replace = TRUE,prob = pX16)
X16 <- apply(X16.prob[,mX16],2,FUN=rmultinomvec)
#table(X16)/1000
pX161 <- sum(pX16*X16.prob[1,]) 
pX162 <- sum(pX16*X16.prob[2,])
p16 <- c(pX161,pX162) #P(X16=1) y P(X16=2) sum(p16)
#################        Genera X17       ####################
pX17 <- p14 # proba de entrar a la columna 1 y 2
mX17 <- sample(1:2,size = 1000, replace = TRUE,prob = pX17)
X17 <- apply(X17.prob[,mX17],2,FUN=rmultinomvec)
#table(X17)/1000
pX171 <- sum(pX17*X17.prob[1,]) 
pX172 <- sum(pX17*X17.prob[2,])
p17 <- c(pX171,pX172) #P(X17=1) y P(X17=2) sum(p17)
#################        Genera X18       ####################
pX18 <- p15 # proba de entrar a la columna 1 y 2
mX18 <- sample(1:2,size = 1000, replace = TRUE,prob = pX18)
X18 <- apply(X18.prob[,mX18],2,FUN=rmultinomvec)
#table(X18)/1000
pX181 <- sum(pX18*X18.prob[1,]) 
pX182 <- sum(pX18*X18.prob[2,])
p18 <- c(pX181,pX182) #P(X18=1) y P(X18=2) sum(p18)
#################        Genera X19       ####################
pX19 <- p16 # proba de entrar a la columna 1 y 2
mX19 <- sample(1:2,size = 1000, replace = TRUE,prob = pX19)
X19 <- apply(X19.prob[,mX19],2,FUN=rmultinomvec)
#table(X19)/1000
pX191 <- sum(pX19*X19.prob[1,]) 
pX192 <- sum(pX19*X19.prob[2,])
p19 <- c(pX191,pX192) #P(X19=1) y P(X19=2) sum(p19)
#################        Genera X20       ####################
pX20 <- c(p17[1]*p18[1]*p19, p17[1]*p18[2]*p19, 
          p17[2]*p18[1]*p19, p17[2]*p18[2]*p19) # proba de 
# entrar a la columna 1,2,...,7,8 donde: 
# la columna 1 representa X17=1, X18=1 y X19=1
# la columna 4 representa X17=1, X18=2 y X19=2
mX20 <- sample(1:8,size = 1000, replace = TRUE,prob = pX20)
X20 <- apply(X20.prob[,mX20],2,FUN=rmultinomvec)
#table(X20)/1000
pX201 <- sum(pX20*X20.prob[1,]) 
pX202 <- sum(pX20*X20.prob[2,])
p20 <- c(pX201,pX202) #P(X20=1) y P(X20=2) sum(p20)
##################       Genera Datos      ####################
X1<-as.factor(X1); X2<-as.factor(X2); X3<-as.factor(X3);
X4<-as.factor(X4); X5<-as.factor(X5); X6<-as.factor(X6);
X7<-as.factor(X7); X8<-as.factor(X8); X9<-as.factor(X9)
X10<-as.factor(X10); X11<-as.factor(X11); X12<-as.factor(X12)
X13<-as.factor(X13); X14<-as.factor(X14); X15<-as.factor(X15)
X16<-as.factor(X16); X17<-as.factor(X17); X18<-as.factor(X18)
X19<-as.factor(X19); X20<-as.factor(X20);

red4<-data.frame(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,
                 X11,X12,X13,X14,X15,X16,X17,X18,X19,X20)
##################       Guardar Datos      ####################
probs4<-list(PX1=X1.prob, PX2=X2.prob, PX3=X3.prob, PX4=X4.prob, PX5=X5.prob,
             PX6=X6.prob, PX7=X7.prob, PX8=X8.prob, PX9=X9.prob, PX10=X10.prob,
             PX11=X11.prob, PX12=X12.prob, PX13=X13.prob, PX14=X14.prob,
             PX15=X15.prob, PX16=X16.prob, PX17=X17.prob, PX18=X18.prob,
             PX19=X19.prob, PX20=X20.prob)
save(red4,probs4,file="Red4.RData")
##################       Cargar  Datos      ####################
#load("Red4.RData")
#head(red4)

#dag4<-as.bn("[1][2|1][3|2][4|1][5|1][6|2][7|3][8|1][9|5][10|6][11|5:7][12|6][13|7][14|9:10][15|11:14][16|12:13:15][17|14][18|15][19|16][20|17:18:19]")
#plot(dag4)

###############################################################
################        Red bayesiana 5      ##################
###############################################################

rm(list=ls(all=TRUE))
setwd("C:/Users/Gustavo/Dropbox/Gustavo")
source("Descomposicion.R")
p=runif(16,min=0,max = 1)
q=1-p
mp=cbind(p,q)
X1.prob <- mp[1,] # Dos factores 1,2
X2.prob <- mp[2,] # Dos factores 1,2
X3.prob <- mp[3,] # Dos factores 1,2
X4.prob <- mp[4,] # Dos factores 1,2
X5.prob <- mp[5,] # Dos factores 1,2
X6.prob <- mp[6,] # Dos factores 1,2
X7.prob <- mp[7,] # Dos factores 1,2
X8.prob <- mp[8,] # Dos factores 1,2
X9.prob <- mp[9,] # Dos factores 1,2
X10.prob <- mp[10,] # Dos factores 1,2
X11.prob <- mp[11,] # Dos factores 1,2
X12.prob <- mp[12,] # Dos factores 1,2
X13.prob <- mp[13,] # Dos factores 1,2
X14.prob <- mp[14,] # Dos factores 1,2
X15.prob <- mp[15,] # Dos factores 1,2
X16.prob <- mp[16,] # Dos factores 1,2

X17.prob <- matprob(nr=2,nc=2) # Dos factores 1,2 y tiene 1 padre
X18.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X19.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X20.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X21.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X22.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X23.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X24.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X25.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X26.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X27.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X28.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X29.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X30.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X31.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X32.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X33.prob <- matprob(nr=2,nc=2) # Dos factores 1,2 y tiene 1 padre
X34.prob <- matprob(nr=2,nc=2) # Dos factores 1,2 y tiene 1 padre

X35.prob <- matprob(nr=2,nc=2) # Dos factores 1,2 y tiene 1 padre
X36.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X37.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X38.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X39.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X40.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X41.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X42.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X43.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X44.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X45.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X46.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X47.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X48.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X49.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X50.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X51.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X52.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X53.prob <- matprob(nr=2,nc=2) # Dos factores 1,2 y tiene 1 padre
X54.prob <- matprob(nr=2,nc=2) # Dos factores 1,2 y tiene 1 padre

X55.prob <- matprob(nr=2,nc=2) # Dos factores 1,2 y tiene 1 padre
X56.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X57.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X58.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X59.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X60.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X61.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X62.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X63.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X64.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X65.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X66.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X67.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X68.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X69.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X70.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X71.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X72.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X73.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X74.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X75.prob <- matprob(nr=2,nc=2) # Dos factores 1,2 y tiene 1 padre
X76.prob <- matprob(nr=2,nc=2) # Dos factores 1,2 y tiene 1 padre

X77.prob <- matprob(nr=2,nc=2) # Dos factores 1,2 y tiene 1 padre
X78.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X79.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X80.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X81.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X82.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X83.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X84.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X85.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X86.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X87.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X88.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X89.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X90.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X91.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X92.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X93.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X94.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X95.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X96.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X97.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X98.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
X99.prob <- matprob(nr=2,nc=2) # Dos factores 1,2 y tiene 1 padre
X100.prob <- matprob(nr=2,nc=4) # Dos factores 1,2 y tiene 2 padres
##################        Genera X1-X16        ####################
X1 <- rmultinom(n=1000,p=X1.prob)
X2 <- rmultinom(n=1000,p=X2.prob)
X3 <- rmultinom(n=1000,p=X3.prob)
X4 <- rmultinom(n=1000,p=X4.prob)
X5 <- rmultinom(n=1000,p=X5.prob)
X6 <- rmultinom(n=1000,p=X6.prob)
X7 <- rmultinom(n=1000,p=X7.prob)
X8 <- rmultinom(n=1000,p=X8.prob)
X9 <- rmultinom(n=1000,p=X9.prob)
X10 <- rmultinom(n=1000,p=X10.prob)
X11 <- rmultinom(n=1000,p=X11.prob)
X12 <- rmultinom(n=1000,p=X12.prob)
X13 <- rmultinom(n=1000,p=X13.prob)
X14 <- rmultinom(n=1000,p=X14.prob)
X15 <- rmultinom(n=1000,p=X15.prob)
X16 <- rmultinom(n=1000,p=X16.prob)
##################        Genera X17-X34        ####################
f17=GeneraXi1p(Pp=X1.prob,Pci=X17.prob) # un solo padre 
X17=f17$X
p17=f17$P
rm(f17)
### 18
f18=GeneraXi2p(Pp1=X1.prob,Pp2=X2.prob,Pci=X18.prob) # dos padres
X18=f18$X
p18=f18$P
rm(f18)
### 19
f19=GeneraXi2p(Pp1=X2.prob,Pp2=X3.prob,Pci=X19.prob) # dos padres
X19=f19$X
p19=f19$P
rm(f19)
### 20
f20=GeneraXi2p(Pp1=X3.prob,Pp2=X4.prob,Pci=X20.prob) # dos padres
X20=f20$X
p20=f20$P
rm(f20)
### 21
f21=GeneraXi2p(Pp1=X4.prob,Pp2=X5.prob,Pci=X21.prob) # dos padres
X21=f21$X
p21=f21$P
rm(f21)
### 22
f22=GeneraXi2p(Pp1=X5.prob,Pp2=X6.prob,Pci=X22.prob) # dos padres
X22=f22$X
p22=f22$P
rm(f22)
### 23
f23=GeneraXi2p(Pp1=X6.prob,Pp2=X7.prob,Pci=X23.prob) # dos padres
X23=f23$X
p23=f23$P
rm(f23)
### 24
f24=GeneraXi2p(Pp1=X7.prob,Pp2=X8.prob,Pci=X24.prob) # dos padres
X24=f24$X
p24=f24$P
rm(f24)
### 25
f25=GeneraXi2p(Pp1=X8.prob,Pp2=X9.prob,Pci=X25.prob) # dos padres
X25=f25$X
p25=f25$P
rm(f25)
### 26
f26=GeneraXi2p(Pp1=X9.prob,Pp2=X10.prob,Pci=X26.prob) # dos padres
X26=f26$X
p26=f26$P
rm(f26)
### 27
f27=GeneraXi2p(Pp1=X10.prob,Pp2=X11.prob,Pci=X27.prob) # dos padres
X27=f27$X
p27=f27$P
rm(f27)
### 28
f28=GeneraXi2p(Pp1=X11.prob,Pp2=X12.prob,Pci=X28.prob) # dos padres
X28=f28$X
p28=f28$P
rm(f28)
### 29
f29=GeneraXi2p(Pp1=X12.prob,Pp2=X13.prob,Pci=X29.prob) # dos padres
X29=f29$X
p29=f29$P
rm(f29)
### 30
f30=GeneraXi2p(Pp1=X13.prob,Pp2=X14.prob,Pci=X30.prob) # dos padres
X30=f30$X
p30=f30$P
rm(f30)
### 31
f31=GeneraXi2p(Pp1=X14.prob,Pp2=X15.prob,Pci=X31.prob) # dos padres
X31=f31$X
p31=f31$P
rm(f31)
### 32
f32=GeneraXi2p(Pp1=X15.prob,Pp2=X16.prob,Pci=X32.prob) # dos padres
X32=f32$X
p32=f32$P
rm(f32)
### 33
f33=GeneraXi1p(Pp=X16.prob,Pci=X33.prob) # un solo padre 
X33=f33$X
p33=f33$P
rm(f33)
### 34
f34=GeneraXi1p(Pp=X16.prob,Pci=X34.prob) # un solo padre 
X34=f34$X
p34=f34$P
rm(f34)
##################        Genera X35-X54        ####################
### 35
f35=GeneraXi1p(Pp=p17,Pci=X35.prob) # un solo padre 
X35=f35$X
p35=f35$P
rm(f35)
### 36
f36=GeneraXi2p(Pp1=p17,Pp2=p18,Pci=X36.prob) # dos padres
X36=f36$X
p36=f36$P
rm(f36)
### 37
f37=GeneraXi2p(Pp1=p18,Pp2=p19,Pci=X37.prob) # dos padres
X37=f37$X
p37=f37$P
rm(f37)
### 38
f38=GeneraXi2p(Pp1=p19,Pp2=p20,Pci=X38.prob) # dos padres
X38=f38$X
p38=f38$P
rm(f38)
### 39
f39=GeneraXi2p(Pp1=p20,Pp2=p21,Pci=X39.prob) # dos padres
X39=f39$X
p39=f39$P
rm(f39)
### 40
f40=GeneraXi2p(Pp1=p21,Pp2=p22,Pci=X40.prob) # dos padres
X40=f40$X
p40=f40$P
rm(f40)
### 41
f41=GeneraXi2p(Pp1=p22,Pp2=p23,Pci=X41.prob) # dos padres
X41=f41$X
p41=f41$P
rm(f41)
### 42
f42=GeneraXi2p(Pp1=p23,Pp2=p24,Pci=X42.prob) # dos padres
X42=f42$X
p42=f42$P
rm(f42)
### 43
f43=GeneraXi2p(Pp1=p24,Pp2=p25,Pci=X43.prob) # dos padres
X43=f43$X
p43=f43$P
rm(f43)
### 44
f44=GeneraXi2p(Pp1=p25,Pp2=p26,Pci=X44.prob) # dos padres
X44=f44$X
p44=f44$P
rm(f44)
### 45
f45=GeneraXi2p(Pp1=p26,Pp2=p27,Pci=X45.prob) # dos padres
X45=f45$X
p45=f45$P
rm(f45)
### 46
f46=GeneraXi2p(Pp1=p27,Pp2=p28,Pci=X46.prob) # dos padres
X46=f46$X
p46=f46$P
rm(f46)
### 47
f47=GeneraXi2p(Pp1=p28,Pp2=p29,Pci=X47.prob) # dos padres
X47=f47$X
p47=f47$P
rm(f47)
### 48
f48=GeneraXi2p(Pp1=p29,Pp2=p30,Pci=X48.prob) # dos padres
X48=f48$X
p48=f48$P
rm(f48)
### 49
f49=GeneraXi2p(Pp1=p30,Pp2=p31,Pci=X49.prob) # dos padres
X49=f49$X
p49=f49$P
rm(f49)
### 50
f50=GeneraXi2p(Pp1=p31,Pp2=p32,Pci=X50.prob) # dos padres
X50=f50$X
p50=f50$P
rm(f50)
### 51
f51=GeneraXi2p(Pp1=p32,Pp2=p33,Pci=X51.prob) # dos padres
X51=f51$X
p51=f51$P
rm(f51)
### 52
f52=GeneraXi2p(Pp1=p33,Pp2=p34,Pci=X52.prob) # dos padres
X52=f52$X
p52=f52$P
rm(f52)
### 53
f53=GeneraXi1p(Pp=p34,Pci=X53.prob) # un solo padre 
X53=f53$X
p53=f53$P
rm(f53)
### 54
f54=GeneraXi1p(Pp=p34,Pci=X54.prob) # un solo padre 
X54=f54$X
p54=f54$P
rm(f54)
##################        Genera X55-X76        ####################
### 55
f55=GeneraXi1p(Pp=p35,Pci=X55.prob) # un solo padre 
X55=f55$X
p55=f55$P
rm(f55)
### 56
f56=GeneraXi2p(Pp1=p35,Pp2=p36,Pci=X56.prob) # dos padres
X56=f56$X
p56=f56$P
rm(f56)
### 57
f57=GeneraXi2p(Pp1=p36,Pp2=p37,Pci=X57.prob) # dos padres
X57=f57$X
p57=f57$P
rm(f57)
### 58
f58=GeneraXi2p(Pp1=p37,Pp2=p38,Pci=X58.prob) # dos padres
X58=f58$X
p58=f58$P
rm(f58)
### 59
f59=GeneraXi2p(Pp1=p38,Pp2=p39,Pci=X59.prob) # dos padres
X59=f59$X
p59=f59$P
rm(f59)
### 60
f60=GeneraXi2p(Pp1=p39,Pp2=p40,Pci=X60.prob) # dos padres
X60=f60$X
p60=f60$P
rm(f60)
### 61
f61=GeneraXi2p(Pp1=p40,Pp2=p41,Pci=X61.prob) # dos padres
X61=f61$X
p61=f61$P
rm(f61)
### 62
f62=GeneraXi2p(Pp1=p41,Pp2=p42,Pci=X62.prob) # dos padres
X62=f62$X
p62=f62$P
rm(f62)
### 63
f63=GeneraXi2p(Pp1=p42,Pp2=p43,Pci=X63.prob) # dos padres
X63=f63$X
p63=f63$P
rm(f63)
### 64
f64=GeneraXi2p(Pp1=p43,Pp2=p44,Pci=X64.prob) # dos padres
X64=f64$X
p64=f64$P
rm(f64)
### 65
f65=GeneraXi2p(Pp1=p44,Pp2=p45,Pci=X65.prob) # dos padres
X65=f65$X
p65=f65$P
rm(f65)
### 66
f66=GeneraXi2p(Pp1=p45,Pp2=p46,Pci=X66.prob) # dos padres
X66=f66$X
p66=f66$P
rm(f66)
### 67
f67=GeneraXi2p(Pp1=p46,Pp2=p47,Pci=X67.prob) # dos padres
X67=f67$X
p67=f67$P
rm(f67)
### 68
f68=GeneraXi2p(Pp1=p47,Pp2=p48,Pci=X68.prob) # dos padres
X68=f68$X
p68=f68$P
rm(f68)
### 69
f69=GeneraXi2p(Pp1=p48,Pp2=p49,Pci=X69.prob) # dos padres
X69=f69$X
p69=f69$P
rm(f69)
### 70
f70=GeneraXi2p(Pp1=p49,Pp2=p50,Pci=X70.prob) # dos padres
X70=f70$X
p70=f70$P
rm(f70)
### 71
f71=GeneraXi2p(Pp1=p50,Pp2=p51,Pci=X71.prob) # dos padres
X71=f71$X
p71=f71$P
rm(f71)
### 72
f72=GeneraXi2p(Pp1=p51,Pp2=p52,Pci=X72.prob) # dos padres
X72=f72$X
p72=f72$P
rm(f72)
### 73
f73=GeneraXi2p(Pp1=p52,Pp2=p53,Pci=X73.prob) # dos padres
X73=f73$X
p73=f73$P
rm(f73)
### 74
f74=GeneraXi2p(Pp1=p53,Pp2=p54,Pci=X74.prob) # dos padres
X74=f74$X
p74=f74$P
rm(f74)
### 75
f75=GeneraXi1p(Pp=p54,Pci=X75.prob) # un solo padre 
X75=f75$X
p75=f75$P
rm(f75)
### 76
f76=GeneraXi1p(Pp=p54,Pci=X76.prob) # un solo padre 
X76=f76$X
p76=f76$P
rm(f76)
##################        Genera X77-X100        ####################
f77=GeneraXi1p(Pp=p55,Pci=X77.prob) # un solo padre 
X77=f77$X
p77=f77$P
rm(f77)
### 78
f78=GeneraXi2p(Pp1=p55,Pp2=p56,Pci=X78.prob) # dos padres
X78=f78$X
p78=f78$P
rm(f78)
### 79
f79=GeneraXi2p(Pp1=p56,Pp2=p57,Pci=X79.prob) # dos padres
X79=f79$X
p79=f79$P
rm(f79)
### 80
f80=GeneraXi2p(Pp1=p57,Pp2=p58,Pci=X80.prob) # dos padres
X80=f80$X
p80=f80$P
rm(f80)
### 81
f81=GeneraXi2p(Pp1=p58,Pp2=p59,Pci=X81.prob) # dos padres
X81=f81$X
p81=f81$P
rm(f81)
### 82
f82=GeneraXi2p(Pp1=p59,Pp2=p60,Pci=X82.prob) # dos padres
X82=f82$X
p82=f82$P
rm(f82)
### 83
f83=GeneraXi2p(Pp1=p60,Pp2=p61,Pci=X83.prob) # dos padres
X83=f83$X
p83=f83$P
rm(f83)
### 84
f84=GeneraXi2p(Pp1=p61,Pp2=p62,Pci=X84.prob) # dos padres
X84=f84$X
p84=f84$P
rm(f84)
### 85
f85=GeneraXi2p(Pp1=p62,Pp2=p63,Pci=X85.prob) # dos padres
X85=f85$X
p85=f85$P
rm(f85)
### 86
f86=GeneraXi2p(Pp1=p63,Pp2=p64,Pci=X86.prob) # dos padres
X86=f86$X
p86=f86$P
rm(f86)
### 87
f87=GeneraXi2p(Pp1=p64,Pp2=p65,Pci=X87.prob) # dos padres
X87=f87$X
p87=f87$P
rm(f87)
### 88
f88=GeneraXi2p(Pp1=p65,Pp2=p66,Pci=X88.prob) # dos padres
X88=f88$X
p88=f88$P
rm(f88)
### 89
f89=GeneraXi2p(Pp1=p66,Pp2=p67,Pci=X89.prob) # dos padres
X89=f89$X
p89=f89$P
rm(f89)
### 90
f90=GeneraXi2p(Pp1=p67,Pp2=p68,Pci=X90.prob) # dos padres
X90=f90$X
p90=f90$P
rm(f90)
### 91
f91=GeneraXi2p(Pp1=p68,Pp2=p69,Pci=X91.prob) # dos padres
X91=f91$X
p91=f91$P
rm(f91)
### 92
f92=GeneraXi2p(Pp1=p69,Pp2=p70,Pci=X92.prob) # dos padres
X92=f92$X
p92=f92$P
rm(f92)
### 93
f93=GeneraXi2p(Pp1=p70,Pp2=p71,Pci=X93.prob) # dos padres
X93=f93$X
p93=f93$P
rm(f93)
### 94
f94=GeneraXi2p(Pp1=p71,Pp2=p72,Pci=X94.prob) # dos padres
X94=f94$X
p94=f94$P
rm(f94)
### 95
f95=GeneraXi2p(Pp1=p72,Pp2=p73,Pci=X95.prob) # dos padres
X95=f95$X
p95=f95$P
rm(f95)
### 96
f96=GeneraXi2p(Pp1=p73,Pp2=p74,Pci=X96.prob) # dos padres
X96=f96$X
p96=f96$P
rm(f96)
### 97
f97=GeneraXi2p(Pp1=p74,Pp2=p75,Pci=X97.prob) # dos padres
X97=f97$X
p97=f97$P
rm(f97)
### 98
f98=GeneraXi2p(Pp1=p75,Pp2=p76,Pci=X98.prob) # dos padres
X98=f98$X
p98=f98$P
rm(f98)
### 99
f99=GeneraXi1p(Pp=p76,Pci=X99.prob) # un solo padre 
X99=f99$X
p99=f99$P
rm(f99)
### 100
f100=GeneraXi2p(Pp1=p80,Pp2=p96,Pci=X100.prob) # dos padres
X100=f100$X
p100=f100$P
rm(f100)
##################       Genera Datos      ####################
X1<-as.factor(X1); X2<-as.factor(X2); X3<-as.factor(X3);
X4<-as.factor(X4); X5<-as.factor(X5); X6<-as.factor(X6);
X7<-as.factor(X7); X8<-as.factor(X8); X9<-as.factor(X9);
X10<-as.factor(X10); X11<-as.factor(X11); X12<-as.factor(X12);
X13<-as.factor(X13); X14<-as.factor(X14); X15<-as.factor(X15);
X16<-as.factor(X16); X17<-as.factor(X17); X18<-as.factor(X18);
X19<-as.factor(X19); X20<-as.factor(X20); X21<-as.factor(X21);
X22<-as.factor(X22); X23<-as.factor(X23); X24<-as.factor(X24);
X25<-as.factor(X25); X26<-as.factor(X26); X27<-as.factor(X27);
X28<-as.factor(X28); X29<-as.factor(X29); X30<-as.factor(X30);
X31<-as.factor(X31); X32<-as.factor(X32); X33<-as.factor(X33);
X34<-as.factor(X34); X35<-as.factor(X35); X36<-as.factor(X36);
X37<-as.factor(X37); X38<-as.factor(X38); X39<-as.factor(X39);
X40<-as.factor(X40); X41<-as.factor(X41); X42<-as.factor(X42);
X43<-as.factor(X43); X44<-as.factor(X44); X45<-as.factor(X45);
X46<-as.factor(X46); X47<-as.factor(X47); X48<-as.factor(X48);
X49<-as.factor(X49); X50<-as.factor(X50); X51<-as.factor(X51);
X52<-as.factor(X52); X53<-as.factor(X53); X54<-as.factor(X54);
X55<-as.factor(X55); X56<-as.factor(X56); X57<-as.factor(X57);
X58<-as.factor(X58); X59<-as.factor(X59); X60<-as.factor(X60);
X61<-as.factor(X61); X62<-as.factor(X62); X63<-as.factor(X63);
X64<-as.factor(X64); X65<-as.factor(X65); X66<-as.factor(X66);
X67<-as.factor(X67); X68<-as.factor(X68); X69<-as.factor(X69);
X70<-as.factor(X70); X71<-as.factor(X71); X72<-as.factor(X72);
X73<-as.factor(X73); X74<-as.factor(X74); X75<-as.factor(X75);
X76<-as.factor(X76); X77<-as.factor(X77); X78<-as.factor(X78);
X79<-as.factor(X79); X80<-as.factor(X80); X81<-as.factor(X81);
X82<-as.factor(X82); X83<-as.factor(X83); X84<-as.factor(X84); 
X85<-as.factor(X85); X86<-as.factor(X86); X87<-as.factor(X87);
X88<-as.factor(X88); X89<-as.factor(X89); X90<-as.factor(X90);
X91<-as.factor(X91); X92<-as.factor(X92); X93<-as.factor(X93);
X94<-as.factor(X94); X95<-as.factor(X95); X96<-as.factor(X96);
X97<-as.factor(X97); X98<-as.factor(X98); X99<-as.factor(X99);
X100<-as.factor(X100);

red5<-data.frame(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,
                 X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,
                 X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,
                 X61,X62,X63,X64,X65,X66,X67,X68,X69,X70,X71,X72,X73,X74,X75,X76,X77,X78,X79,X80,
                 X81,X82,X83,X84,X85,X86,X87,X88,X89,X90,X91,X92,X93,X94,X95,X96,X97,X98,X99,X100)
##################       Guardar Datos      ###################
probs5<-list(PX1=X1.prob, PX2=X2.prob, PX3=X3.prob, PX4=X4.prob, PX5=X5.prob, PX6=X6.prob, PX7=X7.prob, PX8=X8.prob, PX9=X9.prob, PX10=X10.prob,
             PX11=X11.prob, PX12=X12.prob, PX13=X13.prob, PX14=X14.prob, PX15=X15.prob, PX16=X16.prob, PX17=X17.prob, PX18=X18.prob, PX19=X19.prob, PX20=X20.prob,
             PX21=X21.prob, PX22=X22.prob, PX23=X23.prob, PX24=X24.prob, PX25=X25.prob, PX26=X26.prob, PX27=X27.prob, PX28=X28.prob, PX29=X29.prob, PX30=X30.prob,
             PX31=X31.prob, PX32=X32.prob, PX33=X33.prob, PX34=X34.prob, PX35=X35.prob, PX36=X36.prob, PX37=X37.prob, PX38=X38.prob, PX39=X39.prob, PX40=X40.prob,
             PX41=X41.prob, PX42=X42.prob, PX43=X43.prob, PX44=X44.prob, PX45=X45.prob, PX46=X46.prob, PX47=X47.prob, PX48=X48.prob, PX49=X49.prob, PX50=X50.prob,
             PX51=X51.prob, PX52=X52.prob, PX53=X53.prob, PX54=X54.prob, PX55=X55.prob, PX56=X56.prob, PX57=X57.prob, PX58=X58.prob, PX59=X59.prob, PX60=X60.prob,
             PX61=X61.prob, PX62=X62.prob, PX63=X63.prob, PX64=X64.prob, PX65=X65.prob, PX66=X66.prob, PX67=X67.prob, PX68=X68.prob, PX69=X69.prob, PX70=X70.prob,
             PX71=X71.prob, PX72=X72.prob, PX73=X73.prob, PX74=X74.prob, PX75=X75.prob, PX76=X76.prob, PX77=X77.prob, PX78=X78.prob, PX79=X79.prob, PX80=X80.prob,
             PX81=X81.prob, PX82=X82.prob, PX83=X83.prob, PX84=X84.prob, PX85=X85.prob, PX86=X86.prob, PX87=X87.prob, PX88=X88.prob, PX89=X89.prob, PX90=X90.prob,
             PX91=X91.prob, PX92=X92.prob, PX93=X93.prob, PX94=X94.prob, PX95=X95.prob, PX96=X96.prob, PX97=X97.prob, PX98=X98.prob, PX99=X99.prob, PX100=X100.prob)
save(red5,probs5,file="Red5.RData")
##################       Cargar  Datos      ####################
#load("Red5.RData")
#head(red5)

#dag4<-as.bn("[1][2|1][3|2][4|1][5|1][6|2][7|3][8|1][9|5][10|6][11|5:7][12|6][13|7][14|9:10][15|11:14][16|12:13:15][17|14][18|15][19|16][20|17:18:19]")
#plot(dag4)
length(probs5)
probs5$PX91
