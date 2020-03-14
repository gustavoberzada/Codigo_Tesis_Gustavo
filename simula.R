setwd("C:/Users/Gustavo/Dropbox/Gustavo")
#par(mar=c(0,0,0,0))
rm(list=ls(all=TRUE))
source("Descomposicion.R")
###############################################################
################        Ejemplo Dra        ####################
###############################################################
rmultinom<-function(n,p){
  #n simulaciones de una multinomial con m categorias 
  #donde la i-esima categoria ocurre con probabilidad p_i
  #p tiene que sumar 1 y tiene longitud m
  u<-runif(n)
  return(cut(u,cumsum(c(0,p)),label=FALSE))
}
rmultinomvec<-function(x){
  rmultinom(1,p=x)
}

#mprob<-matrix(NA,nrow=4,ncol=144)
#for(i in 1:144){
 #prob=c(0,sort(runif(3)),1)
 #prob<-prob[-1]-prob[-length(prob)]
 ##sum(prob)
 ##plot(prob)
 #mprob[,i]<-prob
#}
matprob<-function(nr,nc)
{ #Esta funcion genera una matriz de probabilidades aleatoria
  # de nr filas y nc columnas, nr es el numero de categorias
  # y la suma de cada columna es 1.
  mprob<-matrix(NA,nrow=nr,ncol=nc)
  for(i in 1:nc){
    prob=c(0,sort(runif(nr-1)),1)
    prob<-prob[-1]-prob[-length(prob)]
    #sum(prob)
    #plot(prob)
    mprob[,i]<-prob
  }
  return(mprob)
}

#nsim<-1000
#wsim<-sample(1:144,size=nsim,replace=TRUE)

#gg<-apply(mprob[,wsim],2,FUN=rmultinomvec)
###############################################################
################        Ejemplo libro      ####################
###############################################################

A.prob <- c(0.30, 0.50, 0.20)
S.prob <- c(0.60, 0.40)
E.prob <- matrix(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64,
                  0.36, 0.70, 0.30, 0.90, 0.10), ncol =6,nrow = 2)
O.prob <- matrix(c(0.96, 0.04, 0.92, 0.08), ncol = 2,nrow = 2)
R.prob <- matrix(c(0.25, 0.75, 0.20, 0.80), ncol = 2,nrow = 2)
T.prob <- matrix(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58,
                  0.24, 0.18, 0.70, 0.21, 0.09), nrow = 3,ncol = 4)
print(xtable(t(T.prob)), include.rownames = FALSE)

A<-rmultinom(n=5000,p=A.prob)
S<-rmultinom(n=5000,p=S.prob)

pAS<-c(A.prob*0.6, A.prob*0.4) #proba de entrar a la columna 1,...,6
mAS<-sample(1:6,size = 5000, replace = TRUE,prob = pAS)
#table(mAS)/5000
E<-apply(E.prob[,mAS],2,FUN=rmultinomvec)

ph<-sum(E.prob[1,]*pAS) 
pu<-sum(E.prob[2,]*pAS)
pER<-c(ph,pu) # P(E=1) y P(E=2)
mER<-sample(1:2,size = 5000, replace = TRUE,prob = pER)
#table(mAS)/5000
R<-apply(R.prob[,mER],2,FUN=rmultinomvec)

mEO<-sample(1:2,size = 5000, replace = TRUE,prob = pER)
O<-apply(O.prob[,mEO],2,FUN=rmultinomvec)

pemp<-sum(O.prob[1,]*pER)
pself<-sum(O.prob[2,]*pER)

psmall<-sum(R.prob[1,]*pER)
pbig<-sum(R.prob[2,]*pER)

pse<-psmall*pemp
pss<-psmall*pself
pbe<-pbig*pemp
pbs<-pbig*pself

pOR<-c(pse,pss,pbe,pbs)
mOR<-sample(1:4,size = 5000, replace = TRUE,prob = pOR)
T1<-apply(T.prob[,mOR],2,FUN=rmultinomvec)
PT=c(T.prob[1,]*pOR,T.prob[2,]*pOR,T.prob[3,]*pOR)

A<-as.factor(A)
S<-as.factor(S)
E<-as.factor(E)
R<-as.factor(R)
O<-as.factor(O)
T1<-as.factor(T1)
datossim<-data.frame(A,S,E,R,O,T1)
head(datossim)
probs<-list(PA=A.prob,PS=S.prob,PE=E.prob,PR=R.prob,
             PO=O.prob,PT=T.prob)
#save(datossim,probs,file="Ejemplo.RData")
###############################################################
###########        Estimacion de parametros    ################
###############################################################
dag<-as.bn("[A][S][E|A:S][O|E][R|E][T1|O:R]")
bn.mle <- bn.fit(dag, data = datossim, method = "mle")
#bn.bayes <- bn.fit(dag, data = datossim, method = "bayes")
###############################################################
#bn.mle$A$prob
#bn.mle$S$prob
#bn.mle$E$prob
#dag<-rsmax2(datossim) #gs aracne
#plot(dag)
#d<-bn.boot(data=datossim, R = 200, m=1000,statistic = arcs)

###############################################################
###########        Valor esperado vertices     ################
###############################################################
i2=c(1,2)
i3=c(1,2,3)
i4=c(1,2,3,4)
i5=c(1,2,3,4,5)
EA=sum(i3*A.prob)
ES=sum(i2*S.prob)
EE=sum(i2*pER)
ER=sum(c(psmall,pbig)*i2)
EO=sum(c(pemp,pself)*i2)
ET=sum(PT*i3)
ValorEsperado=list(A=EA,S=ES,E=EE,R=ER,O=EO,T1=ET)

###############################################################
###########    Valor esperado estimado vertices     ###########
###############################################################
rm(list=ls(all=TRUE))
library(bnlearn)
load("Ejemplo.RData")
head(datossim)
dag<-as.bn("[A][S][E|A:S][O|E][R|E][T1|O:R]")
bn.mle <- bn.fit(dag, data = datossim, method = "mle")
EEA=sum(bn.mle$A$prob*i3)# sum((table(datossim$A)/5000)*i3)
EES=sum(bn.mle$S$prob*i2)# sum((table(datossim$S)/5000)*i2)
PcEE=cbind(bn.mle$E$prob[,,1],bn.mle$E$prob[,,2]) # proba condicional estimada de E
pase=c(bn.mle$A$prob*bn.mle$S$prob[1],bn.mle$A$prob*bn.mle$S$prob[2]) # proba conjunta A y S
PEE=c(sum(PcEE[1,]*pase),sum(PcEE[2,]*pase))
EEE=sum(PEE*i2) # sum((table(datossim$E)/5000)*i2)
PRE=c(sum(PEE*bn.mle$R$prob[1,]),sum(PEE*bn.mle$R$prob[2,]))
EER=sum(PRE*i2)
POE=c(sum(PEE*bn.mle$O$prob[1,]),sum(PEE*bn.mle$O$prob[2,]))
EEO=sum(POE*i2)
PORE=c(PRE[1]*POE,PRE[2]*POE)
PcTE=cbind(bn.mle$T1$prob[,,1],bn.mle$T1$prob[,,2])
PTE=c(sum(PcTE[1,]*PORE),sum(PcTE[2,]*PORE),sum(PcTE[3,]*PORE))
EET=sum(PTE*i3)
ValorEsperadoEst=list(A=EEA,S=EES,E=EEE,R=EER,O=EEO,T1=EET)
#load("Red1.RData")
#red1
#probs1
###############################################################
#########   Valor esperado estimado vertices  red 1   #########
###############################################################
load("DEBNk21.RData")
load("Red1.RData")
colnames(red1)<-c("1","2","3","4","5","6")
source("Descomposicion.R")
dagDEBNk21<-as.bn(Transforma(RES1$opdag))
plot(dagDEBNk21)
mle.DEBNk21 <- bn.fit(dagDEBNk21, data = red1, method = "mle")
EV1=sum(probs1$PX1*IdentidadP(2)) # Valor esperado de V1
EEV1=sum(mle.DEBNk21$"1"$prob*IdentidadP(2)) # Valor esperado estimado de V1
E



