rm(list=ls(all=TRUE))
source("Descomposicion.R")


Pobla<-Poblacion(10,6)
Pobla[[1]]
dd<-as.bn(Transforma(Pobla[[1]]))
score(dd,data=D,type="k2")

#preguntared(P,RB) # funcion pregunta si una rd esta en una pob

POB<-Poblacion(100,6) #poblacion (pi,b)
rbb<-list(POB[[1]][[1]],POB[[2]][[2]])
rbb<-list(c(5,6,4,2,3,1),
          c(1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0))
Transforma(rbb)
POBT<-POBES(POB) #poblacion [1:2]
POBT[[1]]
EV<-Eval(POBT,D)

pos<-sample(1:100, size =1, replace = FALSE)
xi<-POB[[pos]]
inds<-1:100

xit<-as.bn(Transforma(xi))
sub<-1:100
y<-list()
for (i in 1:90) {
  sub<-sub[-c(pos)]
  indices<-sample(sub, size =3, replace = FALSE)
  xr1<-POB[[indices[1]]]
  xr2<-POB[[indices[2]]]
  xr3<-POB[[indices[3]]]
  y[[i]]<-suma(xr1,ProductoEscalar(resta(xr2,xr3),0.7))
  pos<-c(pos,indices)
}


z<-list(Cruzapi(xi[[1]],y[[1]]),Cruzab(xi[[2]],y[[2]],0.1,indices[2]))
zt<-as.bn(Transforma(z))
sy<-score(zt,data = D,type = "k2")
sxi<-score(xit,data = D,type = "k2")
if (sy<=sxi) {
  res<-y
  
} else {
  res<-xi
}
pos<-c(pos,indices[4])


lapply(POBT, Regresa(POBT))
Regresa(POBT[[10]])

choose(500,2)
