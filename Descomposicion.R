###############################################################
###############       Operaciones      ########################
###############################################################


library(bnlearn) # paq de redes bayesianas
library(gtools) # combinations
library(plyr) # count cuenta
library(graph)
library(igraph)

suma<-function(a1,a2)
{ #suma RB en una lista de la forma (pi,b)
  sa1<-a1[[1]]
  sa2<-a2[[1]]
  sres<-sa1[sa2]
  ba1<-a1[[2]]
  ba2<-a2[[2]]
  bres<-xor(ba1,ba2)*1
  return(res=list(sres,bres))
}

resta<-function(a1,a2)
{ #resta RB en una lista de la forma (pi,b)
  #browser()
  sa1<-a1[[1]]
  sa2<-a2[[1]]
  tsa2<-rbind(seq(1,length(sa1)), sa2)
  tsa2<-tsa2[,order(tsa2[2,])]
  isa2<-tsa2[1,]
  sres<-isa2[sa1]
  ba1<-a1[[2]]
  ba2<-a2[[2]]
  bres<-xor(ba1,ba2)*1
  if (length(bres)==0) bres<-Identidadb(length(sa1))
  return(res=list(sres,bres))
}

pegabasesig<-function(ve,nv)
{ #esta funcion pega (per,0)
  #ve = a un vector de sigmas y nv= numero de vertices
  n<-length(ve)
  IP<-IdentidadP(nv)
  IB<-Identidadb(nv)
  res<-list()
  for (i in 1:n) {
    res[[i]]<-list(as.vector(sigma(IP,ve[i])),IB)
  }
  return(res) 
}

pegabasebit<-function(ve,nv)
{ #esta funcion pega (iota,b)
  #ve = a un vector de bits y nv= numero de vertices
  n<-length(ve)
  IP<-IdentidadP(nv)
  resbz<-list()
  for (i in 1:n) {
    IB<-Identidadb(nv)
    IB[ve[i]]<-1
    resbz[[i]]<-list(IP,IB)
  }
  return(resbz) 
}

#PerteneceVF(c(1,2,3),1)
#PerteneceVF

sigma<-function(x,y)
{ #Esta funcion verifica que el ultimo renglon puede ser 
  # expresado como la composicion de sigmas en y
  #Ejemplo:
  #permutaciones<-sigma(x=c(1,2,3,4,5),y=c(2,3,1,2,4,3))
  #(3,4,5,1,2)=y=<sigma2?sigma3?sigma1?sigma2?sigma4?sigma3?e>
  n<-length(x)
  m<-length(y)
  M<-matrix(0,nrow =m, ncol = n )
  for (i in 1:m) {
    pos<-y[i]
    aux<-x[pos]
    x[pos]<-x[pos+1]
    x[pos+1]<-aux
    M[i,]<-x
  }
  return(M)
}

conjuntoZ<-function(per)
{ #Esta funcion calcula el conjunto Z, 
  #donde Z={i:pi(i)>pi(i+1)}
  #conjuntoZ(c(2,1,3,4))
  n<-length(per)
  iota=seq(1:n)
  mdif<-max(abs(per-iota))
  if (mdif==0){Z=0; return(Z)}
  else {
    x<-per[1:n-1]
    y<-per[2:n]
    Z<-which(x>y)
    return(Z)
  }
}

Elimina<-function(vec,j)
{ #Esta funcion funcion elimina el elemento j de un conjunto
  # el conjunto es visto como vector
  res<-vec[-j]
  m<-length(res)
  if(m==0){res<-0}
  return(res)
}

Pertenece<-function(v,elem) 
{ #esta funcion regresa un 1 si un elemento pertenece al 
  #conjunto, donde el conjunto es un vector
  #Pertenece(c(1,2),0)
  n<-length(v)
  res<-rep(0,n)
  for (i in 1:n) {
    res[i]<-(v[i]==elem)*1
  }
  return(max(res))
}

PerteneceVF<-function(v,elem) 
{ #esta funcion regresa un True si un elemento esta en un vec
  n<-length(v)
  res<-rep(0,n)
  for (i in 1:n) {
    res[i]<-(v[i]==elem)*1
  }
  return(as.logical(max(res)))
}
#RANDBS(c(1,3,2,4))
RANDBS<-function(p)
{ #esta funcion da una descomposici?n minima de una permutaci?n
  #Ejemplo:
  #xx<-RANDBS(c(2,4,3,1))
  #sigma(1:4,xx)
  #p<-c(1,2,3,4)
  s<-vector()
  n<-length(p)
  idp<-IdentidadP(n)
  valor<-min((idp==p)*1)
  if (valor==1) return(0)
  Z<-conjuntoZ(p)
  while(length(Z)>0)
  {
    m<-length(Z)
    j<-sample(1:m,1)#correr mas
    i<-Z[j]
    Z<-Z[-j]
    p<-sigma(x=p,y=c(i))[1,]
    s<-c(s,c(i))
    pi_1<-Pertenece(v=Z,elem = i-1)
    pi_1[is.na(pi_1)]<-0
    C1<-i>0
    C2<-pi_1==0
    C3<-p[i-1]>p[i]
    C3[is.logical(0)]<-FALSE
    C3[is.na(C3)]<-FALSE
    if ((C1)&&(C2)&&(C3)){Z<-c(Z,i-1)}
    pim1<-Pertenece(v=Z,elem = i+1)
    pim1[is.na(pim1)]<-0
    C4<-i<n-1
    C5<-pim1==0
    C6<-p[i+1]>p[i+2]
    C6[is.logical(0)]<-FALSE
    if ((C4)&&(C5)&&(C6)){Z<-c(Z,i+1)}
    Z<-unique(Z)
  }
  return(rev(s)) 
}
#verificacion
#P<-RANDBS(c(3,4,5,1,2))
#P
#descomposicion=sigma(x=c(1,2,3,4,5),y=P)
#descomposicion
#permutaciones<-sigma(x=c(1,2,3,4,5),y=c(2,3,1,2,4,3))
#(3,4,5,1,2)=y=<sigma2?sigma3?sigma1?sigma2?sigma4?sigma3?e>

descminb<-function(b)
{ #Esta funcion calcula la descomposicion minima de un bit
  #descminb(c(1,0,0,1,1,1))
  pos<-which(b==1)
  if (length(pos)==0) {return(b)}
  m<-length(b)
  n<-length(pos)
  M<-matrix(0,nrow =n, ncol = m)
  nombre<-vector() #estas lineas asignan nombre a col
  for (i in 1:m) {
    nombre[i]<-paste("b",i)
  }
  
  colnames(M)<-nombre
  
  for (i in 1:n) {
    y<-rep(0, m)
    y[pos[i]]<-1
    M[i,]<-y
  }
  return(list(nombre[pos],pos,M))
  #return(M)
}
#b<-c(0,0,0,0,0,0)
#descminb(b)
#descminb(b)[[3]]
#dim(N)[2]


oexclusivo<-function(M)
{ #Verificacion cuando la desc minima de b es una matriz
  m<-dim(M)[2]
  n<-dim(M)[1]
  res<-rep(0,m)
  for (i in 1:n) {
    res<-xor(M[i,],res)*1
    
  }
  return(res)
}

###############################################################
###########    Descompocion minima RB de A   ##################
###############################################################

DMA<-function(RB)
{ #un algoritmo de descomposici?n aleatoria para A
  #el argumento es una red bayesiana en forma de lista
  #p<-c(2,4,3,1)
  #b<-c(0,1,1,1,0,1)
  #RB<-r
  #RB<-list(c(1,2,3,4),c(0,0,0,0,0,0))
  n<-length(RB[[1]])
  m<-choose(n,2)
  p<-RB[[1]]
  b<-RB[[2]]
  #sigma(1:4,c(1,2,3,2))
  DescPI<-RANDBS(p)
  DescB<-descminb(b)[[1]]
  DESCB<-descminb(b)[[2]]
  if ((DescPI==0)&&(DESCB==0)){return(0)}
  if ((DescPI==0)&&(DESCB!=0)){return(DescB)}
  if ((DescPI!=0)&&(DESCB==0)){return(DescPI)}
  L<-length(DescPI)
  M<-length(DescB)
  r<-rep(0,L+M)
  S<-L+M
  indices<-sort(sample(1:S,size = L))
  r[indices]<-DescPI
  r[-indices]<-DescB
  complem<-1:S
  li<-list(r,DescPI,DESCB,indices,complem[-indices])
  return(li)
}

IniciaRB<-function(nv)
{# Da una red bayesiana x=(pi,b) 
  m<-choose(nv,2)
  x<-1:nv
  p<-sample(x,size = nv,replace = FALSE,prob = rep(1/nv,nv))
  b<-rbinom(n=m, size=1, prob=2/(nv-1))
  x1<-list(p,b)
  return(x1)
}

Iinversa<-function(per,x)
{ #Calcula la Imagen Inversa de una permutacion 
  # Ejemplo: 
  #x<-c(1,1)  
  #per<-c(3,1,4,2,5)
  #Iinversa(per,x)  
  z<-length(x)
  n<-length(per)
  I<-1:n
  P<-rbind(I,per)
  imi<-rep(0,z)
  for (i in 1:z) {
    imi[i]<-which(P["per",]==x[i])
  }
  return(imi)
}

#combinations(4,2,1:4)
conjuntoC<-function(redb)
{
  #Esta funcion calcula el cojunto C={(i,j):1<=i<j<=n}, la
  # imagen inversa de los nodos, los padres, los hijos 
  # y el num de vertices. El argumento es una red bayesiana 
  # en forma de lista. Su salida es una lista con dos elementos
  # una matriz y un numero(num de vertices).
  # Ejemplo: 
  #redb<-list(c(3,1,4,2,5),c(0,1,1,0,1,1,1,1,1,1))
  #conjuntoC(redb)
  per<-redb[[1]]
  b<-redb[[2]]
  n<-length(per)
  m<-length(b)
  I<-1:n
  pos<-which(b==1)
  v<-1:m 
  combinaciones <- combinations(n, 2, v)
  C<-combinaciones[pos,] 
  if(length(C)==2) C<-matrix(C,ncol=2)
  colnames(C)<-c("i","j")
  M<-C
  for (i in 1:dim(M)[1]) {
   M[i,]<-Iinversa(per,C[i,])
  }
  colnames(M)<-c("I.","Inv.")
  H<-M
  for (i in 1:dim(H)[1]) {
    if (M[i,1]<M[i,2]){
      H[i,]=C[i,]
    }else {
        H[i,]=rev(C[i,])
        }
  }
  colnames(H)<-c("Padre","Hijo")
  R<-cbind(C,M,H)
  return(list(R,n))
}
#rb<-list(c(3,1,4,2),c(0,1,1,0,1,1))
#conjuntoC(rb) 
Padres<-function(rb)
{ #Esta funcion obtiene los padres de una red bayesiana en la 
  # forma de lista (pi,b). Su salida es una lista con 
  # los padres del elemento [[i]] de la lista. Asigna un "No"
  # a los vertices que no tienen padres.
  #Ejemplo rb<-list(c(1,3,4,2),c(1,0,1,0,1,1))
  # Padres(rb)
  PH<-conjuntoC(rb)
  PH56<-conjuntoC(rb)[[1]][,5:6]
  if (length(PH56)==2)PH56<-matrix(PH56,ncol = 2)
  padres<-list()
  for (i in 1:PH[[2]]) {
    poshijos<-which(PH56[,2]==i)
    p<-length(poshijos)
    if (p==0) {
      padres[[i]]<-"No"
    } else {
      padres[[i]]<-PH56[,1][poshijos]
    }
  }
  return(padres)
}

hijoscadena<-function(v)
{ #Pega los hijos de un vertice y los separa por el caracter
  # ":" (1:2:3) con el fin de 
  # llegar a la estructura "[3][1|2:3][2|3][4|1:2:3]"
  nh<-length(v)
  res<-character()
  for (i in 1:nh) {
    res<-paste0(res,v[i],":") 
  }
  hijos<-gsub('.{1}$', '', res)
  return(hijos)
  
}

hijos<-function(Y1)
{ #Esta funcion busca hijos de una red bayesiana y 
  # los pega entre corchetes. El argumento es Y1 es una lista
  #
  ver<-1:length(Y1)
  nf<-which(Y1=="No")
  posqtpa<-ver[-nf]
  cad<-character()
  for (i in posqtpa) {
    res<-paste0("[",i,"|",hijoscadena(Y1[[i]]),"]")
    cad<-paste0(cad,res)
  }
  return(cad)
}

Transforma<-function(rba)
{ #Transforma la red bayesina a [H|P]
  # el argumento de funcion es una red bayesiana en forma
  # de lista (pi,b). Ejemplo:
  #rba<-list(c(3,2,1,4),c(0,1,1,1,1,1))
  #Transforma(rba)
  con_1<-conjuntoC(rba)[[1]][,5:6]
  con_2<-conjuntoC(rba)[[2]]
  Y<-Padres(rba)
  nf<-which(Y=="No")
  chijos<-hijos(Y)
  huer<-character()
  for (i in 1:length(nf)) {
    huer<-paste0(huer,"[",nf[i],"]")
  }
  resultado<-paste0(huer,chijos)
  return(resultado)
}

Plot<-function(rbl)
{ #plot de una red bayesiana
  dag = model2network(Transforma(rbl))
  plot(dag)
}
####################################################
Poblacion<-function(N1,v)
{ #Genera una pobacion de redesbayesianas de tama?o
  # N1 y numero de vertices v
  m<-choose(v,2)
  res<-list()
  for (i in 1:N1) {
    res[[i]]<-IniciaRB(v)
  }
  return(res)
}

IdentidadR<-function(n)
{ #Funcion que calcula la identidad de una red bayesiana
  # de la forma (pi,b) donde n es el numero de vertices
  per<-1:n
  m<-choose(n,2)
  b<-rep(0,m)
  rb<-list(per,b)
  return(rb)
}

IdentidadP<-function(n)
{ #Esta funcion calcula la identidad de una permutacion
  In<-IdentidadR(n)
  Ip<-In[[1]]
  return(Ip)
}

Identidadb<-function(n)
{ #Esta funcion calcula la identidad de un bit
  Inb<-IdentidadR(n)
  Ib<-Inb[[2]]
  return(Ib)
}

fb<-function(n,cv)
{  #Esta funcion asigana un 1 en la posicion n, cv=longitud b
  res<-rep(0,cv)
  res[n]<-1
  return(res)
}

fsigma<-function(n,v)
{ #esta funcion calcula la permutacion sigma_i 
  # es decir permuta el i-esimo elemento con el i+1
  res<-1:v
  temp<-res[n]
  res[n]<-res[n+1]
  res[n+1]<-temp
  return(res)
}

getL<-function(RB)
{ #esta funcion obtiene la descomposicion minima de la forma
  # (sigma,0)*(iota,bi)
  #RB<-list(c(1,4,3,2),c(1,0,0,1,0,0))
  v<-length(RB[[1]])
  cv<-choose(v,2)
  DescA<-DMA(RB)
  if (is.list(DescA)){
    r<-cbind(DescA[[4]],DescA[[2]],1)
    r<-rbind(r,cbind(DescA[[5]],DescA[[3]],0))
    r<-r[order(r[,1]),]
    M<-length(DescA[[1]])
    L<-as.list(rep(NA,M))
    for (i in 1:M) {
      L[[i]]<-list(1:v,rep(0,cv))
      if (r[i,3]==1) L[[i]][[1]]<-fsigma(r[i,2],v)
      else  L[[i]][[2]]<-fb(r[i,2],cv)
    }
    return(L)
  }
  if (min(DescA)==0){return(IdentidadR(v))}
  if (is.character(DescA)){
    veve<-as.numeric(gsub("b","",DescA))
    re1<-pegabasebit(veve,v)
    return(re1)
  }
  if (is.numeric(DescA)){
    veve2<-pegabasesig(DescA,v)
    return(veve2)
  }
  
}

ProductoEscalar<-function(rb,F1)
{ #Esta funcion hace el producto escalar de F*RB
  #RB<-list(c(1,2,3,4),c(0,0,0,0,0,0))
  #RB<-list(c(1,3,2,4),c(1,1,0,0,0,0))
  #rb<-list(c(4,3,2,1),c(1,1,1,0,1,0))
  #F1<-0.3
  n<-length(rb[[1]]) #numero de vertices
  con1<-min((rb[[1]]==IdentidadP(n))*1)
  con2<-min((rb[[2]]==Identidadb(n))*1)
  if ((con1==1)&&(con2==1)) return(rb)
  GG<-getL(rb)
  lon<-length(GG)    #numero de redes bayesianas 
  k<-ceiling(F1*lon)
  res<-IdentidadR(n)
  for (i in k:1) {
    res<-suma(GG[[i]],res)
  }
  return(res)
}

preguntared<-function(P,RB)
{ #esta funcion regresa 1 si RB esta en la pobacion P
  N<-length(P)
  for (i in 1:N) {
    c1<-min((P[[i]][[1]]==RB[[1]])*1)
    c2<-min((P[[i]][[2]]==RB[[2]])*1)
    if (c1&c2) {
      res<-1
    } else {
      res<-0
    }
  }
  return(res)
}

Cruzapi<-function(p1,p2)
{ #Esta funcion hace el cruce ciclico CYC para 
  #las permutaciones
  n<-length(p1)
  r<-rep(0,n)
  
  while(length(which(r==0))>0){
    indice1<-which(r==0)
    indice10<-indice1[1]
    pos<-indice10 #
    r[indice10]<-p1[indice10]
    
    for (i in 1:n) {
      r[which(p2==r[pos])]<-p1[which(p2==r[pos])]
      pos<-which(p2==r[pos])
    }
    
    indice2<-which(r==0)    
    indice20<-indice2[1]
    pos2<-indice20 #
    r[indice20]<-p2[indice20]
    
    for (i in 1:length(indice2)) {
      r[which(p1==r[pos2])]<-p2[which(p1==r[pos2])]
      pos2<-which(p1==r[pos2])
    }
    
  }
  return(r)
}

Cruzab<-function(b1,b2)
{ #esta funcion realiza la cruza de dos bits
  #el segundo bit corresponde al mutante y_i
  #Ejemplo:
  #b1<-c(1,0,1,0,1,1)
  #b2<-c(1,1,0,0,1,0)
  CR<-runif(1,min = 0,max = 1)
  m<-length(b1)
  z<-rep(0,m)
  r<-runif(m)
  r2<-sample(1:m,size = 1,replace = FALSE,prob = rep(1/m,m))
  for (j in 1:m) {
    if ((r[j]<=CR)|(r2==j)) {
      z[j]<-b2[j]
    } else {
      z[j]<-b1[j]
    }
  }
  return(z)
}

POBES<-function(BNE)
{ #Esta funcion transforma la poblacion de redes bayesianas de
  # la forma (pi,b) a una poblacion de la forma 
  # "[3][1|2:3][2|3][4|1:2:3]"
  n<-length(BNE)
  ch<-list(rep(NA,n))
  for (i in 1:n) {
    ch[[i]]<-as.bn(Transforma(BNE[[i]]))
  }
  return(ch)
}

Eval<-function(lirb,datos, sctype)
{ #Evalua una lista de rb en formato as.bn
  n<-length(lirb)
  res<-rep(0,n)
  for (i in 1:n) {
    res[i]<-score(lirb[[i]],data = datos, type = sctype)
  }
  return(res)
}

SH<-function(vec,ivec)
{ #Esta funcion obtiene los vertices que no tienen hijos
  n<-length(ivec)
  res<-vector()
  for (i in 1:n) {
    res[i]<-Pertenece(vec,i)
  }
  pos<-which(res==0)
  if (length(pos)==0) {
    pos<-0
    return(0)
  } else {
    return(pos)
  }
}

pvector<-function(ve1,ve2)
{ #Esta funcion regunta si dos vectores son iguales
  res<-min((ve1==ve2)*1)
  res<-as.logical(res)
  return(res)
}

destransformap<-function(rbnmodelo)
{
  #Esta funcion obtiene una posible permutacion de la red
  # bayesiana en formato "[2][3][1|3][4|1:3][5|1:2:3:4][6|1:4]"
  Mat<-matrix(as.numeric(rbnmodelo$arcs),ncol = 2,
              nrow = length(as.numeric(rbnmodelo$arcs))/2)
  #Mat matriz de padres e hijos
  n1<-length(rbnmodelo$nodes)
  pp<-IdentidadP(n1)
  cuenta<-count(Mat[,2],1) #cuenta el no de hijos
  orden<-cuenta[order(cuenta[,2]),] #ordena los hijos de menor
  # a mayor frecuencia.
  sinhij<-SH(orden[,1],1:n1) #busca los nodos sin hijos
  conhijos<-orden[,1]
  per<-c(sinhij,conhijos)
  resu<-vector()
  if (length(per)>n1) {
    resu<-per[2:n1]
    return(resu)
  } else {
    return(per)
  }
}

ordenaconjc<-function(Ma)
{ #Esta funcion ordena el conjunto de arcos para llegar al 
  # conjuto C={(i,j):1<=i<j<=n}
  #Ma<-Mat
  n<-dim(Ma)[1]
  m<-dim(Ma)[2]
  resul<-matrix(data = NA,nrow = n,ncol = m)
  for (i in 1:n) {
    resul[i,]=sort(Ma[i,])
  }
  return(resul)
}

destransformab<-function(rbnmodelo2)
{ #Esta funcion obtiene el vector de bits b, el argumento
  # es un objeto rb del paquete 
  n2<-length(rbnmodelo2$nodes)
  m<-choose(n2,2)
  Mat<-matrix(as.numeric(rbnmodelo2$arcs),ncol = 2,
              nrow = length(as.numeric(rbnmodelo2$arcs))/2)
  Mat<-ordenaconjc(Mat)
  arcos<-Mat[order(Mat[,1]),]
  Conc<-combinations(n2, 2, 1:m)
  res<-rep(0,m)
  nf<-dim(arcos)[1]
  for (j in 1:nf) {
    for (i in 1:m) {
      if (pvector(Conc[i,],arcos[j,])) res[i]<-1
    }
  }
  return(res)
}

Regresa<-function(bnred)
{ #Esta funcion regresa la red bayesiana de la forma
  # (pi,b) en una lista.
  per<-destransformap(bnred)
  b<-destransformab(bnred)
  res<-list(per,b)
  return(res)
}

Regresalista<-function(lisbnred)
{ #Esta funcion regresa una lista de redes bayesianas 
  # de la forma (pi,b).
  nl<-length(lisbnred)
  res<-list()
  for (i in 1:nl) {
    res[[i]]<-Regresa(lisbnred[i])
  }
  return(res)
}

DiferentialMutation<-function(x1,x2,x3)
{ #Esta funcion muta los individuos x1,x2,x3, F en [0.1,1]
  # ,es decir, y= (x1 + t) + F*(x2 - x3).
  FF<-runif(1,min = 0.1,max = 1)
  nv<-length(x1[[1]])
  dif<-resta(x2,x3)
  pes<-ProductoEscalar(dif,FF)
  premuta<-suma(x1,Premutacion(nv))
  yres<-suma(premuta,pes)
  return(yres)
}

Cruza<-function(x,y)
{ #Esta funcion hace la cruza de dos redes bayesianas, 
  # CR esta en [0,1]
  res1<-Cruzapi(x[[1]],y[[1]])
  res2<-Cruzab(x[[2]],y[[2]])
  res<-list(res1,res2)
  return(res)
}

ElembaseA<-function(numv)
{ #Esta funcion da un elemento del conjunto generador de redes
  # bayesianas A= ST' U U' donde ST'={(sigma_i,0): ...} 
  # U'={(iota,b_j):...}. El argumento es el numero de 
  # vertices de una red bayesiana
  m<-choose(numv,2)
  s<-sample(1:(numv-1),size = 1,replace = FALSE,
            prob = rep(1/(numv-1),numv-1))
  STprima<-pegabasesig(s,numv)
  u<-sample(1:m,size = 1,replace = FALSE,prob = rep(1/m,m))
  Uprima<-pegabasebit(u,numv)
  res<-suma(STprima[[1]],Uprima[[1]])
  return(res)
}

Premutacion<-function(nov)
{ #Esta funcion genera una red bayesiana t=(pi,b) mediante
  # un parametro de pre-mutacion pm en [0.1,0.3]. Su 
  # argumento es el numero de vertices y su salida es t rb
  # en forma de lista.
  pm<-runif(1,min = 0.1, max = 0.3)
  r<-runif(1,min = 0, max = 1)
  tt<-IdentidadR(nov)
  if (r>=pm) {return(IdentidadR(nov))} 
  while (r<pm) {
    redbase<-ElembaseA(nov)
    tt<-suma(redbase,tt)
    r<-runif(1,min = 0, max = 1)
  }
  return(tt)
}

maximocomponente<-function(x,y)
{ #Esta funcion calcula el maximo componente a componente de dos
  # vectores de igual magnitud
  n<-length(x)
  res<-rep(0,n)
  for (i in 1:n) {
    if (x[i]>=y[i]) {
      res[i]<-x[i]
    } else {
      res[i]<-y[i]
    }
  }
  return(res)
}

DEBN<-function(N,dataset,sctype)
{ #Esta funcion 
  #asia_1000<-rbn(x=dagn,n = 1000, data=datos, fit = "mle", debug = FALSE)
  #dataset <- red4 
  #N<-100 #numero de redes bayesianas
  #sctype<-"k2"
  xt<-Sys.time()
  n<-length(nchar(colnames(dataset))) #num de vertices= num de var
  prb<-Poblacion(N,n) #poblacion de 50 rb en una lista prb[[1]]
  pest<-POBES(prb) #lista de rb transformadas a as.bn pest[[1]]
  evaluaciones<-Eval(pest,datos=dataset,sctype) #evaluaciones de score de x
  ArcosX<-list()
  for (i in 1:N) {
    ArcosX[[i]]<-dim(pest[[i]]$arcs)[1]
  }
  Y<-list()
  Z<-list()
  ArcosZ<-list()
  quien<-list()
  evalz<-vector()
  for (i in 1:N) {
    individuos<-1:N
    indsini<-individuos[-i]
    indices<-sample(indsini, size =3, replace = FALSE,
                    prob = rep(1/(N-1),N-1))
    r1<-prb[[indices[1]]]
    r2<-prb[[indices[2]]]
    r3<-prb[[indices[3]]]
    Y[[i]]<-DiferentialMutation(x1=r1,x2=r2,x3=r3)
    Z[[i]]<-Cruza(prb[[i]],Y[[i]])
  }
  Zest<-POBES(Z)
  for (i in 1:N) {
    ArcosZ[[i]]<-dim(Zest[[i]]$arcs)[1]
  }
  evaluaz<-Eval(Zest,datos=dataset,sctype)
  
  Xmax<-max(evaluaciones)
  Zmax<-max(evaluaz)
  seleccion<-maximocomponente(x=evaluaciones,y=evaluaz)
  
  if (Xmax>Zmax) {
    posX<-which(evaluaciones==Xmax)
    resX<-prb[[posX]]
    Plot(resX)
    return(list(maxS=Xmax,opdag=resX,nets=c(pest,Zest),eval=c(evaluaciones,evaluaz), arcos=c(ArcosX,ArcosZ), tiempo=Sys.time()-xt))
  } else {
    posZ<-which(evaluaz==Zmax)
    resZ<-Z[[posZ]]
    Plot(resZ)
    return(list(maxS=Zmax,opdag=resZ,nets=c(pest,Zest),eval=c(evaluaciones,evaluaz),arcos=c(ArcosX,ArcosZ), tiempo=Sys.time()-xt))
  }
}

HC<-function(datos,typescore)
{
  x<-Sys.time()
  gad<-hc(x=datos, score = typescore)
  y<-Sys.time()-x
  s<-score(gad, data = datos, type = typescore) 
  res<-list(dag=gad,score=s,tiempo=y)
  return(res)
}

TABU<-function(datos,typescore)
{
  x<-Sys.time()
  gad<-tabu(x=datos, score = typescore)
  y<-Sys.time()-x
  s<-score(gad, data = datos, type = typescore) 
  res<-list(dag=gad,score=s,tiempo=y)
  return(res)
}

MMHC<-function(datos,typescore)
{
  x<-Sys.time()
  gad<-mmhc(x=datos, maximize.args = typescore)
  y<-Sys.time()-x
  s<-score(gad, data = datos, type = typescore) 
  res<-list(dag=gad,score=s,tiempo=y)
  return(res)
}

RM2<-function(algoritmo,datos,typescore)
{
  x<-Sys.time()
  gad<-rsmax2(x=datos, maximize = algoritmo ,maximize.args = typescore)
  y<-Sys.time()-x
  s<-score(gad, data = datos, type = typescore) 
  res<-list(dag=gad,score=s,tiempo=y)
  return(res)
}

mult2vec<-function(v1,v2)
{ # Esta funci?n multiplica dos vectores componente a componente,
  #  fija el segundo vector y multiplica las componentes del primero
  n<-length(v1)
  res<-vector()
  for (i in 1:n) {
    r<-v1[i]*v2
    res<-c(res,r)
  }
  return(res)
}

multvec<-function(l1)
{ # Esta funci?n multiplica una lista de vectores componente a componente,
  #  fija el ultimo vector y multiplica con el penultimo vector
  n<-length(l1)
  res<-l1[[n]]
  vec<-(n-1):1
  for (i in vec) {
   res<- mult2vec(v1=l1[[i]], v2=res)
  }
  return(res)
}

marginal<-function(M,p)
{ #Esta funcion calcula la distribucion marginal de un nodo en una red bayesiana disc.
  m<-dim(M)[1]
  res<-vector()
  for (i in 1:m) {
    res[i]<-sum(M[i,]*p)
  }
  return(res)
}

bn2igraph<-function(dag){ #Esta funcion para convertir un dag en formato bn (bnlearn) a un 
  #                         formato de igraph
  idag<-igraph.from.graphNEL(bnlearn::as.graphNEL(dag))
  return(idag)
}

nnivel<-function(n,mpos){max(mpos[mpos[,2]==n,1])}

meannivel<-function(n,mpos){mean(mpos[mpos[,2]==n,3])}

plotired <-function(pos,nivel,idag)
{
  mpos<-cbind(pos,nivel)
  nnivel(1,mpos)
  numniv<-sapply(nivel,FUN=nnivel,mpos=mpos)  #numero de elementos por nivel
  m_mpos<-cbind(mpos,pos/numniv)
  mean_nivel<-sapply(nivel,FUN=meannivel,mpos=m_mpos)  #numero de elementos por nivel
  xpos<-((m_mpos[,3]-mean_nivel)+.5)*400  #en el intervalo (0,400)
  y<-(max(nivel)-nivel+1)/max(nivel)
  ypos<-(y-mean(y)+0.5)*400
  lay<-cbind(xpos,ypos)
  
  plot(idag, edge.arrow.size=.6, edge.color="blue",
       vertex.color="orange", vertex.frame.color="black",
       vertex.label=V(idag)$media, vertex.label.color="black",
       layout=lay,vertex.size=10)
}

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

GeneraXi1p<-function(Pp,Pci)
{
  #Esta funcion simula 1000 obs. de un vertice con un padre y calcula su probabilidad marginal
  #sus argumentos son Pp= proba marginal del padre , Pci= proba condicional del nodo en cuestion.
  mXi <- sample(1:2,size = 1000, replace = TRUE,prob = Pp)
  Xi <- apply(Pci[,mXi],2,FUN=rmultinomvec)
  pmi <- marginal(M=Pci,p=Pp) #P(V_i=i) i=1,2
  return(list(X=Xi,P=pmi))
}

GeneraXi2p<-function(Pp1,Pp2,Pci)
{
  #Esta funcion simula 1000 obs. de un vertice con dos padres y calcula su probabilidad marginal
  # sus argumentos son Pp1= proba marginal del padre 1, Pp2= lo mismo con 2, y Pci= proba 
  # condicional del nodo en cuestion.
  pXi <- multvec(list(Pp1,Pp2)) #proba de entrar a la columna 1,2, 3 y 4
  mXi <- sample(1:4,size = 1000, replace = TRUE,prob = pXi)
  Xi <- apply(Pci[,mXi],2,FUN=rmultinomvec)
  pmi <- marginal(M=Pci,p=pXi) #P(V_i=i) i=1,2
  return(list(X=Xi,P=pmi))
}

EncuentraNivel<-function(ve){
  n<-length(ve)
  re<-rep(0,n)
  for (i in 1:n) {
    if (ve[i]<=16) {re[i]=1}
    if ((ve[i]>16)&&(ve[i]<=34)) {re[i]=2}
    if ((ve[i]>34)&&(ve[i]<=54)) {re[i]=3}
    if ((ve[i]>54)&&(ve[i]<=76)) {re[i]=4}
    if ((ve[i]>76)&&(ve[i]<=99)) {re[i]=5}
    if (ve[i]>99) {re[i]=6}
  }
  return(re)
}


cuenta1=function(v,mat){
  m=nrow(mat)
  r1= rep(0,m)
  for (i in 1:m) {
    r1[i]= min((v==mat[i,])*1)
  }
  return(sum(r1))
}

cuentaarcos=function(m1,m2){
  #Esta funcion cuenta el numero de arcos de la matriz m1 que coinciden con la matriz m2
  n1=nrow(m1)
  r2=rep(0,n1)
  for (i in 1:n1) {
    r2[i]= cuenta1(v=m1[i,],mat=m2)
  }
  return(sum(r2))
}

IniciaBNRB<-function(nv)
{ # Da una red bayesiana x=(pi,b) dejando el ultimo nodo como nodo de 
  #  supervivencia
  m<-choose(nv,2)
  nvm1=nv-1
  x<-1:nvm1
  pm1<-sample(x,size = nvm1,replace = FALSE,prob = rep(1/nvm1,nvm1))
  p=c(pm1,nv)
  b<-rbinom(n=m, size=1, prob=2/(nv-1))
  x1<-list(p,b)
  return(x1)
}


PoblacionBNRB<-function(N1,v)
{ #Genera una pobacion de redesbayesianas de tamano
  # N1 y numero de vertices v, con redes bayesianas restringidas
  m<-choose(v,2)
  res<-list()
  for (i in 1:N1) {
    res[[i]]<-IniciaBNRB(v)
  }
  return(res)
}

DEBNRest<-function(N,dataset,sctype)
{ #Esta funcion 
  #asia_1000<-rbn(x=dagn,n = 1000, data=datos, fit = "mle", debug = FALSE)
  #dataset <- red4 
  #N<-100 #numero de redes bayesianas
  #sctype<-"k2"
  xt<-Sys.time()
  n<-length(nchar(colnames(dataset))) #num de vertices= num de var
  prb<-PoblacionBNRB(N,n) #poblacion de 50 rb en una lista prb[[1]]
  pest<-POBES(prb) #lista de rb transformadas a as.bn pest[[1]]
  evaluaciones<-Eval(pest,datos=dataset,sctype) #evaluaciones de score de x
  ArcosX<-list()
  for (i in 1:N) {
    ArcosX[[i]]<-dim(pest[[i]]$arcs)[1]
  }
  Y<-list()
  Z<-list()
  ArcosZ<-list()
  quien<-list()
  evalz<-vector()
  for (i in 1:N) {
    individuos<-1:N
    indsini<-individuos[-i]
    indices<-sample(indsini, size =3, replace = FALSE,
                    prob = rep(1/(N-1),N-1))
    r1<-prb[[indices[1]]]
    r2<-prb[[indices[2]]]
    r3<-prb[[indices[3]]]
    Y[[i]]<-DiferentialMutation(x1=r1,x2=r2,x3=r3)
    Z[[i]]<-Cruza(prb[[i]],Y[[i]])
  }
  Zest<-POBES(Z)
  for (i in 1:N) {
    ArcosZ[[i]]<-dim(Zest[[i]]$arcs)[1]
  }
  evaluaz<-Eval(Zest,datos=dataset,sctype)
  
  Xmax<-max(evaluaciones)
  Zmax<-max(evaluaz)
  seleccion<-maximocomponente(x=evaluaciones,y=evaluaz)
  
  if (Xmax>Zmax) {
    posX<-which(evaluaciones==Xmax)
    resX<-prb[[posX]]
    Plot(resX)
    return(list(maxS=Xmax,opdag=resX,nets=c(pest,Zest),eval=c(evaluaciones,evaluaz), arcos=c(ArcosX,ArcosZ), tiempo=Sys.time()-xt))
  } else {
    posZ<-which(evaluaz==Zmax)
    resZ<-Z[[posZ]]
    Plot(resZ)
    return(list(maxS=Zmax,opdag=resZ,nets=c(pest,Zest),eval=c(evaluaciones,evaluaz),arcos=c(ArcosX,ArcosZ), tiempo=Sys.time()-xt))
  }
}
