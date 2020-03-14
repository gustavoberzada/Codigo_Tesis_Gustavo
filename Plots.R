#############################################################
############            Plot igraph        ##################
#############################################################
setwd("C:/Users/Gustavo/Dropbox/Gustavo")
source("Descomposicion.R")
#################         Ejemplo          ##################
data("asia") 
dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") 
plot(dag) 

idag<-bn2igraph(dag)  #idag esta en formato de red de igraph

#plot(idag,vertex.size=25)  
nivel<-c(1,2,4,3,2,1,2,4) #la posición, de arriba a abajo, segun el nombre del vertice
pos<-c(2,1,1,1,2,1,3,2)
#V(idag)
plotired(pos=pos,nivel=nivel,idag=idag)
#tkplot(idag)
#l <- tkplot.getcoords(7)
#plot(idag, edge.arrow.size=.6, edge.color="blue",
#     vertex.color="orange", vertex.frame.color="#ffffff",
#     vertex.label=V(idag)$media, vertex.label.color="black",
#     layout=l)




#################         Red 1          ##################
load("Red1.RData")
head(red1)
colnames(red1)<-c("1","2","3","4","5","6")
dagreal1 = model2network("[1][2][3][4][5][6|1:2:3:4:5]")
plot(dagreal1)
idagreal1<-bn2igraph(dagreal1) 
nivelreal1=c(1,1,1,1,1,2)
posreal1=c(1,2,3,4,5,1)
pdagreal1<-plotired(pos=posreal1,nivel=nivelreal1,idag=idagreal1)
##########################################################
load("DEBNk21.RData")
dagDEBNk21=model2network(Transforma(DEk21$opdag))
#plot(dagDEBNk21,radius = 240)
idagDEBNk21=bn2igraph(dagDEBNk21)
pdagreal1<-plotired(pos=posreal1,nivel=nivelreal1,idag=idagDEBNk21)
##########################################################
load("DEBNbde1.RData")
dagDEBNbde1=model2network(Transforma(DEbde1$opdag))
#plot(dagDEBNbde1,radius = 240)
idagDEBNbde1=bn2igraph(dagDEBNbde1)
pdagDEBNbde1<-plotired(pos=posreal1,nivel=nivelreal1,idag=idagDEBNbde1)
##########################################################
load("DEBNlogv1.RData")
dagDEBNlogv1=model2network(Transforma(DElogv1$opdag))
#plot(dagDEBNlogv1,radius = 240)
idagDEBNlogv1=bn2igraph(dagDEBNlogv1)
niveldelogv1<-c(1,1,1,2,1,2)
posdelogv1<-c(1,2,3,1,4,2)
pdagDEBNlogv1<-plotired(pos=posdelogv1,nivel=niveldelogv1,idag=idagDEBNlogv1)
##########################################################
load("Hclogv1.RData")
daghclogv1=hclogv1$dag
#plot(daghclogv1,radius = 240)
idaghclogv1=bn2igraph(daghclogv1)
plot(idaghclogv1,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idaghclogv1)$media, vertex.label.color="black",vertex.size=25)
##########################################################
########################## DAGs Restrigido
##############################################
load("DEBNRESk21.RData")
dagDEBNRESk21=model2network(Transforma(DERk21$opdag))
#plot(dagDEBNRESk21,radius = 240)
idagDEBNRESk21=bn2igraph(dagDEBNRESk21)

plot(idagDEBNRESk21,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESk21)$media, vertex.label.color="black",vertex.size=25)
##############################################
load("DEBNRESbde1.RData")
dagDEBNRESbde1=model2network(Transforma(DERbde1$opdag))
#plot(dagDEBNRESbde1,radius = 240)
idagDEBNRESbde1=bn2igraph(dagDEBNRESbde1)

plot(idagDEBNRESbde1,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESbde1)$media, vertex.label.color="black",vertex.size=25)
##############################################
load("DEBNRESlogv1.RData")
dagDEBNRESlogv1=model2network(Transforma(DERlogv1$opdag))
#plot(dagDEBNRESlogv1,radius = 240)
idagDEBNRESlogv1=bn2igraph(dagDEBNRESlogv1)

plot(idagDEBNRESlogv1,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESlogv1)$media, vertex.label.color="black",vertex.size=25)


#################         Red 2          ##################
load("Red2.RData")
head(red2)
colnames(red2)<-c("1","2","3","4","5","6")
dagreal2 = model2network("[1][2][5][4|1:2][3|1:2:5][6|4:3]")
plot(dagreal2)
idagreal2<-bn2igraph(dagreal2) 
nivelreal2=c(1,1,2,2,2,3)
posreal2=c(1,2,2,1,3,1)
pdagreal2<-plotired(pos=posreal2,nivel=nivelreal2,idag=idagreal2)
##########################################################
load("DEBNk22.RData")
dagDEBNk22=model2network(Transforma(DEk22$opdag))
#plot(dagDEBNk22,radius = 240)
idagDEBNk22=bn2igraph(dagDEBNk22)
pdagreal2<-plotired(pos=posreal2,nivel=nivelreal2,idag=idagDEBNk22)
##########################################################
load("DEBNbde2.RData")
dagDEBNbde2=model2network(Transforma(DEbde2$opdag))
#plot(dagDEBNbde2,radius = 240)
idagDEBNbde2=bn2igraph(dagDEBNbde2)
pdagDEBNbde2<-plotired(pos=posreal2,nivel=nivelreal2,idag=idagDEBNbde2)
##########################################################
load("DEBNlogv2.RData")
dagDEBNlogv2=model2network(Transforma(DElogv2$opdag))
#plot(dagDEBNlogv2,radius = 240)
idagDEBNlogv2=bn2igraph(dagDEBNlogv2)
plot(idagDEBNlogv2,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNlogv2)$media, vertex.label.color="black",vertex.size=25)

##########################################################
load("Hclogv2.RData")
daghclogv2=hclogv2$dag
#plot(daghclogv2,radius = 240)
idaghclogv2=bn2igraph(daghclogv2)
plot(idaghclogv2,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idaghclogv2)$media, vertex.label.color="black",vertex.size=25)
##########################################################
########################## DAGs Restrigido
##############################################
load("DEBNRESk22.RData")
dagDEBNRESk22=model2network(Transforma(DERk22$opdag))
#plot(dagDEBNRESk22,radius = 240)
idagDEBNRESk22=bn2igraph(dagDEBNRESk22)

plot(idagDEBNRESk22,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESk22)$media, vertex.label.color="black",vertex.size=25)
##############################################
load("DEBNRESbde2.RData")
dagDEBNRESbde2=model2network(Transforma(DERbde2$opdag))
#plot(dagDEBNRESbde2,radius = 240)
idagDEBNRESbde2=bn2igraph(dagDEBNRESbde2)

plot(idagDEBNRESbde2,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESbde2)$media, vertex.label.color="black",vertex.size=25)
##############################################
load("DEBNRESlogv2.RData")
dagDEBNRESlogv2=model2network(Transforma(DERlogv2$opdag))
#plot(dagDEBNRESlogv2,radius = 240)
idagDEBNRESlogv2=bn2igraph(dagDEBNRESlogv2)

plot(idagDEBNRESlogv2,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESlogv2)$media, vertex.label.color="black",vertex.size=25)

tkplot(idagDEBNRESlogv2)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(2)
plot(idagDEBNRESlogv2,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESlogv2)$media, vertex.label.color="black",vertex.size=25, layout=l)


#################         Red 3          ##################
load("Red3.RData")
head(red3)
colnames(red3)<-c("1","2","3","4","5")
dagreal3 = model2network("[1][2][3|1:2][4|1:2:3][5|4]")
plot(dagreal3)
idagreal3<-bn2igraph(dagreal3) 
nivelreal3=c(2,1,2,3,3)
posreal3=c(2,1,1,1,2)
pdagreal3<-plotired(pos=posreal3,nivel=nivelreal3,idag=idagreal3)
##########################################################
load("DEBNk23.RData")
dagDEBNk23=model2network(Transforma(DEk23$opdag))
#plot(dagDEBNk23,radius = 240)
idagDEBNk23=bn2igraph(dagDEBNk23)
pdagdek23<-plotired(pos=posreal3,nivel=nivelreal3,idag=idagDEBNk23)
##########################################################
load("DEBNbde3.RData")
dagDEBNbde3=model2network(Transforma(DEbde3$opdag))
#plot(dagDEBNbde3,radius = 240)
idagDEBNbde3=bn2igraph(dagDEBNbde3)
pdagDEBNbde3<-plotired(pos=posreal3,nivel=nivelreal3,idag=idagDEBNbde3)
##########################################################
load("DEBNlogv3.RData")
dagDEBNlogv3=model2network(Transforma(DElogv3$opdag))
#plot(dagDEBNlogv3,radius = 240)
idagDEBNlogv3=bn2igraph(dagDEBNlogv3)
pdagDEBNbde3<-plotired(pos=posreal3,nivel=nivelreal3,idag=idagDEBNlogv3)

##########################################################
load("Hclogv3.RData")
daghclogv3=hclogv3$dag
#plot(daghclogv3,radius = 240)
idaghclogv3=bn2igraph(daghclogv3)
pdagDEBNbde3<-plotired(pos=posreal3,nivel=nivelreal3,idag=idaghclogv3)
##########################################################
########################## DAGs Restrigido
##############################################
load("DEBNRESk23.RData")
dagDEBNRESk23=model2network(Transforma(DERk23$opdag))
#plot(dagDEBNRESk23,radius = 240)
idagDEBNRESk23=bn2igraph(dagDEBNRESk23)

plot(idagDEBNRESk23,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESk23)$media, vertex.label.color="black",vertex.size=25)
##############################################
load("DEBNRESbde3.RData")
dagDEBNRESbde3=model2network(Transforma(DERbde3$opdag))
#plot(dagDEBNRESbde3,radius = 240)
idagDEBNRESbde3=bn2igraph(dagDEBNRESbde3)

plot(idagDEBNRESbde3,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESbde3)$media, vertex.label.color="black",vertex.size=25)
##############################################
load("DEBNRESlogv3.RData")
dagDEBNRESlogv3=model2network(Transforma(DERlogv3$opdag))
#plot(dagDEBNRESlogv3,radius = 240)
idagDEBNRESlogv3=bn2igraph(dagDEBNRESlogv3)

plot(idagDEBNRESlogv3,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESlogv3)$media, vertex.label.color="black",vertex.size=25)

tkplot(idagDEBNRESlogv3)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(2)
plot(idagDEBNRESlogv3,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESlogv3)$media, vertex.label.color="black",vertex.size=25, layout=l)



#################         Red 4          ##################
load("Red4.RData")
head(red4)
colnames(red4)<-c("1","2","3","4","5","6","7","8","9","10",
                  "11","12","13","14","15","16","17","18","19","20")
dagreal4 = model2network("[1][2|1][3|2][4|1][5|1][6|2][7|3][8|1][9|5][10|6][11|5:7][12|6][13|7][14|9:10][15|11:14][16|12:13:15][17|14][18|15][19|16][20|17:18:19]")
plot(dagreal4)
idagreal4<-bn2igraph(dagreal4) 
#V(idagreal4) 1,10,11,12,13,14,15,16,17,18,19,2,20, 3,4,5,6,7,8,9
nivelreal4=c( 1, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 1, 6,1,2,2,2,2,2,3)
posreal4=  c( 1, 2, 3, 4, 5, 1, 2, 3, 1, 2, 3, 2, 1,3,1,2,3,4,5,1)
pdagreal4<-plotired(pos=posreal4,nivel=nivelreal4,idag=idagreal4)

##########################################################
load("DEBNk24.RData")
dagDEBNk24=model2network(Transforma(DEk24$opdag))
#plot(dagDEBNk24,radius = 240)
idagDEBNk24=bn2igraph(dagDEBNk24)
plot(idagDEBNk24,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNk24)$media, vertex.label.color="black",vertex.size=25)
tkplot(idagDEBNk24)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(28)
plot(idagDEBNk24,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNk24)$media, vertex.label.color="black",vertex.size=25, layout=l)

##########################################################
load("DEBNbde4.RData")
dagDEBNbde4=model2network(Transforma(DEbde4$opdag))
#plot(dagDEBNbde4,radius = 240)
idagDEBNbde4=bn2igraph(dagDEBNbde4)
pdagDEBNbde4<-plotired(pos=posreal4,nivel=nivelreal4,idag=idagDEBNbde4)
##########################################################
load("DEBNlogv4.RData")
dagDEBNlogv4=model2network(Transforma(DElogv4$opdag))
#plot(dagDEBNlogv4,radius = 240)
idagDEBNlogv4=bn2igraph(dagDEBNlogv4)
plot(idagDEBNlogv4,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNlogv4)$media, vertex.label.color="black",vertex.size=25)
tkplot(idagDEBNlogv4)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(29)
plot(idagDEBNlogv4,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNlogv4)$media, vertex.label.color="black",vertex.size=25, layout=l)


##########################################################
load("Hclogv4.RData")
daghclogv4=hclogv4$dag
#plot(daghclogv4,radius = 240)
idaghclogv4=bn2igraph(daghclogv4)
plot(idaghclogv4,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idaghclogv4)$media, vertex.label.color="black",vertex.size=25)
tkplot(idaghclogv4)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(30)
plot(idaghclogv4,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idaghclogv4)$media, vertex.label.color="black",vertex.size=25, layout=l)
##########################################################
########################## DAGs Restrigido
##############################################
load("DEBNRESk24.RData")
dagDEBNRESk24=model2network(Transforma(DERk24$opdag))
#plot(dagDEBNRESk24,radius = 240)
idagDEBNRESk24=bn2igraph(dagDEBNRESk24)

plot(idagDEBNRESk24,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESk24)$media, vertex.label.color="black",vertex.size=25)

tkplot(idagDEBNRESk24)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(3)
plot(idagDEBNRESk24,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESk24)$media, vertex.label.color="black",vertex.size=25, layout=l)



##############################################
load("DEBNRESbde4.RData")
dagDEBNRESbde4=model2network(Transforma(DERbde4$opdag))
#plot(dagDEBNRESbde4,radius = 240)
idagDEBNRESbde4=bn2igraph(dagDEBNRESbde4)

plot(idagDEBNRESbde4,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESbde4)$media, vertex.label.color="black",vertex.size=25)
tkplot(idagDEBNRESbde4)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(4)
plot(idagDEBNRESbde4,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESbde4)$media, vertex.label.color="black",vertex.size=25, layout=l)


##############################################
load("DEBNRESlogv4.RData")
dagDEBNRESlogv4=model2network(Transforma(DERlogv4$opdag))
#plot(dagDEBNRESlogv4,radius = 240)
idagDEBNRESlogv4=bn2igraph(dagDEBNRESlogv4)

plot(idagDEBNRESlogv4,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESlogv4)$media, vertex.label.color="black",vertex.size=25)

tkplot(idagDEBNRESlogv4)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(6)
plot(idagDEBNRESlogv4,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESlogv4)$media, vertex.label.color="black",vertex.size=25, layout=l)




#dag<-as.bn("[A][S][E|A:S][O|E][R|E][T|O:R]")
#idag<-bn2igraph(dag) 
#V(idag)
#niveld=c(1,2,3,3,1,4)
#posd  =  c(1,1,1,2,2,1)
#pdagreal4<-plotired(pos=posd,nivel=niveld,idag=idag)

################## plots de la suma, diferencia y producto punto #########

a=as.bn("[1][2|4][3|1][4|1]")
b=as.bn("[1][2|4][3|4][4|1]")
r1=as.bn("[1][2][3|1][4|3]")
r2=as.bn("[1][2][4][3|1:4]")
c=as.bn("[2][4][1|3:4][3|2:4]")
r3=as.bn("[1][2][3|1][4|1]")

plot(a)
plot(b)
plot(r1)
plot(r2)
plot(c)
plot(r3)

ia=bn2igraph(a)
ib=bn2igraph(b)
ir1=bn2igraph(r1)
ir2=bn2igraph(r2)
ic=bn2igraph(c)
ir3=bn2igraph(r3)

#V(ia)
niv=c(1,2,3,2)
pos=c(1,1,1,2)

plotired(pos=pos,nivel = niv,idag = ia)
plotired(pos=pos,nivel = niv,idag = ib)
plotired(pos=pos,nivel = niv,idag = ir1)
plotired(pos=pos,nivel = niv,idag = ir2)
plotired(pos=pos,nivel = niv,idag = ic)
plotired(pos=pos,nivel = niv,idag = ir3)


dag=as.bn("[X1][X2][Xn][T][S|X1:X2:Xn:T]")
#par(mar=c(0,0,0,0))
plot(dag)
idag=bn2igraph(dag)
plot(idag,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idag)$media, vertex.label.color="black",vertex.size=30)
tkplot(idag)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(4)
plot(idag,edge.arrow.size=.5, edge.color="blue",
     vertex.color=c("orange","cyan","orange","orange","orange"), vertex.frame.color="black",
     vertex.label=V(idag)$media, vertex.label.color="black",vertex.size=35, layout=l)

