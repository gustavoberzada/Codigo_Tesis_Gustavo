##############################################################
##################     Ejemplo Asia   ########################
##############################################################
setwd("C:/Users/Gustavo/Dropbox/Gustavo")
#par(mar=c(0,0,0,0))
rm(list=ls(all=TRUE))
source("Descomposicion.R")
data(asia)
dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")
#plot(dag)
#colnames(asia)
#head(asia)
# datos asia con nombres numericos
datos<-asia
colnames(datos)<-c("1","2","3","4","5","6","7","8")
#head(datos)
dagn = model2network("[1][2][3|1][4|2][5|2][8|5:6][6|3:4][7|6]")
plot(dagn)
idag=bn2igraph(dagn)
plot(idag, edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idag)$media, vertex.label.color="black",vertex.size=25)
tkplot(idag)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(2)
plot(idag,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idag)$media, vertex.label.color="black",vertex.size=25, layout=l)




#parRegresa(dagn)
#dagn = model2network("[1][2][3][4|2][5|2][8|5:6][6|3:4][7|6]")
#S<-score(dagn,data=datos,type="k2") # -11110.15
#simula datos de una red con 1000 observaciones
asia_1000<-rbn(x=dagn,n = 1000, data=datos, fit = "mle", debug = FALSE)

RES<-DEBN(N=50, dataset = datos,sctype="k2") #-2285.391 pi=1 3 4 5 8 7 2 6, 
#b=0 1 0 0 0 0 1 0 0 0 0 0 1 0 0 1 1 1 0 1 1 1 0 0 1 0 1 0
RES$maxS
RES$opdag
de=model2network(Transforma(RES$opdag))
plot(de,radius = 240)
ide=bn2igraph(de)
plot(ide, edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(ide)$media, vertex.label.color="black",vertex.size=25)
tkplot(ide)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(3)
plot(ide,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(ide)$media, vertex.label.color="black",vertex.size=25, layout=l)




arcosx<-RES$arcos[1:50]
arcosz<-RES$arcos[51:100]
sx<-RES$eval[1:50]
sz<-RES$eval[51:100]
plot(arcosx,sx, xlab = "Número de arcos", ylab = "Score K2", cex=1.5,
     main = "Selección de la mutación", pch=16, col=rgb(1,0,0,0.6), ylim=c(min(sx,sz),max(sx,sz)))
points(arcosz,sz, pch=17, col=rgb(0,0.2,1,0.6),cex=1.5)
legend("topleft", legend=c("Población generada", "Población mutada"),
       col=c(rgb(1,0,0,0.6), rgb(0,0.2,1,0.6)), pch = c(16,17),box.lty=1,cex = c(1.5,1.5))
RES$tiempo
##############################################################
##################     Red bayesiana 1      ##################
##############################################################
load("Red1.RData")
head(red1)
colnames(red1)<-c("1","2","3","4","5","6")
#dag1r = model2network("[X1][X2][X3][X4][T][S|X1:X2:X3:X4:T]")
#plot(dag1r) #T=5 y S=6
dag1 = model2network("[1][2][3][4][5][6|1:2:3:4:5]")
plot(dag1)
################### Score's del grafo real
#score(dag1,data = red1, type = "k2") 
#score(dag1,data = red1, type = "bde") 
#score(dag1,data = red1, type = "loglik") 
################### DEBN 
#DEk21<-DEBN(N=100, dataset = red1, sctype="k2")
#save(DEk21,file="DEBNk21.RData")
#rm(DEk21)
load("DEBNk21.RData")
DEk21$maxS
DEk21$tiempo
plot(model2network(Transforma(DEk21$opdag)),radius = 240)

################################################
#DEbde1<-DEBN(N=100, dataset = red1, sctype="bde")
#save(DEbde1,file="DEBNbde1.RData")
#rm(DEbde1)
load("DEBNbde1.RData")
DEbde1$maxS
DEbde1$tiempo
plot(model2network(Transforma(DEbde1$opdag)),radius = 240)
################################################
#DElogv1<-DEBN(N=100, dataset = red1, sctype="loglik")
#save(DElogv1,file="DEBNlogv1.RData")
#rm(DElogv1)
load("DEBNlogv1.RData")
DElogv1$maxS
DElogv1$tiempo
plot(model2network(Transforma(DElogv1$opdag)),radius = 240)
###################  hc
#hck21<-HC(datos=red1,typescore="k2")
#save(hck21,file="HCk21.RData")
#rm(hck21)
load("HCk21.RData")
hck21$score
hck21$tiempo
plot(hck21$dag, radius = 240)
################################################
#hcbde1<-HC(datos=red1,typescore="bde")
#save(hcbde1,file="HCbde1.RData")
#rm(hcbde1)
load("HCbde1.RData")
hcbde1$score
hcbde1$tiempo
plot(hcbde1$dag, radius = 240) 
################################################
#hclogv1<-HC(datos=red1,typescore="loglik")
#save(hclogv1,file="HClogv1.RData")
#rm(hclogv1)
load("HClogv1.RData")
hclogv1$score
hclogv1$tiempo
plot(hclogv1$dag, radius = 240)
##############################################
tak21<-TABU(datos=red1,typescore="k2")
tak21$score
tak21$tiempo
plot(tak21$dag, radius = 240) 
##############################################
tabde1<-TABU(datos=red1,typescore="bde")
tabde1$score
tabde1$tiempo
plot(tabde1$dag, radius = 240)
##############################################
talogv1<-TABU(datos=red1,typescore="loglik")
talogv1$score
talogv1$tiempo
plot(talogv1$dag, radius = 240)

##############################################
mmhck21<-MMHC(datos=red1,typescore="k2")
mmhck21$score
mmhck21$tiempo
plot(mmhck21$dag, radius = 240) 
##############################################
mmhcbde1<-MMHC(datos=red1,typescore="bde")
mmhcbde1$score
mmhcbde1$tiempo
plot(mmhcbde1$dag, radius = 240) 
##############################################
mmhclogv1<-MMHC(datos=red1,typescore="loglik")
mmhclogv1$score
mmhclogv1$tiempo
plot(mmhclogv1$dag, radius = 240) 

##############################################
rmk21<-RM2(algoritmo="tabu",datos=red1,typescore="k2")
rmk21$score
rmk21$tiempo
plot(rmk21$dag, radius = 240) 
##############################################
rmbde1<-RM2(algoritmo="tabu",datos=red1,typescore="bde")
rmbde1$score
rmbde1$tiempo
plot(rmbde1$dag, radius = 240)
##############################################
rmlogv1<-RM2(algoritmo="tabu",datos=red1,typescore="loglik")
rmlogv1$score
rmlogv1$tiempo
plot(rmlogv1$dag, radius = 240)

###############################################
################### DEBN Restringido
#DERk21<-DEBNRest(N=100, dataset = red1, sctype="k2")
#save(DERk21,file="DEBNRESk21.RData")
#rm(DERk21)
load("DEBNRESk21.RData")
DERk21$maxS
DERk21$tiempo
plot(model2network(Transforma(DERk21$opdag)),radius = 240)
#DERbde1<-DEBNRest(N=100, dataset = red1, sctype="bde")
#save(DERbde1,file="DEBNRESbde1.RData")
#rm(DERbde1)
load("DEBNRESbde1.RData")
DERbde1$maxS
DERbde1$tiempo
plot(model2network(Transforma(DERbde1$opdag)),radius = 240)
#DERlogv1<-DEBNRest(N=100, dataset = red1, sctype="loglik")
#save(DERlogv1,file="DEBNRESlogv1.RData")
#rm(DERlogv1)
load("DEBNRESlogv1.RData")
DERlogv1$maxS
DERlogv1$tiempo
plot(model2network(Transforma(DERlogv1$opdag)),radius = 240)

##############################################################
##################     Red bayesiana 2      ##################
##############################################################
rm(red1)
load("Red2.RData")
colnames(red2)<-c("1","2","3","4","5","6")
#dag2r = model2network("[X1][X2][T][X4|X1:X2][X3|X1:X2:T][S|X4:X3]")
#plot(dag2r) #T=5 y S=6
dag2 = model2network("[1][2][5][4|1:2][3|1:2:5][6|4:3]")
plot(dag2)
################### Score's del grafo real
#score(dag2,data = red2, type = "k2") = -5170.832
#score(dag2,data = red2, type = "bde") = -5293.971
#score(dag2,data = red2, type = "loglik") = -5066.337
################### DEBN 
#DEk22<-DEBN(N=100, dataset = red2, sctype="k2")
#save(DEk22,file="DEBNk22.RData")
#rm(DEk22)
load("DEBNk22.RData")
DEk22$maxS
DEk22$tiempo
plot(model2network(Transforma(DEk22$opdag)),radius = 240)
################################################
#DEbde2<-DEBN(N=100, dataset = red2, sctype="bde")
#save(DEbde2,file="DEBNbde2.RData")
#rm(DEbde2)
load("DEBNbde2.RData")
DEbde2$maxS
DEbde2$tiempo
plot(model2network(Transforma(DEbde2$opdag)),radius = 240)
################################################
#DElogv2<-DEBN(N=100, dataset = red2, sctype="loglik")
#save(DElogv2,file="DEBNlogv2.RData")
#rm(DElogv2)
load("DEBNlogv2.RData")
DElogv2$maxS
DElogv2$tiempo
plot(model2network(Transforma(DElogv2$opdag)),radius = 240)
################### scores hc
#hck22<-HC(datos=red2,typescore="k2")
#save(hck22,file="HCk22.RData")
#rm(hck22)
load("HCk22.RData")
hck22$score
hck22$tiempo
plot(hck22$dag, radius = 240)
################################################
#hcbde2<-HC(datos=red2,typescore="bde")
#save(hcbde2,file="HCbde2.RData")
#rm(hcbde2)
load("HCbde2.RData")
hcbde2$score
hcbde2$tiempo
plot(hcbde2$dag, radius = 240)
################################################
#hclogv2<-HC(datos=red2,typescore="loglik")
#save(hclogv2,file="HClogv2.RData")
#rm(hclogv2)
load("HClogv2.RData")
hclogv2$score
hclogv2$tiempo
plot(hclogv2$dag, radius = 240)

##############################################
tak22<-TABU(datos=red2,typescore="k2")
tak22$score
tak22$tiempo
plot(tak22$dag, radius = 240) 
##############################################
tabde2<-TABU(datos=red2,typescore="bde")
tabde2$score
tabde2$tiempo
plot(tabde2$dag, radius = 240)
##############################################
talogv2<-TABU(datos=red2,typescore="loglik")
talogv2$score
talogv2$tiempo
plot(talogv2$dag, radius = 240)
#mismo que hclogv2
##############################################
mmhck22<-MMHC(datos=red2,typescore="k2")
mmhck22$score
mmhck22$tiempo
plot(mmhck22$dag, radius = 240) 
##############################################
mmhcbde2<-MMHC(datos=red2,typescore="bde")
mmhcbde2$score
mmhcbde2$tiempo
plot(mmhcbde2$dag, radius = 240) 
##############################################
mmhclogv2<-MMHC(datos=red2,typescore="loglik")
mmhclogv2$score
mmhclogv2$tiempo
plot(mmhclogv2$dag, radius = 240) 
#save(mmhclogv2,file ="MMHClogv2.RData")
##############################################
rmk22<-RM2(algoritmo="tabu",datos=red2,typescore="k2")
rmk22$score
rmk22$tiempo
plot(rmk22$dag, radius = 240) 
##############################################
rmbde2<-RM2(algoritmo="tabu",datos=red2,typescore="bde")
rmbde2$score
rmbde2$tiempo
plot(rmbde2$dag, radius = 240)
##############################################
rmlogv2<-RM2(algoritmo="tabu",datos=red2,typescore="loglik")
rmlogv2$score
rmlogv2$tiempo
plot(rmlogv2$dag, radius = 240)
#save(rmlogv2,file ="RMlogv2.RData")
###############################################
################### DEBN Restringido
#DERk22<-DEBNRest(N=100, dataset = red2, sctype="k2")
#save(DERk22,file="DEBNRESk22.RData")
#rm(DERk22)
load("DEBNRESk22.RData")
DERk22$maxS
DERk22$tiempo
plot(model2network(Transforma(DERk22$opdag)),radius = 240)
#DERbde2<-DEBNRest(N=100, dataset = red2, sctype="bde")
#save(DERbde2,file="DEBNRESbde2.RData")
#rm(DERbde2)
load("DEBNRESbde2.RData")
DERbde2$maxS
DERbde2$tiempo
plot(model2network(Transforma(DERbde2$opdag)),radius = 240)
#DERlogv2<-DEBNRest(N=100, dataset = red2, sctype="loglik")
#save(DERlogv2,file="DEBNRESlogv2.RData")
#rm(DERlogv2)
load("DEBNRESlogv2.RData")
DERlogv2$maxS
DERlogv2$tiempo
plot(model2network(Transforma(DERlogv2$opdag)),radius = 240)

##############################################################
##################     Red bayesiana 3      ##################
##############################################################
rm(red2)
load("Red3.RData")
colnames(red3)<-c("1","2","3","4","5")
#dag3r = model2network("[X1][X2][X3|X1:X2][X4|X1:X2:X3][X5|X4]")
#plot(dag3r) #T=5 y S=6
dag3 = model2network("[1][2][3|1:2][4|1:2:3][5|4]")
plot(dag3)
################### Score's del grafo real
#score(dag3,data = red3, type = "k2") =-4533.472
#score(dag3,data = red3, type = "bde") =-4626.819
#score(dag3,data = red3, type = "loglik") =-4444.53
################### DEBN 
#DEk23<-DEBN(N=100, dataset = red3, sctype="k2")
#save(DEk23,file="DEBNk23.RData")
#rm(DEk23)
load("DEBNk23.RData")
DEk23$maxS
DEk23$tiempo
plot(model2network(Transforma(DEk23$opdag)),radius = 240)
################################################
#DEbde3<-DEBN(N=100, dataset = red3, sctype="bde")
#save(DEbde3,file="DEBNbde3.RData")
#rm(DEbde3)
load("DEBNbde3.RData")
DEbde3$maxS
DEbde3$tiempo
plot(model2network(Transforma(DEbde3$opdag)),radius = 240)
################################################
#DElogv3<-DEBN(N=100, dataset = red3, sctype="loglik")
#save(DElogv3,file="DEBNlogv3.RData")
#rm(DElogv3)
load("DEBNlogv3.RData")
DElogv3$maxS
DElogv3$tiempo
plot(model2network(Transforma(DElogv3$opdag)),radius = 240)
################### scores hc
#hck23<-HC(datos=red3,typescore="k2")
#save(hck23,file="HCk23.RData")
#rm(hck23)
load("HCk23.RData")
hck23$score
hck23$tiempo
plot(hck23$dag, radius = 240)
################################################
#hcbde3<-HC(datos=red3,typescore="bde")
#save(hcbde3,file="HCbde3.RData")
#rm(hcbde3)
load("HCbde3.RData")
hcbde3$score
hcbde3$tiempo
plot(hcbde3$dag, radius = 240)
################################################
#hclogv3<-HC(datos=red3,typescore="loglik")
#save(hclogv3,file="HClogv3.RData")
#rm(hclogv2)
load("HClogv3.RData")
hclogv3$score
hclogv3$tiempo
plot(hclogv3$dag, radius = 240)
rm(red2)
##############################################
tak23<-TABU(datos=red3,typescore="k2")
tak23$score
tak23$tiempo
plot(tak23$dag, radius = 240) 
##############################################
tabde3<-TABU(datos=red3,typescore="bde")
tabde3$score
tabde3$tiempo
plot(tabde3$dag, radius = 240)
##############################################
talogv3<-TABU(datos=red3,typescore="loglik")
talogv3$score
talogv3$tiempo
plot(talogv3$dag, radius = 240)
#mismo que hclogv3
##############################################
mmhck23<-MMHC(datos=red3,typescore="k2")
mmhck23$score
mmhck23$tiempo
plot(mmhck23$dag, radius = 240) 
##############################################
mmhcbde3<-MMHC(datos=red3,typescore="bde")
mmhcbde3$score
mmhcbde3$tiempo
plot(mmhcbde3$dag, radius = 240) 
##############################################
mmhclogv3<-MMHC(datos=red3,typescore="loglik")
mmhclogv3$score
mmhclogv3$tiempo
plot(mmhclogv3$dag, radius = 240) 
#save(mmhclogv2,file ="MMHClogv2.RData")
##############################################
rmk23<-RM2(algoritmo="tabu",datos=red3,typescore="k2")
rmk23$score
rmk23$tiempo
plot(rmk23$dag, radius = 240) 
##############################################
rmbde3<-RM2(algoritmo="tabu",datos=red3,typescore="bde")
rmbde3$score
rmbde3$tiempo
plot(rmbde3$dag, radius = 240)
##############################################
rmlogv3<-RM2(algoritmo="tabu",datos=red3,typescore="loglik")
rmlogv3$score
rmlogv3$tiempo
plot(rmlogv3$dag, radius = 240)
#save(rmlogv2,file ="RMlogv2.RData")
###############################################
################### DEBN Restringido
#DERk23<-DEBNRest(N=100, dataset = red3, sctype="k2")
#save(DERk23,file="DEBNRESk23.RData")
#rm(DERk23)
load("DEBNRESk23.RData")
DERk23$maxS
DERk23$tiempo
plot(model2network(Transforma(DERk23$opdag)),radius = 240)
#DERbde3<-DEBNRest(N=100, dataset = red3, sctype="bde")
#save(DERbde3,file="DEBNRESbde3.RData")
#rm(DERbde3)
load("DEBNRESbde3.RData")
DERbde3$maxS
DERbde3$tiempo
plot(model2network(Transforma(DERbde3$opdag)),radius = 240)
#DERlogv3<-DEBNRest(N=100, dataset = red3, sctype="loglik")
#save(DERlogv3,file="DEBNRESlogv3.RData")
#rm(DERlogv3)
load("DEBNRESlogv3.RData")
DERlogv3$maxS
DERlogv3$tiempo
plot(model2network(Transforma(DERlogv3$opdag)),radius = 240)
##############################################################
##################     Red bayesiana 4      ##################
##############################################################
load("Red4.RData")
rm(red3)
colnames(red4)<-c("1","2","3","4","5","6","7","8","9","10",
                  "11","12","13","14","15","16","17","18","19","20")
dag4 = model2network("[1][2|1][3|2][4|1][5|1][6|2][7|3][8|1][9|5][10|6][11|5:7][12|6][13|7][14|9:10][15|11:14][16|12:13:15][17|14][18|15][19|16][20|17:18:19]")
plot(dag4,radius = 280) #dag4$arcs
#score(dag4,data = red4, type = "k2") = -12587.17
#score(dag4,data = red4, type = "bde") = -12661.81
#score(dag4,data = red4, type = "loglik") =-12437.41
################### DEBN 
#DEk24<-DEBN(N=100, dataset = red4, sctype="k2")
#save(DEk24,file="DEBNk24.RData")
#rm(DEk24)
load("DEBNk24.RData")
DEk24$maxS
DEk24$tiempo
gDEk24<-model2network(Transforma(DEk24$opdag))
plot(gDEk24,radius = 240)
gDEk24$arcs
################################################
#DEbde4<-DEBN(N=100, dataset = red4, sctype="bde")
#save(DEbde4,file="DEBNbde4.RData")
#rm(DEbde4)
load("DEBNbde4.RData")
DEbde4$maxS
DEbde4$tiempo
gDEbde4<-model2network(Transforma(DEbde4$opdag))
plot(gDEbde4,radius = 240)
gDEbde4$arcs
  ################################################
#DElogv4<-DEBN(N=100, dataset = red4, sctype="loglik")
#save(DElogv4,file="DEBNlogv4.RData")
#rm(DElogv4)
load("DEBNlogv4.RData")
DElogv4$maxS
DElogv4$tiempo
gDElogv4<-model2network(Transforma(DElogv4$opdag))
plot(gDElogv4,radius = 240)
gDElogv4$arcs
################### scores hc
#hck24<-HC(datos=red4,typescore="k2")
#save(hck24,file="HCk24.RData")
#rm(hck24)
load("HCk24.RData")
hck24$score
hck24$tiempo
plot(hck24$dag, radius = 240)
hck24$dag$arcs
################################################
#hcbde4<-HC(datos=red4,typescore="bde")
#save(hcbde4,file="HCbde4.RData")
#rm(hcbde4)
load("HCbde4.RData")
hcbde4$score
hcbde4$tiempo
plot(hcbde4$dag, radius = 240)
hcbde4$dag$arcs
################################################
#hclogv4<-HC(datos=red4,typescore="loglik")
#save(hclogv4,file="HClogv4.RData")
#rm(hclogv4)
load("HClogv4.RData")
hclogv4$score
hclogv4$tiempo
plot(hclogv4$dag, radius = 240)
hclogv4$dag$arcs
##############################################
tak24<-TABU(datos=red4,typescore="k2")
tak24$score
tak24$tiempo
plot(tak24$dag, radius = 240) 
tak24$dag$arcs
##############################################
tabde4<-TABU(datos=red4,typescore="bde")
tabde4$score
tabde4$tiempo
plot(tabde4$dag, radius = 240)
tabde4$dag$arcs
##############################################
talogv4<-TABU(datos=red4,typescore="loglik")
talogv4$score
talogv4$tiempo
plot(talogv4$dag, radius = 240)
talogv4$dag$arcs
#mismo que hclogv3
##############################################
mmhck24<-MMHC(datos=red4,typescore="k2")
mmhck24$score
mmhck24$tiempo
plot(mmhck24$dag, radius = 240) 
mmhck24$dag$arcs
##############################################
mmhcbde4<-MMHC(datos=red4,typescore="bde")
mmhcbde4$score
mmhcbde4$tiempo
plot(mmhcbde4$dag, radius = 240)
mmhcbde4$dag$arcs
##############################################
mmhclogv4<-MMHC(datos=red4,typescore="loglik")
mmhclogv4$score
mmhclogv4$tiempo
plot(mmhclogv4$dag, radius = 240) 
mmhclogv4$dag$arcs
#save(mmhclogv2,file ="MMHClogv2.RData")
##############################################
rmk24<-RM2(algoritmo="tabu",datos=red4,typescore="k2")
rmk24$score
rmk24$tiempo
plot(rmk24$dag, radius = 240) 
rmk24$dag$arcs
##############################################
rmbde4<-RM2(algoritmo="tabu",datos=red4,typescore="bde")
rmbde4$score
rmbde4$tiempo
plot(rmbde4$dag, radius = 240)
rmbde4$dag$arcs
##############################################
rmlogv4<-RM2(algoritmo="tabu",datos=red4,typescore="loglik")
rmlogv4$score
rmlogv4$tiempo
plot(rmlogv4$dag, radius = 240)
rmlogv4$dag$arcs
#save(rmlogv2,file ="RMlogv2.RData")
###############################################
################### DEBN Restringido
#DERk24<-DEBNRest(N=100, dataset = red4, sctype="k2")
#save(DERk24,file="DEBNRESk24.RData")
#rm(DERk24)
load("DEBNRESk24.RData")
DERk24$maxS
DERk24$tiempo
gk24=model2network(Transforma(DERk24$opdag))
plot(gk24,radius = 240)
cuentaarcos(m1=gk24$arcs,m2=dag4$arcs)
#DERbde4<-DEBNRest(N=100, dataset = red4, sctype="bde")
#save(DERbde4,file="DEBNRESbde4.RData")
#rm(DERbde4)
load("DEBNRESbde4.RData")
DERbde4$maxS
DERbde4$tiempo
gbde4=model2network(Transforma(DERbde4$opdag))
plot(gbde4,radius = 240)
cuentaarcos(m1=gbde4$arcs,m2=dag4$arcs)
#DERlogv4<-DEBNRest(N=100, dataset = red4, sctype="loglik")
#save(DERlogv4,file="DEBNRESlogv4.RData")
#rm(DERlogv4)
load("DEBNRESlogv4.RData")
DERlogv4$maxS
DERlogv4$tiempo
glogv4=model2network(Transforma(DERlogv4$opdag))
plot(glogv4,radius = 240)
cuentaarcos(m1=glogv4$arcs,m2=dag4$arcs)
##############################################################
##################     Red bayesiana 5      ##################
##############################################################
load("Red5.RData")
source("Descomposicion.R")
#head(red5)
colnames(red5)<-c( "1", "2", "3", "4", "5", "6", "7", "8", "9","10","11","12","13","14","15","16","17","18","19","20",
                  "21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40",
                  "41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60",
                  "61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80",
                  "81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100")

a="[1][2][3][4][5][6][7][8][9][10][11][12][13][14][15][16][17|1][18|1:2][19|2:3][20|3:4][21|4:5][22|5:6][23|6:7][24|7:8][25|8:9][26|9:10][27|10:11][28|11:12][29|12:13][30|13:14][31|14:15][32|15:16][33|16][34|16][35|17][36|17:18][37|18:19][38|19:20][39|20:21][40|21:22][41|22:23][42|23:24][43|24:25][44|25:26][45|26:27][46|27:28][47|28:29][48|29:30][49|30:31][50|31:32][51|32:33][52|33:34][53|34][54|34][55|35][56|35:36][57|36:37][58|37:38][59|38:39][60|39:40][61|40:41][62|41:42][63|42:43][64|43:44][65|44:45][66|45:46][67|46:47][68|47:48][69|48:49][70|49:50][71|50:51][72|51:52][73|52:53][74|53:54][75|54][76|54][77|55][78|55:56][79|56:57][80|57:58][81|58:59][82|59:60][83|60:61][84|61:62][85|62:63][86|63:64][87|64:65][88|65:66][89|66:67][90|67:68][91|68:69][92|69:70][93|70:71][94|71:72][95|72:73][96|73:74][97|74:75][98|75:76][99|76][100|80:96]"
dag5 = model2network(a) #157 arcos
plot(dag5)
dag5$arcs
#"[1][2][3][4][5][6][7][8][9][10][11][12][13][14][15][16]"
#"[17|1][18|1:2][19|2:3][20|3:4][21|4:5][22|5:6][23|6:7][24|7:8][25|8:9][26|9:10][27|10:11][28|11:12][29|12:13][30|13:14][31|14:15][32|15:16][33|16][34|16]"
#"[35|17][36|17:18][37|18:19][38|19:20][39|20:21][40|21:22][41|22:23][42|23:24][43|24:25][44|25:26][45|26:27][46|27:28][47|28:29][48|29:30][49|30:31][50|31:32][51|32:33][52|33:34][53|34][54|34]"
#"[55|35][56|35:36][57|36:37][58|37:38][59|38:39][60|39:40][61|40:41][62|41:42][63|42:43][64|43:44][65|44:45][66|45:46][67|46:47][68|47:48][69|48:49][70|49:50][71|50:51][72|51:52][73|52:53][74|53:54][75|54][76|54]"
#"[77|55][78|55:56][79|56:57][80|57:58][81|58:59][82|59:60][83|60:61][84|61:62][85|62:63][86|63:64][87|64:65][88|65:66][89|66:67][90|67:68][91|68:69][92|69:70][93|70:71][94|71:72][95|72:73][96|73:74][97|74:75][98|75:76][99|76][100|76]"
idag5<-bn2igraph(dag5)  #idag esta en formato de red de igraph 
V(idag5)
#ver=V(idag5)
ver=c(1, 10, 100, 11, 12, 13, 14, 15, 16, 17, 18, 19, 2, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 3,
      30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 4, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 5, 50, 51,
      52, 53, 54, 55, 56, 57, 58, 59, 6, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 7, 70, 71, 72, 73,
      74, 75, 76, 77, 78, 79, 8, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 9, 90, 91, 92, 93, 94, 95,
      96, 97, 98, 99)
nivel<-EncuentraNivel(ve=ver) #la posiciÃ³n, de arriba a abajo, segun el nombre del vertice

pos<-c(1, 10, 1, 11, 12, 13, 14, 15, 16, 1, 2, 3, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 3, 14, 15, 
       16, 17, 18, 1, 2, 3, 4, 5, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 5, 16, 17, 18, 19, 20, 1,
       2, 3, 4, 5, 6, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 7, 16, 17, 18, 19, 20, 21, 22, 1, 2, 3,
       8, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 9, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)

#V(idag5) plot(idag5)
plotired(pos=pos,nivel=nivel,idag=idag5)


score(dag5,data = red5, type = "k2")

bnlearn::score(dag5,data = red5, type = "bde") 
score(dag5,data = red5, type = "loglik")
dag5$arcs
################### DEBN 
################### K2 DEBN
DEk25<-DEBN(N=100, dataset = red5, sctype="k2")
#save(DEk25,file="DEBNk25.RData")
#rm(DEk25)
load("DEBNk25.RData")
DEk25$maxS
DEk25$tiempo
gDEk25<-model2network(Transforma(DEk25$opdag))
plot(gDEk25,radius = 240)
gDEk25$arcs
cuentaarcos(m1=gDEk25$arcs,m2=dag5$arcs)
9743-nrow(gDEk25$arcs)+2
################### K2 DEBN Rest
#DERk25<-DEBNRest(N=100, dataset = red5, sctype="k2")
#save(DERk25,file="DEBNRESk25.RData")
#rm(DERk25)
load("DEBNRESk25.RData")
DERk25$maxS
DERk25$tiempo
gDERk25<-model2network(Transforma(DERk25$opdag))
plot(gDERk25,radius = 240)
gDERk25$arcs
cuentaarcos(m1=gDERk25$arcs,m2=dag5$arcs)
9743-nrow(gDERk25$arcs)+2
################################################
#DEbde5<-DEBN(N=100, dataset = red5, sctype="bde")
#save(DEbde5,file="DEBNbde5.RData")
#rm(DEbde5)
load("DEBNbde5.RData")
DEbde5$maxS
DEbde5$tiempo
gDEbde5<-model2network(Transforma(DEbde5$opdag))
plot(gDEbde5,radius = 240)
gDEbde5$arcs
cuentaarcos(m1=gDEbde5$arcs,m2=dag5$arcs)
9743-nrow(gDEbde5$arcs)
################### bde DEBN Rest
#DERbde5<-DEBNRest(N=100, dataset = red5, sctype="bde")
#save(DERbde5,file="DEBNRESbde5.RData")
#rm(DERbde5)
load("DEBNRESbde5.RData")
DERbde5$maxS
DERbde5$tiempo
gDERbde5<-model2network(Transforma(DERbde5$opdag))
plot(gDERbde5,radius = 240)
gDERbde5$arcs
cuentaarcos(m1=gDERbde5$arcs,m2=dag5$arcs)
9743-nrow(gDERbde5$arcs)+2
################################################
#DElogv5<-DEBN(N=100, dataset = red5, sctype="loglik")
#save(DElogv5,file="DEBNlogv5.RData")
#rm(DElogv5)
load("DEBNlogv5.RData")
DElogv5$maxS
DElogv5$tiempo
gDElogv5<-model2network(Transforma(DElogv5$opdag))
plot(gDElogv5,radius = 240)
cuentaarcos(m1=gDElogv5$arcs,m2=dag5$arcs)
9743-nrow(gDElogv5$arcs)+3
################### logv DEBN Rest
#DERlogv5<-DEBNRest(N=100, dataset = red5, sctype="loglik")
#save(DERlogv5,file="DEBNRESlogv5.RData")
#rm(DERlogv5)
load("DEBNRESlogv5.RData")
DERlogv5$maxS
DERlogv5$tiempo
gDERlogv5<-model2network(Transforma(DERlogv5$opdag))
plot(gDERlogv5,radius = 240)
gDERlogv5$arcs
cuentaarcos(m1=gDERlogv5$arcs,m2=dag5$arcs)
9743-nrow(gDERlogv5$arcs)+6
################### scores hc
#hck25<-HC(datos=red5,typescore="k2")
#save(hck25,file="HCk25.RData")
#rm(hck25)
load("HCk25.RData")
hck25$score
hck25$tiempo
plot(hck25$dag, radius = 240)
cuentaarcos(m1=hck25$dag$arcs,m2=dag5$arcs)
9743-nrow(hck25$dag$arcs)+4
################################################
#hcbde5<-HC(datos=red5,typescore="bde")
#save(hcbde5,file="HCbde5.RData")
#rm(hcbde5)
load("HCbde5.RData")
hcbde5$score
hcbde5$tiempo
plot(hcbde5$dag, radius = 240)
cuentaarcos(m1=hcbde5$dag$arcs,m2=dag5$arcs)
9743-nrow(hcbde5$dag$arcs)+1
################################################
#hclogv5<-HC(datos=red5,typescore="loglik")
#save(hclogv5,file="HClogv5.RData")
#rm(hclogv5)
load("HClogv5.RData")
hclogv5$score
hclogv5$tiempo
plot(hclogv5$dag, radius = 240)
cuentaarcos(m1=hclogv5$dag$arcs,m2=dag5$arcs)
9743-nrow(hclogv5$dag$arcs)+27
##############################################
tak25<-TABU(datos=red5,typescore="k2")
tak25$score
tak25$tiempo
plot(tak25$dag, radius = 240) 

cuentaarcos(m1=tak25$dag$arcs,m2=dag5$arcs)
9743-nrow(tak25$dag$arcs)+4
##############################################
tabde5<-TABU(datos=red5,typescore="bde")
tabde5$score
tabde5$tiempo
plot(tabde5$dag, radius = 240)

cuentaarcos(m1=tabde5$dag$arcs,m2=dag5$arcs)
9743-nrow(tabde5$dag$arcs)+1
##############################################
talogv5<-TABU(datos=red5,typescore="loglik")
talogv5$score
talogv5$tiempo
plot(talogv5$dag, radius = 240)

cuentaarcos(m1=talogv5$dag$arcs,m2=dag5$arcs)
9743-nrow(talogv5$dag$arcs)+27
#mismo que hclogv3
##############################################
mmhck25<-MMHC(datos=red5,typescore="k2")
mmhck25$score
mmhck25$tiempo
plot(mmhck25$dag, radius = 240) 

cuentaarcos(m1=mmhck25$dag$arcs,m2=dag5$arcs)
9743-nrow(mmhck25$dag$arcs)+2
##############################################
mmhcbde5<-MMHC(datos=red5,typescore="bde")
mmhcbde5$score
mmhcbde5$tiempo
plot(mmhcbde5$dag, radius = 240)

cuentaarcos(m1=mmhcbde5$dag$arcs,m2=dag5$arcs)
9743-nrow(mmhcbde5$dag$arcs)+1
##############################################
mmhclogv5<-MMHC(datos=red5,typescore="loglik")
mmhclogv5$score
mmhclogv5$tiempo
plot(mmhclogv5$dag, radius = 240) 

cuentaarcos(m1=mmhclogv5$dag$arcs,m2=dag5$arcs)
9743-nrow(mmhclogv5$dag$arcs)+2

#save(mmhclogv2,file ="MMHClogv2.RData")
##############################################
rmk25<-RM2(algoritmo="tabu",datos=red5,typescore="k2")
rmk25$score
rmk25$tiempo
plot(rmk25$dag, radius = 240) 


cuentaarcos(m1=rmk25$dag$arcs,m2=dag5$arcs)
9743-nrow(rmk25$dag$arcs)+1
##############################################
rmbde5<-RM2(algoritmo="tabu",datos=red5,typescore="bde")
rmbde5$score
rmbde5$tiempo
plot(rmbde5$dag, radius = 240)

cuentaarcos(m1=rmbde5$dag$arcs,m2=dag5$arcs)
9743-nrow(rmbde5$dag$arcs)+1
##############################################
rmlogv5<-RM2(algoritmo="tabu",datos=red5,typescore="loglik")
rmlogv5$score
rmlogv5$tiempo
plot(rmlogv5$dag, radius = 240)


cuentaarcos(m1=rmlogv5$dag$arcs,m2=dag5$arcs)
9743-nrow(rmlogv5$dag$arcs)+1
#save(rmlogv2,file ="RMlogv2.RData")








##############################################################
##################     VE Red bayesiana 1      ###############
##############################################################
setwd("C:/Users/User/Dropbox/Gustavo")
par(mar=c(0,0,0,0))
rm(list=ls(all=TRUE))
source("Descomposicion.R")

load("Red1.RData")
colnames(red1)<-c("1","2","3","4","5","6")
probs1$PX1
probs1$PX2
probs1$PX3
probs1$PX4
probs1$PT
probs1$Ps
p12345r1

bn.mle1$`1`$prob
bn.mle1$`2`$prob
bn.mle1$`3`$prob
bn.mle1$`4`$prob
bn.mle1$`5`$prob
pE6DEk21
############         Valor Esperado red 1       ###############
E1r1 <-sum(probs1$PX1*IdentidadP(2))
E2r1 <-sum(probs1$PX2*IdentidadP(2))
E3r1 <-sum(probs1$PX3*IdentidadP(3))
E4r1 <-sum(probs1$PX4*IdentidadP(2))
E5r1 <-sum(probs1$PT*IdentidadP(3))
lr1=list(probs1$PX1,probs1$PX2,probs1$PX3,probs1$PX4,probs1$PT)
p12345r1<- multvec(lr1)
p6r1<-marginal(M=probs1$Ps,p=p12345r1) #P(V6=i) i=1,...,5
E6r1<-sum(p6r1*IdentidadP(5))
VE1<-c(E1r1,E2r1,E3r1,E4r1,E5r1,E6r1)
#########       Valor Esperado Estimado DEBNk21  ###############
load("DEBNk21.RData")
g1<-model2network(Transforma(DEk21$opdag))
bn.mle1 <- bn.fit(g1, data = red1, method = "mle")
#plot(g1,radius = 240)
# EE1DEk21= val esperado estimado DEBNk21
EE1DEk21<-sum(bn.mle1$`1`$prob*IdentidadP(2))
EE2DEk21<-sum(bn.mle1$`2`$prob*IdentidadP(2))
EE3DEk21<-sum(bn.mle1$`3`$prob*IdentidadP(3))
EE4DEk21<-sum(bn.mle1$`4`$prob*IdentidadP(2))
EE5DEk21<-sum(bn.mle1$`5`$prob*IdentidadP(3))
pE6DEk21<-marginal(M=bn.mle1$`6`$prob,p=bn.mle1$`4`$prob)
EE6DEk21<-sum(pE6DEk21*IdentidadP(5))
VEEDEk21<-c(EE1DEk21,EE2DEk21,EE3DEk21,EE4DEk21,EE5DEk21,EE6DEk21)
VEE1<-VEEDEk21
#########       Valor Esperado Estimado DEBNbde1  ###############
rm(bn.mle1)
load("DEBNbde1.RData")
g2<-model2network(Transforma(DEbde1$opdag))
#plot(g2,radius = 240)
bn.mle2 <- bn.fit(g2, data = red1, method = "mle")
pE1DEbde1<-marginal(M=bn.mle2$`1`$prob,p=bn.mle2$`2`$prob)
EE1DEbde1<-sum(pE1DEbde1*IdentidadP(2))
EE2DEbde1<-sum(bn.mle2$`2`$prob*IdentidadP(2))
EE3DEbde1<-sum(bn.mle2$`3`$prob*IdentidadP(3))
EE4DEbde1<-sum(bn.mle2$`4`$prob*IdentidadP(2))
EE5DEbde1<-sum(bn.mle2$`5`$prob*IdentidadP(3))
EE6DEbde1<-sum(bn.mle2$`6`$prob*IdentidadP(5))
VEEDEbde1<-c(EE1DEbde1,EE2DEbde1,EE3DEbde1,EE4DEbde1,EE5DEbde1,EE6DEbde1)
#########       Valor Esperado Estimado DEBNlogv1  ###############
rm(bn.mle2)
load("DEBNlogv1.RData")
g3<-model2network(Transforma(DElogv1$opdag))
#plot(g3,radius = 240)
bn.mle3 <- bn.fit(g3, data = red1, method = "mle")
pE1DElogv1<-marginal(M=bn.mle3$`1`$prob,p=bn.mle3$`4`$prob)
EE1DElogv1<-sum(pE1DElogv1*IdentidadP(2))
pE2DElogv1<-marginal(M=bn.mle3$`2`$prob,p=bn.mle3$`4`$prob)
EE2DElogv1<-sum(pE2DElogv1*IdentidadP(2))
EE3DElogv1<-sum(bn.mle3$`3`$prob*IdentidadP(3))
EE4DElogv1<-sum(bn.mle3$`4`$prob*IdentidadP(2))
EE5DElogv1<-sum(bn.mle3$`5`$prob*IdentidadP(3))
load("HCk21")

g4<-model2network("[1][2][3][4][5][6]")
plot(g4)
bn.mle4 <- bn.fit(g4, data = red1, method = "mle")
sum(bn.mle4$`1`$prob*IdentidadP(2))
##############################################################
##################     VE Red bayesiana 2      ###############
##############################################################

par(mar=c(0,0,0,0))
rm(list=ls(all=TRUE))
source("Descomposicion.R")

load("Red2.RData")

colnames(red2)<-c("1","2","3","4","5","6")

############         Valor Esperado red 2       ###############
E1r2 <-sum(probs2$PX1*IdentidadP(2))
E2r2 <-sum(probs2$PX2*IdentidadP(2))
lr2=list(probs2$PX1,probs2$PX2,probs2$PT)
p125r2<- multvec(lr2)
p3r2<-marginal(M=probs2$PX3,p=p125r2)
E3r2<-sum(p3r2*IdentidadP(3))
l4r2=list(probs2$PX1,probs2$PX2)
p12r2<- multvec(l4r2)
p4r2<-marginal(M=probs2$PX4,p=p12r2)
E4r2<-sum(p4r2*IdentidadP(2))
E5r2<-sum(probs2$PT*IdentidadP(3))
l6r2<-list(p3r2,p4r2)
p34r2<- multvec(l6r2)
p6r2<-marginal(M=probs2$Ps,p=p34r2)
E6r2<-sum(p6r2*IdentidadP(5))

VE2<-c(E1r2,E2r2,E3r2,E4r2,E5r2,E6r2)

#########       Valor Esperado Estimado   ###############
load("HCk22.RData")
g2<-model2network("[1][2][3][4][5][6]")
bn.mle2 <- bn.fit(g2, data = red2, method = "mle")
#plot(g2,radius = 240)
# EE1DEk21= val esperado estimado DEBNk21
EE1r2<-sum(bn.mle2$`1`$prob*IdentidadP(2))
EE2r2<-sum(bn.mle2$`2`$prob*IdentidadP(2))
EE3r2<-sum(bn.mle2$`3`$prob*IdentidadP(3))
EE4r2<-sum(bn.mle2$`4`$prob*IdentidadP(2))
EE5r2<-sum(bn.mle2$`5`$prob*IdentidadP(3))
EE6r2<-sum(bn.mle2$`6`$prob*IdentidadP(5))
VEE2<-c(EE1r2,EE2r2,EE3r2,EE4r2,EE5r2,EE6r2)

##############################################################
##################     VE Red bayesiana 3      ###############
##############################################################

par(mar=c(0,0,0,0))
rm(list=ls(all=TRUE))
source("Descomposicion.R")

load("Red3.RData")

colnames(red3)<-c("1","2","3","4","5")

############         Valor Esperado red 3       ###############
E1r3 <-sum(probs3$PX1*IdentidadP(2))
E2r3 <-sum(probs3$PX2*IdentidadP(2))
l3r3=list(probs3$PX1,probs3$PX2)
p12r3<- multvec(l3r3)
p3r3<-marginal(M=probs3$PX3,p=p12r3)
E3r3<-sum(p3r3*IdentidadP(3))
l4r3=list(probs3$PX1,probs3$PX2,p3r3)
p123r3<- multvec(l4r3)
p4r3<-marginal(M=probs3$PX4,p=p123r3)
E4r3<-sum(p4r3*IdentidadP(3))
p5r3<-marginal(M=probs3$PX5,p=p4r3)
E5r3<-sum(p5r3*IdentidadP(4))
VE3<-c(E1r3,E2r3,E3r3,E4r3,E5r3)

#########       Valor Esperado Estimado   ###############
load("HCk23.RData")
g3<-model2network("[1][2][3][4][5]")
bn.mle3 <- bn.fit(g3, data = red3, method = "mle")
#plot(g3,radius = 240)
# EE1DEk21= val esperado estimado DEBNk21
EE1r3<-sum(bn.mle3$`1`$prob*IdentidadP(2))
EE2r3<-sum(bn.mle3$`2`$prob*IdentidadP(2))
EE3r3<-sum(bn.mle3$`3`$prob*IdentidadP(3))
EE4r3<-sum(bn.mle3$`4`$prob*IdentidadP(3))
EE5r3<-sum(bn.mle3$`5`$prob*IdentidadP(4))

VEE3<-c(EE1r3,EE2r3,EE3r3,EE4r3,EE5r3)

##############################################################
##################     VE Red bayesiana 4      ###############
##############################################################

par(mar=c(0,0,0,0))
rm(list=ls(all=TRUE))
source("Descomposicion.R")

load("Red4.RData")
colnames(red4)<-c("1","2","3","4","5","6","7","8","9","10",
                  "11","12","13","14","15","16","17","18","19","20")

############         Valor Esperado red 4       ###############
E1r4 <-sum(probs4$PX1*IdentidadP(2))
p2r4<-marginal(M=probs4$PX2,p=probs4$PX1)
E2r4 <-sum(p2r4*IdentidadP(2))
p3r4<-marginal(M=probs4$PX3,p=p2r4)
E3r4 <-sum(p3r4*IdentidadP(2))
p4r4<-marginal(M=probs4$PX4,p=probs4$PX1)
E4r4 <-sum(p5r4*IdentidadP(2))
p5r4<-marginal(M=probs4$PX5,p=probs4$PX1)
E5r4 <-sum(p5r4*IdentidadP(2))
p6r4<-marginal(M=probs4$PX6,p=p2r4)
E6r4 <-sum(p6r4*IdentidadP(2))
p7r4<-marginal(M=probs4$PX7,p=p3r4)
E7r4 <-sum(p7r4*IdentidadP(2))
p8r4<-marginal(M=probs4$PX8,p=probs4$PX1)
E8r4 <-sum(p8r4*IdentidadP(2))
p9r4<-marginal(M=probs4$PX9,p=p5r4)
E9r4 <-sum(p9r4*IdentidadP(2))
p10r4<-marginal(M=probs4$PX10,p=p6r4)
E10r4 <-sum(p10r4*IdentidadP(2))
p57r4<-multvec(list(p5r4,p7r4))
p11r4<-marginal(M=probs4$PX11,p=p57r4)
E11r4 <-sum(p11r4*IdentidadP(2))
p12r4<-marginal(M=probs4$PX12,p=p6r4)
E12r4 <-sum(p12r4*IdentidadP(2))
p13r4<-marginal(M=probs4$PX13,p=p7r4)
E13r4 <-sum(p13r4*IdentidadP(2))
p910r4<-multvec(list(p9r4,p10r4))
p14r4<-marginal(M=probs4$PX14,p=p910r4)
E14r4 <-sum(p14r4*IdentidadP(2))
p1114r4<-multvec(list(p11r4,p14r4))
p15r4<-marginal(M=probs4$PX15,p=p1114r4)
E15r4 <-sum(p15r4*IdentidadP(2))
p121315r4<-multvec(list(p12r4,p13r4,p15r4))
p16r4<-marginal(M=probs4$PX16,p=p121315r4)
E16r4 <-sum(p16r4*IdentidadP(2))
p17r4<-marginal(M=probs4$PX17,p=p14r4)
E17r4 <-sum(p17r4*IdentidadP(2))
p18r4<-marginal(M=probs4$PX18,p=p15r4)
E18r4 <-sum(p18r4*IdentidadP(2))
p19r4<-marginal(M=probs4$PX19,p=p16r4)
E19r4 <-sum(p19r4*IdentidadP(2))
p171819r4<-multvec(list(p17r4,p18r4,p19r4))
p20r4<-marginal(M=probs4$PX20,p=p171819r4)
E20r4 <-sum(p20r4*IdentidadP(2))
VE4<-c(E1r4,E2r4,E3r4,E4r4,E5r4,E6r4,E7r4,E8r4,E9r4,E10r4,
       E11r4,E12r4,E13r4,E14r4,E15r4,E16r4,E17r4,E18r4,E19r4,E20r4)
#########       Valor Esperado Estimado   ###############
load("HCbde4.RData")

g4<-model2network("[1][2][3][4][5][6][7][8][9][10][11][13][14][15][16][17][18][19][20][12|6]")
bn.mle4 <- bn.fit(g4, data = red4, method = "mle")
#plot(g4,radius = 240)
# EE1DEk21= val esperado estimado DEBNk21
EE1r4<-sum(bn.mle4$`1`$prob*IdentidadP(2))
EE2r4<-sum(bn.mle4$`2`$prob*IdentidadP(2))
EE3r4<-sum(bn.mle4$`3`$prob*IdentidadP(2))
EE4r4<-sum(bn.mle4$`4`$prob*IdentidadP(2))
EE5r4<-sum(bn.mle4$`5`$prob*IdentidadP(2))
EE6r4<-sum(bn.mle4$`6`$prob*IdentidadP(2))
EE7r4<-sum(bn.mle4$`7`$prob*IdentidadP(2))
EE8r4<-sum(bn.mle4$`8`$prob*IdentidadP(2))
EE9r4<-sum(bn.mle4$`9`$prob*IdentidadP(2))
EE10r4<-sum(bn.mle4$`10`$prob*IdentidadP(2))
EE11r4<-sum(bn.mle4$`11`$prob*IdentidadP(2))
pE12r4<-marginal(M=bn.mle4$`12`$prob,p=bn.mle4$`6`$prob)
EE12r4 <-sum(pE12r4*IdentidadP(2))
EE13r4<-sum(bn.mle4$`13`$prob*IdentidadP(2))
EE14r4<-sum(bn.mle4$`14`$prob*IdentidadP(2))
EE15r4<-sum(bn.mle4$`15`$prob*IdentidadP(2))
EE16r4<-sum(bn.mle4$`16`$prob*IdentidadP(2))
EE17r4<-sum(bn.mle4$`17`$prob*IdentidadP(2))
EE18r4<-sum(bn.mle4$`18`$prob*IdentidadP(2))
EE19r4<-sum(bn.mle4$`19`$prob*IdentidadP(2))
EE20r4<-sum(bn.mle4$`20`$prob*IdentidadP(2))


VEE4<-c(EE1r4,EE2r4,EE3r4,EE4r4,EE5r4,EE6r4,EE7r4,EE8r4,EE9r4,EE10r4,
        EE11r4,EE12r4,EE13r4,EE14r4,EE15r4,EE16r4,EE17r4,EE18r4,EE19r4,EE20r4)

###################       ECM          ##################################

#save(VE1, VEE1, VE2, VEE2, VE3, VEE3, VE4, VEE4,file="VEsp.RData")
#source("Descomposicion.R")

load("Red1.RData")
load("Red2.RData")
load("Red3.RData")
load("Red4.RData")
load("VEsp.RData")

head(red1)
colnames(red1)<-c("1","2","3","4","5","6")
red1<-as.numeric(red1)
ev1r1<-(red1$`1`-VEE1[1])^2
#############################################################################
####################      Plots de scores        ############################
#############################################################################
setwd("C:/Users/Gustavo/Dropbox/Gustavo")
####################           red 1            ############################
#par(mfrow=c(1,2))
red1<-read.csv(file="r1.csv", header=TRUE, sep=",")
x<-c(1,2,3,4,5,6,7)
puntaje1=red1$Score
pos1=c(1,4,5,6,7)
pos1c=c(2,3)
pos2=c(8,11,12,13,14)
pos2c=c(9,10)
pos3=c(15,18,19,20,21)
pos3c=c(16,17)
k2y1<-puntaje1[c(pos1,pos1c)]
bdey1<-puntaje1[c(pos2,pos2c)]
logvy1<-puntaje1[c(pos3,pos3c)]

plot(x,k2y1,type="b", col="orange",ylim=c(min(red1$Score),max(red1$Score)), xlab="Tipos de GAD", 
     ylab="Puntuación",pch=19,lwd=2, main="Red bayesiana 1",xaxt="n",cex=2)

lines(x,bdey1, col="royalblue4", type = "b",pch=18,lwd=2,cex=2)
lines(x,logvy1, col="seagreen4", type = "b",pch=17,lwd=2,cex=2)
legend("bottomright", legend = c("k2", "Bde", "logVer"), col = c("orange","royalblue4","seagreen4"), 
       pch = c(19,18,17), bty = "n", cex = 2, text.col = "black", horiz = F )
axis(1,at=1:7,labels =c("Real", "Hc", "Tabu", "Mmhc", "Rm2-Tabu", "DEBN","DEBN Rest"))

####################           red 2            ############################
#rm(red1)
red2<-read.csv(file="r2.csv", header=TRUE, sep=",")
puntaje2=red2$Score
k2y2<-puntaje2[c(pos1,pos1c)]
bdey2<-puntaje2[c(pos2,pos2c)]
logvy2<-puntaje2[c(pos3,pos3c)]

plot(x,k2y2,type="b", col="orange",ylim=c(min(red2$Score)-200,max(red2$Score)), xlab="Tipos de GAD", 
     ylab="Puntuación",pch=19,lwd=2, main="Red bayesiana 2",xaxt="n",cex=2)

lines(x,bdey2, col="royalblue4", type = "b",pch=18,lwd=2,cex=2)
lines(x,logvy2, col="seagreen4", type = "b",pch=17,lwd=2,cex=2)
legend("bottomright", legend = c("k2", "Bde", "logVer"), col = c("orange","royalblue4","seagreen4"), 
       pch = c(19,18,17), bty = "n", cex = 2, text.col = "black", horiz = F )
axis(1,at=1:7,labels =c("Real", "Hc", "Tabu", "Mmhc", "Rm2-Tabu", "DEBN","DEBN Rest"))
####################           red 3            ############################
#rm(red2)
red3<-read.csv(file="r3.csv", header=TRUE, sep=",")
puntaje3=red3$Score
k2y3<-puntaje3[c(pos1,pos1c)]
bdey3<-puntaje3[c(pos2,pos2c)]
logvy3<-puntaje3[c(pos3,pos3c)]

plot(x,k2y3,type="b", col="orange",ylim=c(min(red3$Score),max(red3$Score)), xlab="Tipos de GAD", 
     ylab="Puntuación",pch=19,lwd=2, main="Red bayesiana 3",xaxt="n",cex=2)

lines(x,bdey3, col="royalblue4", type = "b",pch=18,lwd=2,cex=2)
lines(x,logvy3, col="seagreen4", type = "b",pch=17,lwd=2,cex=2)
legend("bottomright", legend = c("k2", "Bde", "logVer"), col = c("orange","royalblue4","seagreen4"), 
       pch = c(19,18,17), bty = "n", cex = 2, text.col = "black", horiz = F )
axis(1,at=1:7,labels =c("Real", "Hc", "Tabu", "Mmhc", "Rm2-Tabu", "DEBN","DEBN Rest"))
####################           red 4            ############################
#rm(red3)
red4<-read.csv(file="r4.csv", header=TRUE, sep=",")
puntaje4=red4$Score
k2y4<-puntaje4[c(pos1,pos1c)]
bdey4<-puntaje4[c(pos2,pos2c)]
logvy4<-puntaje4[c(pos3,pos3c)]

plot(x,k2y4,type="b", col="orange",ylim=c(min(red4$Score),max(red4$Score)), xlab="Tipos de GAD", 
     ylab="Puntuación",pch=19,lwd=2, main="Red bayesiana 4",xaxt="n",cex=2)

lines(x,bdey4, col="royalblue4", type = "b",pch=18,lwd=2,cex=2)
lines(x,logvy4, col="seagreen4", type = "b",pch=17,lwd=2,cex=2)
legend("topright", legend = c("k2", "Bde", "logVer"), col = c("orange","royalblue4","seagreen4"), 
       pch = c(19,18,17), bty = "n", cex = 2, text.col = "black", horiz = F )
axis(1,at=1:7,labels =c("Real", "Hc", "Tabu", "Mmhc", "Rm2-Tabu", "DEBN","DEBN Rest"))
####################           red 5            ############################
#rm(red4)
red5<-read.csv(file="r5.csv", header=TRUE, sep=",")
puntaje5=red5$Score
k2y5<-puntaje5[c(pos1,pos1c)]
bdey5<-puntaje5[c(pos2,pos2c)]
logvy5<-puntaje5[c(pos3,pos3c)]

plot(x,k2y5,type="b", col="orange",ylim=c(min(red5$Score),max(red5$Score)), xlab="Tipos de GAD", 
     ylab="Puntuación",pch=19,lwd=2, main="Red bayesiana 5",xaxt="n",cex=2)

lines(x,bdey5, col="royalblue4", type = "b",pch=18,lwd=2,cex=2)
lines(x,logvy5, col="seagreen4", type = "b",pch=17,lwd=2,cex=2)
legend("topright", legend = c("k2", "Bde", "logVer"), col = c("orange","royalblue4","seagreen4"), 
       pch = c(19,18,17), bty = "n", cex = 2, text.col = "black", horiz = F )
axis(1,at=1:7,labels =c("Real", "Hc", "Tabu", "Mmhc", "Rm2-Tabu", "DEBN","DEBN Rest"))

#############################################################################
####################      Plots de los tiempos       ########################
#############################################################################
red1$Tiempo
posreal<-which(red1$Tiempo=="-")
tiempo1=red1$Tiempo[-posreal]
tiempo1<-c(1.032, 1.76,  0.005, 0.034, 0.016, 0.001, 1.218, 1.84,  0.009, 0.028,
           0.012, 0.007, 1.04,  1.04,  0.035, 0.024, 0.003, 0.002)
etiqueta1<-c(1.032, 1.76,  0.005, 0.034, 0.46, 0.001, 1.218, 1.84,  0.009, 0.38,
             0.012, 0.33, 1.04,  1.34,  0.035, 0.34, 0.003, 0.42)
length(tiempo1)
plot(1:18, tiempo1, type = "b",xlab="Tipos de GAD", ylab="Tiempo en segundos",pch=19,lwd=2, 
     main="Red bayesiana 1", col="seagreen4",ylim=c(0,5),xaxt="n")
axis(1,at=1:18)
names<-as.character(red1$Tipos.de.scores[-posreal])
text(1:18, etiqueta1, labels=names, cex= 0.7, pos = 3)

#################4
red4$Tiempo
tiempo4=red4$Tiempo[-posreal]
tiempo4<-c(4.051,  6.56,   0.022, 0.044,  0.031,  0.021,  4.535,  6.68,   0.006, 0.002,
           0.018,  0.016,  3.758,  6.31, 12.769, 6.279,  0.019,  0.189)
etiqueta4<-c(4.051,  6.56,   0.022, 0.044,  0.29,  0.021,  4.535,  6.68,   0.006, 0.32,
             0.018,  0.26,  3.758,  6.31, 12.769, 6.279,  0.019,  0.29)
length(tiempo4)
plot(1:18, tiempo4, type = "b",xlab="Tipos de GAD", ylab="Tiempo en segundos",pch=19,lwd=2, 
     main="Red bayesiana 4", col="seagreen4",ylim=c(0,max(tiempo4)+1),xaxt="n")
axis(1,at=1:18)
names<-as.character(red1$Tipos.de.scores[-posreal])
text(1:18, etiqueta4, labels=names, cex= 0.7, pos = 3)
#################5
red5$Tiempo
tiempo5=red5$Tiempo[-posreal]
tiempo5<-c(123.96, 126.04, 1.02, 0.806, 0.49, 0.189,  130.2, 131.52, 0.169, 0.227,   
           0.366, 0.153, 130.6, 127.206, 435.48,  457.86, 0.36, 0.242)
etiqueta5<-c(123.96, 146.04, 1.02, 0.806, 15.49, 0.189,  147.2, 131.52, 0.169, 0.227,   
             22.366, 0.153, 150.6, 127.206, 435.48,  457.86, 0.36, 20.242)
length(tiempo5)
plot(1:18, tiempo5, type = "b",xlab="Tipos de GAD", ylab="Tiempo en segundos",pch=19,lwd=2, 
     main="Red bayesiana 5", col="seagreen4",ylim=c(0,max(tiempo5)+10),xaxt="n")
axis(1,at=1:18)
names<-as.character(red1$Tipos.de.scores[-posreal])
text(1:18, etiqueta5, labels=names, cex= 0.7, pos = 3)

############################################################################
############################ Red bayesiana Rossi ###########################
############################################################################

rm(list=ls())
setwd("C:/Users/Gustavo/Dropbox/Gustavo")
source("Descomposicion.R")
library(survival)
library(KMsurv)
library(snow)
library(snowfall)
library(GlobalDeviance)
#library(prodlim)
data(Rossi)
head(Rossi)
#summary(Rossi)
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

dag6 = model2network("[1][2][3][4][5][6|1:2:3:4:5]")
plot(dag6) # fin=1,race=2,wexp=3,prio=4,week=5, arrest=6

idag6=bn2igraph(dag6)
plot(idag6,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idag6)$media, vertex.label.color="black",vertex.size=25)
tkplot(idag6)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
l<- tkplot.getcoords(5)
plot(idag6,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idag6)$media, vertex.label.color="black",vertex.size=25, layout=l)


################### Score's del grafo real
score(dag6,data = datosRossi, type = "k2") #-1798.562
score(dag6,data = datosRossi, type = "bde") #-1865.484
score(dag6,data = datosRossi, type = "loglik") #=-1592.284
################### DEBN 
#DEk26<-DEBN(N=100, dataset = datosRossi, sctype="k2")
#save(DEk26,file="DEBNk26.RData")
#rm(DEk26)
load("DEBNk26.RData")
DEk26$maxS
DEk26$tiempo
plot(model2network(Transforma(DEk26$opdag)),radius = 240)
################################################
#DEbde6<-DEBN(N=100, dataset = datosRossi, sctype="bde")
#save(DEbde6,file="DEBNbde6.RData")
#rm(DEbde6)
load("DEBNbde6.RData")
DEbde6$maxS
DEbde6$tiempo
plot(model2network(Transforma(DEbde6$opdag)),radius = 240)
################################################
#DElogv6<-DEBN(N=100, dataset = datosRossi, sctype="loglik")
#save(DElogv6,file="DEBNlogv6.RData")
#rm(DElogv6)
load("DEBNlogv6.RData")
DElogv6$maxS
DElogv6$tiempo
plot(model2network(Transforma(DElogv6$opdag)),radius = 240)
dagDElogv6=model2network(Transforma(DElogv6$opdag))
idagDElogv6=bn2igraph(dagDElogv6)
plot(idagDElogv6,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDElogv6)$media, vertex.label.color="black",vertex.size=25)
tkplot(idagDElogv6)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
################### scores hc
#hck26<-HC(datos=datosRossi,typescore="k2")
#save(hck26,file="HCk26.RData")
#rm(hck26)
load("HCk26.RData")
hck26$score
hck26$tiempo
plot(hck26$dag, radius = 240)
################################################
#hcbde6<-HC(datos=datosRossi,typescore="bde")
#save(hcbde6,file="HCbde6.RData")
#rm(hcbde6)
load("HCbde6.RData")
hcbde6$score
hcbde6$tiempo
plot(hcbde6$dag, radius = 240)
################################################
#hclogv6<-HC(datos=datosRossi,typescore="loglik")
#save(hclogv6,file="HClogv6.RData")
#rm(hclogv6)
load("HClogv6.RData")
hclogv6$score
hclogv6$tiempo
plot(hclogv6$dag, radius = 240)


idaghclogv6=bn2igraph(hclogv6$dag)
plot(idaghclogv6,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idaghclogv6)$media, vertex.label.color="black",vertex.size=25)
tkplot(idaghclogv6)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):


##############################################
tak26<-TABU(datos=datosRossi,typescore="k2")
tak26$score
tak26$tiempo
plot(tak26$dag, radius = 240) 
##############################################
tabde6<-TABU(datos=datosRossi,typescore="bde")
tabde6$score
tabde6$tiempo
plot(tabde6$dag, radius = 240)
##############################################
talogv6<-TABU(datos=datosRossi,typescore="loglik")
talogv6$score
talogv6$tiempo
plot(talogv6$dag, radius = 240)
#mismo que hclogv3
idagtalogv6=bn2igraph(talogv6$dag)
plot(idagtalogv6,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagtalogv6)$media, vertex.label.color="black",vertex.size=25)
tkplot(idagtalogv6)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):

##############################################
mmhck26<-MMHC(datos=datosRossi,typescore="k2")
mmhck26$score
mmhck26$tiempo
plot(mmhck26$dag, radius = 240) 
##############################################
mmhcbde6<-MMHC(datos=datosRossi,typescore="bde")
mmhcbde6$score
mmhcbde6$tiempo
plot(mmhcbde6$dag, radius = 240) 
##############################################
mmhclogv6<-MMHC(datos=datosRossi,typescore="loglik")
mmhclogv6$score
mmhclogv6$tiempo
plot(mmhclogv6$dag, radius = 240) 
#save(mmhclogv2,file ="MMHClogv2.RData")
##############################################
rmk26<-RM2(algoritmo="tabu",datos=datosRossi,typescore="k2")
rmk26$score
rmk26$tiempo
plot(rmk26$dag, radius = 240) 
##############################################
rmbde6<-RM2(algoritmo="tabu",datos=datosRossi,typescore="bde")
rmbde6$score
rmbde6$tiempo
plot(rmbde6$dag, radius = 240)
##############################################
rmlogv6<-RM2(algoritmo="tabu",datos=datosRossi,typescore="loglik")
rmlogv6$score
rmlogv6$tiempo
plot(rmlogv6$dag, radius = 240)



###############################################
################### DEBN Restringido
#DERk26<-DEBNRest(N=100, dataset = datosRossi, sctype="k2")
#save(DERk26,file="DEBNRESk26.RData")
#rm(DERk26)
load("DEBNRESk26.RData")
DERk26$maxS
DERk26$tiempo
dagDEBNRESk26=model2network(Transforma(DERk26$opdag))
plot(dagDEBNRESk26,radius = 240)
idagDEBNRESk26=bn2igraph(dagDEBNRESk26)

plot(idagDEBNRESk26,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESk26)$media, vertex.label.color="black",vertex.size=25)



#DERbde6<-DEBNRest(N=100, dataset = datosRossi, sctype="bde")
#save(DERbde6,file="DEBNRESbde6.RData")
#rm(DERbde6)
load("DEBNRESbde6.RData")
DERbde6$maxS
DERbde6$tiempo
dagDEBNRESbde6=model2network(Transforma(DERbde6$opdag))
plot(dagDEBNRESbde6,radius = 240)
idagDEBNRESbde6=bn2igraph(dagDEBNRESbde6)

plot(idagDEBNRESbde6,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESbde6)$media, vertex.label.color="black",vertex.size=25)


load("DEBNRESlogv6.RData")
DERlogv6$maxS
DERlogv6$tiempo
dagDEBNRESlogv6=model2network(Transforma(DERlogv6$opdag))
plot(dagDEBNRESlogv6,radius = 240)
idagDEBNRESlogv6=bn2igraph(dagDEBNRESlogv6)

plot(idagDEBNRESlogv6,edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(idagDEBNRESlogv6)$media, vertex.label.color="black",vertex.size=25)

