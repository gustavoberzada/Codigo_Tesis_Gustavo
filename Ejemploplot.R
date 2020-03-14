install.packages("bnlearn")
library(bnlearn)
data("asia") # "asia" base de datos sint?ticos de Lauritzen y Spiegelhalter (1988) 
#                sobre enfermedades pulmonares (tuberculosis, c?ncer de pulm?n o
#                bronquitis) y visitas a Asia.
head(asia)
dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") # modelo real 
plot(dag) #plot del dag real
dag.learning<-hc(asia) #estructura aprendida a trav?s de Hill-Climbing
plot(dag.learning) #plot del dag aprendido po Hill-Climbing

install.packages("igraph")
require(igraph)

#install.packages("graph")

#Instrucciones de 
#http://www.bioconductor.org/packages/release/bioc/html/graph.html
#To install this package, start R (version "3.6") and enter:
if (!requireNamespace("BiocManager", quietly = TRUE)) 
  install.packages("BiocManager")
BiocManager::install("graph")
require(graph)



bn2igraph<-function(dag){ #function para convertir un dag en formato bn (bnlearn) a un formato de igraph
  idag<-igraph.from.graphNEL(bnlearn::as.graphNEL(dag))
  return(idag)
}

idag<-bn2igraph(dag)  #idag esta en formato de red de igraph

plot(idag)  

# Set edge color to light gray, the node & border color to orange
# Replace the vertex label with the node names stored in "media"
plot(idag, edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(idag)$media, vertex.label.color="black")

#Layout
#Network layouts are simply algorithms that return coordinates for each node in a network

#El siguiente comando permite una grafica interactiva (en nueva ventana) donde podemos mover los vertices
tkplot(idag)
#Despues de posicionar los vertices donde queremos (y sin cerrar la ventana):
layout <- tkplot.getcoords(1)
layout  #da las coordenadas nuevas de los vertices

#Estas coordenadas se pueden usar en la grafica no interactiva directamente:

plot(idag, edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(idag)$media, vertex.label.color="black",
     layout=layout)

#guardar a pdf:

pdf(file="ejemplo.pdf")
plot(idag, edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(idag)$media, vertex.label.color="black",
     layout=layout)
dev.off()









#Lo siguiente lo añado luego de haberte enviado el correo. Es un intento de automatizar los valores de layout 
#metiendo información sobre la genealogía del arbol y el numero de miembros por generación para ubicar los
#vertices en forma homogénea en el plano.

#como automatizar la posición?
V(idag)
#+ 8/8 vertices, named, from bb07e00:
#  [1] A B D E L S T X

#manualmente establecer los valores de los vectores: nivel y pos
nivel<-c(1,2,4,3,2,1,2,4) #la posición, de arriba a abajo, segun el nombre del vertice
pos<-c(2,1,1,1,2,1,3,2)  #la posición por nivel, de izquieda a derecha, por cada nivel y segun el nombre del vertice


mpos<-cbind(pos,nivel)
nnivel<-function(n,mpos){max(mpos[mpos[,2]==n,1])}
nnivel(1,mpos)
numniv<-sapply(nivel,FUN=nnivel,mpos=mpos)  #numero de elementos por nivel
m_mpos<-cbind(mpos,pos/numniv)
meannivel<-function(n,mpos){mean(mpos[mpos[,2]==n,3])}
mean_nivel<-sapply(nivel,FUN=meannivel,mpos=m_mpos)  #numero de elementos por nivel
xpos<-((m_mpos[,3]-mean_nivel)+.5)*400  #en el intervalo (0,400)
y<-(max(nivel)-nivel+1)/max(nivel)
ypos<-(y-mean(y)+0.5)*400
lay<-cbind(xpos,ypos)

plot(idag, edge.arrow.size=.6, edge.color="blue",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(idag)$media, vertex.label.color="black",
     layout=lay)

