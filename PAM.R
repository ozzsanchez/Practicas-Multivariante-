
#_______ PARTITION AROUND MEDOIDS (PAM)_____
# para las medianas a diferencia de k means que son medias #
library(cluster)

# Cargar la matriz de datos.

X<-as.data.frame(state.x77)
colnames(X)

#-------------------------------------
#     Transformacion de datos
#-------------------------------------

#1.- Transformacion de las variables x1,x3 y x8
# con la funcion de logaritmo.

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Population"

X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Illiteracy"

X[,8]<-log(X[,8])
colnames(X)[8]<-"Log-Area"

#---------------------------------
#    Metodo PAM
#---------------------------------

#1.- Separacion de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X)[2]

# 2.- Estandarizacion univariante.
X.s<-scale(X)

# 3.- Aplicacion del algoritmo
pam.3<-pam(X.s,3)

# 4.- Clusters
cl.pam<-pam.3$clustering
cl.pam

#5.- Scatter plot de la matriz con los grupos
col.cluster<-c("blue","red","green")[cl.pam]
pairs(X.s, col=col.cluster, main="PAM", pch=19)

#---------------------------------
#  Visualizacion con Componentes Principales
# funciones que se usan en componentes principales para poder 
# ver la visualización de los cluster  
#----------------------------------
clusplot(X.s,cl.pam)
text(princomp(X.s)$scores[,1:2],
     labels=rownames(X.s),pos=1, col="blue")

#-------------------------------------
#   Silhouette
#-------------------------------------

# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.pam<-silhouette(cl.pam, dist.Euc)

#2.- Generacion del grafico
plot(Sil.pam, main="Silhouette for PAM", 
     col="green")

#--------------------------------
# Ejercico
#________________________________
# 1. replicar el script pero con un numero de clusters diferentes 
# a 3 y 1
# 2. incluir la interpretación del silhouette

#_______ PARTITION AROUND MEDOIDS (PAM)_____
# para las medianas a diferencia de k means que son medias #
library(cluster)

# Cargar la matriz de datos.

x<-as.data.frame(state.x77)
colnames(x)

#-------------------------------------
#     Transformacion de datos
#-------------------------------------

#1.- Transformacion de las variables x1,x3 y x8
# con la funcion de logaritmo.

x[,1]<-log(x[,1])
colnames(x)[1]<-"Log-Population"

x[,3]<-log(x[,3])
colnames(x)[3]<-"Log-Illiteracy"

x[,8]<-log(x[,8])
colnames(x)[8]<-"Log-Area"

#---------------------------------
#    Metodo PAM
#---------------------------------

#1.- Separacion de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X)[2]

# 2.- Estandarizacion univariante.
X.s<-scale(X)

# 3.- Aplicacion del algoritmo con 2  clusters 
pam.3<-pam(X.s,2)

# 4.- Clusters
cl.pam<-pam.3$clustering
cl.pam

#5.- Scatter plot de la matriz con los grupos
col.cluster<-c("blue","red","green","purple","yellow","black")[cl.pam]
pairs(X.s, col=col.cluster, main="PAM", pch=19)

#---------------------------------
#  Visualizacion con Componentes Principales
# funciones que se usan en componentes principales para poder 
# ver la visualización de los cluster  
#----------------------------------
clusplot(X.s,cl.pam)
text(princomp(X.s)$scores[,1:2],
     labels=rownames(X.s),pos=1, col="green")

#-------------------------------------
#   Silhouette
#-------------------------------------

# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.pam<-silhouette(cl.pam, dist.Euc)

#2.- Generacion del grafico
plot(Sil.pam, main="Silhouette for PAM", 
     col="green")
#Con dos clusters podemos tener un buen ancho de Silhouette para cada cluster. 
#Aunque en el primer cluster hay observaciones negativas el averge del Silhouette general es  mayor que si 
#elijo 15 clusters cabe destacar que realice simulaciones con distintas cantidades de clusters 
#pero no son incluidas por que seria mucho y sin sentido, aunque el averege general mejora en diferentes 
#cantidades de clusters, el ancho de Silhouette para cada cluster puede ser un poco  mejor en algunos, 
#en otros es pésimo así que es mejor usar dos pues esta más balanceado.  

