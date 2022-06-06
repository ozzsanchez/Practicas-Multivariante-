
#__________ K-MEANS____________

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
#    Metodo k-means
#---------------------------------

#1.- Separacion de filas y columnas.

dim(X)
# filas 
n<-dim(X)[1]
# columnas 
p<-dim(X)[2]


# 2.- Estandarizacion univariante.
X.s<-scale(X)

# 3.- Algoritmo k-medias (3 grupos)
# nstart: cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
# X.S: estadarizacion de los datos 
Kmeans.3<-kmeans(X.s, 3, nstart=25)

# centroides
Kmeans.3$centers

# cluster de pertenencia
Kmeans.3$cluster


# 4.- suma de cuadrados dentro de los grupos SCDG

SCDG<-sum(Kmeans.3$withinss)
SCDG

# 5.- separa los Clusters
cl.kmeans<-Kmeans.3$cluster
cl.kmeans

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("orange", "red", "green")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-meadias", pch=19)

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------

library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="blue")

#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generacion del grafico
plot(Sil.kmeans, main="Silhouette for k-means", 
col="orange")

#--------------------------------------------
# ejercicio 
#--------------------------------------------
# 1. replicar el script pero con un numero de clusters diferentes 
# a 3 y 1
# 2. incluir la interpretación del silhouetter
#__________ K-MEANS____________

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
#    Metodo k-means
#---------------------------------

#1.- Separacion de filas y columnas.

dim(x)
# filas 
n<-dim(x)[1]
# columnas 
p<-dim(x)[2]


# 2.- Estandarizacion univariante.
x.s<-scale(x)

# 3.- Algoritmo k-medias (8 grupos)
# nstart: cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
# X.S: estadarizacion de los datos 
Kmeans.3<-kmeans(x.s, 8, nstart=25)

# centroides
Kmeans.3$centers

# cluster de pertenencia
Kmeans.3$cluster


# 4.- suma de cuadrados dentro de los grupos SCDG

SCDG<-sum(Kmeans.3$withinss)
SCDG

# 5.- separa los Clusters
cl.kmeans<-Kmeans.3$cluster
cl.kmeans

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("orange", "red", "green","purple","black","yellow","grey","white")[cl.kmeans]
pairs(x.s, col=col.cluster, main="k-meadias", pch=19)

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------

library(cluster) 

clusplot(x.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(x.s)$score[,1:2],
     labels=rownames(x.s), pos=1, col="blue")

#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(x.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generacion del grafico
plot(Sil.kmeans, main="Silhouette for k-means", 
     col="orange")

# entre mas clutser se decida hacer tenemos en el Silhouette que en algunos clusters
# el ancho del Silhouette mejora en unos y empeora en otros 
# teniendo en cuenta que entre mas cercano a uno este el ancho del Silhouette
# la decisión sería hacer solo dos clusters adems la suma de cuadrados nos disminulle.

