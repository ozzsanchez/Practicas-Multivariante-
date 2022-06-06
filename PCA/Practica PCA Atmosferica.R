# Aaplicacion de PCA de forma extensa 

#  1.- instalacion paquete de datos 
install.packages("datos")
# 2.- abrir libreria 
library(datos)
# 3.- eleccion de matriz de datos 
BD<-data.frame(datos::atmosfera)

# 4.- exploracion base de datos

str(BD)
head(BD)# el head se utiliza par copiar el nombre de la variable que no quieres 
# 5.- filtara las variables quedarse con las cuantitativas
BD[c("anio","mes","nube_baja","nube_alta","nube_media","longitud")]<-NULL  # se eliminan las cualitativas y otras variables  

#Al tener muchas observaciones podemos filtrar y trabajar con menos 

BD1 <- BD[1:1000,]
dim(BD1)

# se vuelve a visualizar para asegurarse que se removieron las variables no deseadas y la dimensión de la matriz es menor

str(BD1)   

# 6.- Se definen n (numero de observaciones) y p (9 variables de atmosfera)
dim(BD1)

n<-dim(BD1)[1]
p<-dim(BD1)[2]

# 7.- Generacion de un scatterplot
# de las variables originales
pairs(BD1,col="indianred4", pch=19, 
      main="Variables originales")

# 8.- Obtencion de los componentes principales
# con base en la matriz de covarianza muestral

mu<-colMeans(BD1)
s<-cov(BD1)
s

# Obtencion de los componentes principales nuevamente 
# con base en la matriz de covarianza muestral

mu<-colMeans(BD1)
s<-cov(BD1)
s
# 9.- Obtencion de los componentes principales 
# con base a la matriz de covarianza muestral
es<-eigen(s)
es

# 10.- Matriz de auto-valores
eigen.val<-es$values
eigen.val
# 11.- Matriz de auto-vectores
eigen.vec<-es$vectors
eigen.vec

# Proporcion de variabilidad para cada Valor 
pro.var<-eigen.val/sum(eigen.val)
pro.var
#Grafíco de la proporción  de variabilidad

plot(pro.var, type="l")

# Proporcion de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum
#----------------------------------
# Obtencion de los componentes principales con
# base en la matriz de correlaciones muestrales
#---------------------------------------

R<-cor(BD1)
eR<-eigen(R)
eR

# Obtencion de auto-valores
eigen.val<-eR$values
eigen.val
# Obtencion de auto-vectores
eigen.vec<-eR$vectors
eigen.vec
# Proporcion de variablidad
pro.var<-eigen.val/sum(eigen.val)
pro.var
# Proporcion de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum
# Media de los auto-valores
mean(eigen.val)

#---------------------------
# Obtencion de los coeficientes (nuevas variables)
# 
#--------------------------

# 1.- Centrar los datos con respecto a la media
ones<-matrix(rep(1,n),nrow=n, ncol=1)
ones
# 2.- Construccion de la matriz centrada
X.cen<-as.matrix(BD1)-ones%*%mu
X.cen

# 3.- Construccion de la matriz diagonal de las 
# varianzas
Dx<-diag(diag(s))
Dx

# 4.- Construccion de la matriz centrada multiplicada
# por Dx^1/2

Y<-X.cen%*%solve(Dx)^(1/2)
Y #datos normalizados

# 5.- Construccion de los coeficientes o scores
# eigen.vec matriz de autovectores
scores<-Y%*%eigen.vec

# Nombramos las columnas PC1...PC8
colnames(scores)<-c("PC1","PC2","PC3","PC4","PC5")

# visualizamos
scores

# Generacion del grafico de los scores
pairs(scores, main="scores", col="chartreuse", pch=1)

# ACP via sintetizada 

#1.- calculo de la varianza a las columnas 1= filas, 2= columnas 
apply(BD_1, 2, var)

#2.- aplicar la funcion **prcom** pra reducir la dimencinalidad y centrado por la nedia y escalada por la desviacion estandar (dividir entre sd)

acp<-prcomp(BD_1, center=TRUE, scale=TRUE)
acp

# 3.- generacion del grafico **screeplot**
plot(acp, type="l")

# 4.- Resumen de la matriz **acp**
summary(acp)

# 4.1.- En este punto se seleccionan el numero de componentes, siguiendo el criterio del 80% de la varíanza explicada.
# para este ejemplo se van a seleccionar **2** factores de **0.8683** varianza explicada

# 5.- Construcción del Biplot
biplot(acp, scale=0)











