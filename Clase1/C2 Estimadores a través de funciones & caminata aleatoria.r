# Estimadores a trav�s de funciones

X<-c(1,4,5,1)
Y<-c(4,5,6,8)

#1.Programar la media estad�stica

media<-function(X){
  n<-length(X)
  aux<-0
  for (i in 1:n){
    aux<-aux+X[i]
  }
  aux<-aux/n
  return(aux)
}
media(X)

mean(X)


#2.Programar la varianza estad�stica

varianza<-function(X){
  n<-length(X)
  aux<-0
  for (i in 1:n){
    aux<-aux+((X[i]-media(X))**2)
  }
  aux<-aux/(n-1)
  return(aux)
}
varianza(X)

var(X)


#3. Programar la covarianza

covarianza<-function(X,Y){
  n<-length(X)
  aux<-0
  for (i in 1:n){
    aux<-aux+((X[i]-media(X))*(Y[i]-media(Y)))
  }
  aux<-aux/(n-1)
  return(aux)
}
covarianza(X,Y)

cov(X,Y)


#4. Armar la matriz de Varianza-Covarianza

m<- array(0, dim=c(2,2))
m[1,1]<-covarianza(X,X)
m[1,2]<-covarianza(X,Y)
m[2,1]<-covarianza(Y,X)
m[2,2]<-covarianza(Y,Y)
m

L<-matrix(c(X,Y), nrow=4, ncol=2)
L[,1]

MatrizCov<-function(L){
  n<-length(L[1,])
  m<- array(0, dim=c(n,n))
  for (i in 1:n){
    for (j in 1:n){
      if (i==j){
      m[i,j]<-covarianza(L[,i],L[,i])
      }
      else {
        m[i,j]<-covarianza(L[,i],L[,j])
      }
    } 
  }
  return(m)
}
MatrizCov(L)




#CAMINATA ALEATORIA

install.packages('purrr')
install.packages('simEd')
install.packages('reshape')
install.packages('ggplot2')

library(purrr) #Se utiliza para la funci�n rdunif
library(simEd) #Se utiliza para la funci�n set.seed
library(ggplot2) #Se utiliza para graficar
library(reshape) #Se utiliza para la funci�n melt() de dataframe

set.seed(123)


numero_tiradas <- 50
numero_simulaciones <- 200

caminatas_totales <- list()

for(i in 1:numero_simulaciones){
  caminata_aleatoria <- c(0)
  
  for(x in 1:numero_tiradas){
    piso <- caminata_aleatoria[length(caminata_aleatoria)]
    dado <- rdunif(n = 1, b = 6, a = 1)
    
    if(dado <= 2){
      piso = max(0, piso-1)
    } else if(dado <= 5){
      piso <- piso+1
    } else{
      piso <- piso+rdunif(n = 1, b = 6, a = 1)
    }
    caminata_aleatoria <- c(caminata_aleatoria, piso)
  }
  caminatas_totales[[i]] <- c(caminata_aleatoria)
}

print(caminatas_totales)

#---------------------------->Gr�fica<----------------------------

#Para graficar en R, vamos a utilizar la librer�a ggplot.
#Sin embargo, necesitamos estructurar nuestros datos en un dataframe, es decir,
#tomar los vectores en caminatas totales y acomodarlos en una columna, adem�s de
#agregar otras columnas requeridas para que ggplot pueda funcionar.

df <- as.data.frame(caminatas_totales) #Convertimos nuestra lista a un dataframe
colnames(df) <- 1:numero_simulaciones #Renombramos nuestras columnas conforme 
#al n�mero de simulaci�n realizada

df <- melt(df) #Restructuramos el dataframe de tal manera que nos quedemos con
#dos columnas, "variable" que nos indica a que numero de simulaci�n
#pertenece y "value", donde est�n los valores de las simulaciones.

df$x <- rep(1:length(df$value[df$variable==1])) #Creamos una nueva columna "x"
#que nos indique la posici�n del 
#valor (en este caso era el piso que
#nos encontrabamos), dentro de cada 
#simulaci�n

#Gr�fica para visualizar el total de nuestras caminatas aleatorias

ggplot(data=df, aes(x=x, y=value, color=variable)) + geom_line(show.legend = FALSE) + 
  ggtitle('Simulaci�n de caminatas aleatorias') + labs(x = "N�mero de tirada", y = "Piso del edificio")

print(caminatas_totales)

ultimas_tiradas <- c()
for(i in 1:length(caminatas_totales)){
  ultimas_tiradas <- c(ultimas_tiradas, caminatas_totales[[i]][[51]])
}

print(ultimas_tiradas)

hist(x = ultimas_tiradas, freq = FALSE, main = "Distribuci�n del �ltimo piso alcanzado en las simulaciones",
     xlab = "Piso", ylab = "Frecuencia")

ultimas_tiradas >= 45
sum(ultimas_tiradas >= 45)
sum(ultimas_tiradas >= 45)/numero_simulaciones
