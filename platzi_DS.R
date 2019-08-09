#Platzi Data Science

## Tener limpio mi espacio de trabajo
rm(list=ls())
setwd("~")


#Como crar un lista 

milista <- list(num=23,24, saludo = "hello")

milista

milista$num[1,2]

class(milista)

str(milista)

#Matrices 

mat <- matrix(c(1,2,3,4,5,6,8,8), nrow = 4)
help(matrix) 

colnames(mat) <- c("primera","segunda")
mat[2,2]

#Como crear un DATAFRAME

entidad <- c("TLAXCALA","COLIMA","MORELOS","HIDALGO","DURANGO","SINALOA","TLAXCALA","COLIMA","MORELOS","HIDALGO","DURANGO","SINALOA","TLAXCALA","COLIMA","MORELOS","DURANGO","SINALOA")
             
estatura<- c(1.59,1.8,1.6,1.78,1.7,1.7,1.65,1.75,1.6,1.85,1.7,1.75,1.6,1.7,1.6,1.7,1.72)

sexo <- c("MUJER","HOMBRE","HOMBRE","HOMBRE","HOMBRE","HOMBRE","HOMBRE","HOMBRE","HOMBRE","HOMBRE","HOMBRE","HOMBRE","MUJER","HOMBRE","HOMBRE","MUJER","HOMBRE")

datos <- data.frame(entidad,estatura,sexo)

#$ entidad : Factor w/ 6 levels "COLIMA","DURANGO",..: 6 1 4 3 2 5 6 1 4 3 ...
str(datos)

#nos aroja dos valores, 17 y 3, que coinciden con el num de renglones y columnas
dim(datos) 

#un renglon especifico o un dato 
datos[1,]
datos[3,3]

#toda una columna, que es el vector
datos$sexo

#ver como se cumple una condicion en vector o columna con un valor asignado
est <- datos$estatura>=1.6
est

#Podemos ver las posiciones de los valores en el vector especificado anteriormente que
#cumplen la condicion

estRenglon <- which(est)
estRenglon

#Podemos usar las posiciones anteriores para aplicar un filtro
datos[estRenglon,]

#Un archivo csv
#libros<-read.csv("C:\\Users\\trol_\\Git\\r-introduccion-DS\\data_frames")

setwd("C:\\Users\\trol_\\Git\\r-introduccion-DS\\data_frames")
libros <- read.csv("books.csv",stringsAsFactors = FALSE)

head(libros)
View(libros)

class(libros)
str(libros)

#Como guardar un dataframe como ".csv"
#Iris es un dataframe que esta contenido en la paquetería de R
iris
write.csv(iris,"C:\\Users\\trol_\\Git\\r-introduccion-DS\\data_frames\\iris.csv")

