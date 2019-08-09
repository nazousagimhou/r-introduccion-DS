## Tener limpio mi espacio de trabajo
rm(list=ls())
setwd("~")

#Libreria para graficar
library(ggplot2)
library(fmsb)

#Importar base de datos
setwd("C:\\Users\\trol_\\Git\\r-introduccion-DS\\data_frames")
v_klustera <- read.csv("v.csv",stringsAsFactors = FALSE)

head(v_klustera)
View(v_klustera)

#no se ve bien la grafica con estos parametros 
#ggplot(v_klustera, aes(x=month_tz, y =tiempodeses)) + geom_point()
#tampoco se ve bien 
#ggplot(v_klustera, aes(x=device_mac, y =tiempodeses))
#no se ve nada si se grafican las horas y el tiempo de sesion 
#ggplot(v_klustera, aes(x=hour_tz, y =tiempodeses))+ geom_point()
ggplot(v_klustera, aes(x=hour_tz, y =day_tz))+ geom_point()

dim1<- dim(v_klustera)
dim1

#recortar dos columas del dataframe v_lustera
dia_semana<-v_klustera[["day_of_week_tz"]]
hora_sesion<-v_klustera[["hour_tz"]]

#creamos un nuevo dataframe con esos vectores 
dia_hora <- data.frame(dia_semana,hora_sesion)
dia_hora

#necesitamos un dataframe que solo tenga valores numericos 
#tomamos otro vector
dia_month<-v_klustera[["day_tz"]]

diames_hora<- data.frame(dia_month,hora_sesion)
diames_hora
