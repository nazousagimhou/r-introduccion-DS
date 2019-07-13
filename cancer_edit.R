rm(list=ls())#es para limpiar, i.e. limpia variables pre cargadas  
setwd("~")   #~ es para la ir a la raiz de la compu, hubicacion

##########################
# Machine Learning 		 #
# Decision Trees		 #
#   			 		 #
##########################

#install.packages(c("C50", "gmodels"))
#require('class') para esta secion no se usa
require(gmodels)
require(C50)

#require("AppliedPredictiveModeling")
#C:\Users\ale\Desktop\DS
##Cambia la ruta hacia la tu carpeta en desktop 
#o la carpeta donde guardaste el csv
#setwd("C:\\Users\\ale\\Deskstop\\DataScience")
#setwd("C:\\Users\\ale\\Desktop\\DS")

#Original:
 #dir1 <- "/Users/DanyJKelly/Desktop/DS/decisionTrees" 
#mi dierccion de archivo: 
  #C:\Users\ale\Desktop\DS

#dir1 <- "\\Users\\ale\\Deskstop\\DS" 
setwd("C:\\Users\\ale\\Desktop\\DS")

#Leer datos
#Base de datos de Wisconsin Breast Cancer Diagnostic 
#569 muestras

wbcd <- read.csv("wisc_bc_data.csv",stringsAsFactors = FALSE)

# ver al estructura
#32 propiedades, 1 con un identificador unico, 
#1 para el diagnóstico, 30 númericas

str(wbcd)


# Quitamos la variable id 
# no es necesaria para el clasificador. 
#Si no la quitamos, dará resultados irrelevantes
wbcd <- wbcd[-1]
str(wbcd)

# Generamos una tabla con los diagnosticos
table(wbcd$diagnosis)

# recodificamos la colmna diagnosis como factor
# los algoritmos, el paquete c50 requieren que el valor "objetivo" sea un factor 
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benigno", "Maligno"))
str(wbcd)

# Transformamos la tabla a porcentajes
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# separamos la DB en un set como entrenamiento y otro como prueba
#Daignosis es la columna 1
wbcd_train <- wbcd[1:469, -1]
wbcd_test <- wbcd[470:569, -1]

# Guardamos la clasificacion de cada uno (B o M) de la primera columna
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
str(wbcd_train_labels)
#para que el paquete c5.0 ficione las entradas que tiene necesitan ser tipo factor
# prueba 1 para cambiar a factor, no funciono 
# wbcd_train$diagnosis <- as.factor(wbcd_train$diagnosis)
# prueba 2, tampoco funciono
# wbcd_train$diagnosis <- factor(wbcd_train$diagnosis,levels = c("B", "M"),
#                                labels = c("Benigno", "Maligno"))
#fueron las comillas >:(

wbcd_model <- C5.0(wbcd_train,wbcd_train_labels)

wbcd_model
summary(wbcd_model)

## ------------- ------------- ------------- ------------- -------------
# Evaluamos el modelo
# Creamos un vector con las predicciones sobre nuestos datos de pruebas
wbcd_pred <- predict(wbcd_model, wbcd_test)

CrossTable(wbcd_test_labels, wbcd_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

## ------------- ------------- ------------- ------------- -------------
# boosts
# 
wbcd_boost10_model <- C5.0(wbcd_train, wbcd_train_labels,trials = 10)
wbcd_boost10_model
summary(wbcd_boost10_model)

wbcd_boost_pred10 <- predict(wbcd_boost10_model, wbcd_test)
CrossTable(wbcd_test_labels, wbcd_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicción'))




