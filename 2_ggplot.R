rm(list=ls())
setwd("~")

############################
# Oscar Elton			         #
# Graficos en ggplot      #
# 			                   #
############################

require(foreign)
#require(plyr)
require(tidyverse)
require(readstata13)
require(reshape2)
require(readxl)

#"C:\\Users\\Violet Evergarden\\Documents\\DS")
#dir1 <- "/Users/DanyJKelly/Desktop/DS/ggplot2/Out"
dir1 <-"C:\\Users\\Violet Evergarden\\Documents\\DS"
#dir2 <- "/Users/DanyJKelly/Desktop/DS/ggplot2/Graphs"
dir2 <-"C:\\Users\\Violet Evergarden\\Documents\\DS"

# Usualmente, cuando utilizo bases grandes y voy a hacer distintas graficas de una misma base
# abro la base hasta el principio y la dejo intacta. Al procesar para cada gráfica hago bases temporales.
# Esto me evita tener que estar abriendo una y otra vez la base.
data <- read_csv(paste(dir1, "rnped_limpia.csv", sep="/"))

str(data)

max(data["year"])
min(data["year"])
# Vamos a ver primero lo más obvio y sencillo, ¿cuántos desaparecidos hubo por municipio en...2011 (año más violento)?

########
# Bars #
########
# Esta primera  grafica sera un scatter, pero que va a cumplir 
# la función de las barras dado que vamos a graficar 2,457 municipios
# Primero la base:
tempo <- data %>%
         filter(year==2011) %>%
         group_by(inegi, nom_ent, nom_mun) %>%
         summarise(total = sum(total)) %>%
         arrange(inegi) %>%
         ungroup()

gr <- ggplot(tempo, aes(x=inegi, y=total)) +
      geom_point()

gr
#Que es un operador ternario?


gr <- ggplot(tempo, aes(x=inegi, y=total)) +
      geom_point(color="purple") +
      geom_text(aes(x=inegi, y=total, label=paste(nom_mun, nom_ent, sep=", ")), data=tempo[tempo$total>100,], hjust=0, vjust=-1) +
      labs(title="Total de desaparecidos por municipio", subtitle="2011", x="", y="Total de desaparecidos") +
      theme(axis.text.x = element_blank())
 gr

# Guardar la gráfica
ggsave(paste(dir2, "1.png", sep="/"), plot=gr, width=12, height=12) # el formato normalito     
ggsave(paste(dir2, "1.eps", sep="/"), plot=gr, width=12, height=12) # para diseñadores     
      
# ¿y si queremos ver si mas hombres o mujeres son vulnerables? 
# Veamos la tasa
tempo <- data %>%
         filter(year==2011) %>%
         group_by(sexo) %>%
         summarise(tdes = weighted.mean(tdes, pob))
tempo

# Ese fondito gris es caracteristico de ggplot, pero se puede quitar 
# (ahorita vemos cómo)

gr <- ggplot(tempo, aes(x=sexo, y=tdes)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label=round(tdes, 2), vjust=-1)) +
      labs(title="Tasa de desaparecidos 2011", subtitle="por sexo"
           , x="Sexo", y="Tasa de desaparecidos")  

gr             

ggsave(paste(dir2, "2.png", sep="/"), plot=gr, width=12, height=12)     

# Imaginen que queremos ver el porcentaje de desaparecidos que hay
# por sexo y edad. Es decir, queremos sabe quantas de las mujeres
# desaparecidas tienen entre 0 y 11 a�os, por ejemplo

tempo <- data %>%
         group_by(sexo, rango_edad) %>%
         summarise(total = sum(total)) %>%
         ungroup() %>%
         group_by(sexo) %>%
         mutate(totales = sum(total),
                porcentaje = round((total/totales)*100, 2))
tempo

gr <- ggplot(tempo, aes(x=sexo, y=porcentaje, fill=rango_edad)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5)) +
      labs(title="Porcentaje de desaparecidos entre 2010 y 2015 \npor sexo y edad", 
           x="Sexo", y="% de desaparecidos", fill="Rango de edad")          

ggsave(paste(dir2, "3.png", sep="/"), plot=gr, width=12, height=12)

# Ok, pero se ve muy feita. Vamos a volter las barras, cambiar colores y quitar fondo gris
gr <- ggplot(tempo, aes(x=sexo, y=porcentaje, fill=rango_edad)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=45) +
      scale_fill_manual(values=c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")) +
      labs(title="Porcentaje de desaparecidos entre 2010 y 2015 \npor sexo y edad", 
           x="Sexo", y="% de desaparecidos", fill="Rango de edad") +
      coord_flip() +
      theme_bw()
gr

ggsave(paste(dir2, "4.png", sep="/"), plot=gr, width=12, height=12)

# Ahora queremos saber la tasa por sexo y estado. �Que se les ocurre?
tempo <- data %>%
         group_by(sexo, ent, nom_ent) %>%
         summarise(tdes = weighted.mean(tdes, pob)) %>%
         mutate(tdes = round(tdes, 2))
tempo
         
# Hay de dos. Una...
gr <- ggplot(tempo, aes(x=sexo, y=tdes)) +
      geom_bar(stat="identity", fill="#41b6c4") +
      geom_text(aes(label=tdes), vjust=-0.3, size=3) +
      facet_wrap(~nom_ent) +
      labs(title="Tasa de desaparecidos por sexo y estado \n2010 - 2015", 
           x="Sexo", y="Tasa de desaparecidos") +
      theme_bw()
gr

ggsave(paste(dir2, "5.png", sep="/"), plot=gr, width=12, height=12)

# Y dos... diferencias?
gr <- ggplot(tempo, aes(x=sexo, y=tdes)) +
  geom_bar(stat="identity", fill="#41b6c4") +
  geom_text(aes(label=tdes), vjust=-0.3, size=3) +
  facet_grid(sexo~nom_ent) +
  labs(title="Tasa de desaparecidos por sexo y estado \n2010 - 2015", 
       x="Sexo", y="Tasa de desaparecidos") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))
gr

ggsave(paste(dir2, "6.png", sep="/"), plot=gr, width=20, height=12)

# Ahora barras 2D, mejor conocidas en Data4 como Chicharitos
# Nos sirven cuando queremos ver dos dimensiones de una variable con varias categorias, por ejemplo
# queremos ver % de hombres/mujeres que desaparecieron entre los 12 a 17 años vs 26-40 años

tempo <- data %>%
         group_by(sexo, rango_edad) %>%
         summarise(total = sum(total)) %>%
         ungroup() %>%
         group_by(sexo) %>%
         mutate(totales = sum(total),
                porcentaje = round((total/totales)*100, 2))
tempo

# Quedemonos con lo que nos importa
tempo <- tempo %>%
         select(sexo, rango_edad, porcentaje) %>%
         filter(rango_edad=="0 a 11 a\u00f1os" | rango_edad=="26 a 40 a\u00f1os") %>%
         spread(rango_edad, porcentaje)
tempo

names(tempo) <- c("sexo", "des0a11", "des26a40")


gr <- ggplot(tempo, aes(x = des12a17, y = des26a40, color=sexo)) +
      geom_segment(aes(xend=des12a17), yend=0, size=2) +
      geom_segment(aes(yend=des26a40), xend=0, size=2) +
      scale_y_continuous(limits=c(0, 42)) +
      scale_x_continuous(limits=c(0, 42)) +
      labs(title="Porcentaje de desaparecidos por sexo \n 12 a 17 a�os vs 26 a 40 a�os",
           x="% de desaparecidos 12 a 17 a�os", y="% de desaparecidos 25 a 40 a�os", color="Sexo")+theme_bw()
gr

ggsave(paste(dir2, "7.png", sep="/"), plot=gr, width=12, height=12)

############
# Heatmaps #
############

tempo <- data %>%
         group_by(sexo, rango_edad) %>%
         summarise(tdes = weighted.mean(tdes, pob))

gr <- ggplot(tempo, aes(x=sexo, y=rango_edad, fill=tdes)) +
      geom_tile(color="black") +
      scale_fill_continuous(low="#ffeda0", high="#f03b20") +
      labs(title="Tasa de desaparecidos promedio \n2010-2015", 
           x="Sexo", y="Edad", fill="Tasa de desaparecidos") +
      coord_fixed() +
      theme_bw()
tempo

ggsave(paste(dir2, "8.png", sep="/"), plot=gr, width=12, height = 12)

############ 
# Treemaps #
############
install.packages("treemap")
require(treemap)

# Vamos a ver el porcentaje de desaparecidos por estado en 2011. Un treemap nos ayuda a ver las partes que
# conforman un todo facilmente, utilizando areas.

tempo  <- data %>%
          filter(year==2011) %>%
          group_by(ent, nom_ent) %>%
          summarise(total = sum(total)) %>%
          ungroup() %>%
          mutate(totales = sum(total),
                 porcentaje = round((total/totales)*100, 2))

png(paste(dir2, "9.png", sep="/"), width=12, height=12, units="in", res=300)
treemap(tempo, index="nom_ent", vSize="porcentaje", vColor="index", type="index", title="Porcentaje desaperecidos por entidad - 2011", palette="Reds", title.legend="", border.col="grey", border.lwd=0.5)
dev.off()

png(paste(dir2, "10.png", sep="/"), width=12, height=12, units="in", res=300)
treemap(tempo, index="nom_ent", vSize=c("porcentaje"), vColor="porcentaje", type="value", title="Porcentaje desaperecidos por entidad - 2011", palette="Blues", title.legend="", border.col="grey", border.lwd=0.5)
dev.off() 

###########
# Scatter #
###########

tempo <- data %>%
         group_by(inegi, nom_ent, nom_mun) %>%
         summarise(tdes = round(weighted.mean(tdes, pob),2),
                   total = round(mean(total),2))
tempo

gr <- ggplot(tempo, aes(x=total, y=tdes)) +
      geom_point(color="maroon") +
      geom_text(aes(label=inegi), color="maroon", size=3, vjust=-1, hjust=0) +
      labs(title="Total de desaparecidos vs Tasa de desaparecidos",
           x="Total de desaparecidos", y="Tasa de desaparecidos") +
      theme_bw()
gr

ggsave(paste(dir2, "11.png", sep="/"), plot=gr, width=12, height = 12)

# Lineas de tendencia
#1 linea recta
gr <- ggplot(tempo, aes(x=total, y=tdes)) +
      geom_point(color="maroon") +
      geom_text(aes(label=inegi), color="maroon", size=3, vjust=-1, hjust=0) +
      geom_smooth(method="lm", se=T) + #intervalo de cofianza "se =T"
      labs(title="Total de desaparecidos vs Tasa de desaparecidos",
           x="Total de desaparecidos", y="Tasa de desaparecidos") +
      theme_bw()
ggsave(paste(dir2, "12.png", sep="/"), plot=gr, width =12, height = 12)

#2 escalas
gr <- ggplot(tempo, aes(x=total, y=tdes)) +
      geom_point(color="maroon") +
      geom_text(aes(label=inegi), color="maroon", size=3, vjust=-1, hjust=0) +
      geom_smooth() +
      labs(title="Total de desaparecidos vs Tasa de desaparecidos",
           x="Total de desaparecidos", y="Tasa de desaparecidos") +
      theme_bw() +
      scale_x_log10() + scale_y_log10()

gr
ggsave(paste(dir2, "13.png", sep="/"),plot=gr, width=12, height = 12)


##########
# Fiebre #
##########
# Sirve para ver tendencia en el tiempo
tempo <- data %>%
         group_by(sexo, year) %>%
         summarise(tdes = round(weighted.mean(tdes, pob),2))

gr <- ggplot(tempo, aes(x=year, y=tdes, color=sexo)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label=tdes), vjust=-1) +
      labs(title="Tendencia de la tasa de desaparecidos \n 2010-2015",
           x="Año", y="Tasa de desaparecidos", color="Sexo") +
      theme_bw()
ggsave(paste(dir2, "15.png", sep="/"), plot=gr, width=12, height = 12)

########
# Area #
########
# Otra forma seria ver el % de desaparecidos hombres y mujeres por a�o
tempo <- data %>%
         group_by(sexo, year) %>%
         summarise(total = sum(total)) %>%
         ungroup() %>%
         group_by(year) %>%
         mutate(totales = sum(total),
                porcentaje = round((total/totales)*100,2))

gr <- ggplot(tempo, aes(x=year, y=porcentaje, fill=sexo)) +
      geom_area() +
      geom_point( data=tempo[tempo$sexo=='Mujer',])+
      geom_text(aes(label=porcentaje), vjust=-1, data=tempo[tempo$sexo=='Mujer',])+
      scale_fill_manual(values=c("#67a9cf", "#ef8a62")) +
      labs(title="Porcentaje de desaparecidos por sexo \n 2010-2015",
           x="Año", y="% de desaparecidos", fill="Sexo") +
      theme_bw()
ggsave(paste(dir2, "16.png", sep="/"), plot=gr, width=12, height = 12)

# que tal % por estado por año?
tempo <- data %>%
         group_by(nom_ent, year) %>%
         summarise(total = sum(total)) %>%
         ungroup() %>%
         group_by(year) %>%
         mutate(totales = sum(total),
                porcentaje = round((total/totales)*100,2))
tempo

gr <- ggplot(tempo, aes(x=year, y=porcentaje, fill=nom_ent)) +
      geom_area(color="black") +
      labs(title="Porcentaje de desaparecidos por entidad \n 2010-2015",
           x="Año", y="% de desaparecidos", fill="Entidad") +
      theme_bw()
ggsave(paste(dir2, "17.png", sep="/"), plot=gr, width=12, height = 12)

############
# Boxplots #
############
tempo <- data %>%
         group_by(nom_ent, year) %>%
         summarise(tdes = weighted.mean(tdes, pob))
tempo

gr <- ggplot(tempo, aes(x=nom_ent, y=tdes)) +
      geom_boxplot() +
      labs(title="Distribucion tasa de desaparecidos por estado", x="Entidad", y="Tasa de desaparecidos") +
      theme_bw() +
      theme(axis.text.x = element_text(angle=90))
ggsave(paste(dir2, "18.png", sep="/"), plot=gr, width=12, height = 12)


tempo$year <- as.character(tempo$year)
gr <- ggplot(tempo, aes(x=year, y=tdes, fill=year)) +
      geom_boxplot() +
      labs(title="Distribucion tasa de desaparecidos por a�o", x="A�o", y="Tasa de desaparecidos", fill="") +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            legend.position = "bottom")
ggsave(paste(dir2, "19.png", sep="/"), plot=gr, width=12, height = 12)






