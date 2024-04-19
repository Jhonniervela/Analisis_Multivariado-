#----Instalación de librerias---

library(beeswarm)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(ggplot2)
library(stringr)

#---Importar archivos-----
resultados2014 = read_delim("C:/Users/USUARIO/Documents/Análisis Multivariado - Valeria Valera/CLASE 01/Pract. parte 01/resultados2014.csv",",", 
escape_double = FALSE, trim_ws = TRUE)
head(resultados2014) 


resultados2018 = read_delim("C:/Users/USUARIO/Documents/Análisis Multivariado - Valeria Valera/CLASE 01/Pract. parte 01/resultados2018.csv",",", 
escape_double = FALSE, trim_ws = TRUE) 
head(resultados2018) 

#Mostrar filas y columnas
dim(resultados2014) 

#Estructura/caracteristica de los datos
str(resultados2014)

#Nombres de las variables
names(resultados2014) 

#Descripción de las variables
summary(resultados2014) 

#Actualizar los nombres de las columnas con el nombre de los partidos politicos
partidos_nombre = c('pase18', 'pac18', 'adc18', 'pt18', 'fa18', 'pin18',
                    'pln18', 'pml18', 'png18', 'prc18', 'prsc18', 'prn18', 'pusc18')

cam_nombre = function(dataframe){
  for (i in 1:length(partidos_nombre)){
    names(dataframe)[names(dataframe) == paste0('votos', i)] =
      partidos_nombre[i]
  }
  return(dataframe)
}
resultados2018 = cam_nombre(resultados2018) 
head(resultados2018, 5) 

#-----Calcular porcentaje-------

votos_porcentaje = function(dataframe){
  x = dataframe%>%
    group_by(codigo)%>%
    mutate_all(funs((. / votos_validos)*100))%>%
    select(-votos_validos)
  return(x)
}

resultados2014_porcen = votos_porcentaje(resultados2014)
resultados2018_porcen = votos_porcentaje(resultados2018)
head(resultados2014_porcen)
head(resultados2018_porcen)




#-----Funcion para sacar partido ganador por zona------
winner = function(dataframe, periodo){
  x = dataframe%>%
    gather(partido, votos, -codigo) %>%
    group_by(codigo)%>%
    filter(votos==max(votos))%>%
    separate(partido, c(paste0("partido", periodo)),
             sep="1")%>%
    select(-votos)
  return(x)
  }

winner2014=winner(resultados2014_porcen, 14)
winner2018 =winner(resultados2018_porcen, 18)
table(winner2014$partido14)
table(winner2018$partido18)

#----¿Como cambio la distribucion de los cantones ganados por cada partido politico en comparación con las elecciones 2014?

cambio = winner2018%>%
  left_join(winner2014, by="codigo")%>%
  mutate(cambio=ifelse(partido18==partido14,"sin
cambio", "cambio"),
         robo=ifelse(cambio=="cambio",
                     paste(partido18, partido14, sep=" al "), "sin cambio"))

table(cambio$cambio)
table(cambio$robo)

#----Grafico de acuerdo al partido politico------
grafico_votos = function(partido, color){
  x = resultados2018_porcen%>%
    select(codigo, paste0(partido,18))%>%
    left_join(
      (resultados2014_porcen%>%
         select(codigo, paste0(partido,14))),
      by="codigo")%>%
    gather(anio, votos, - codigo)%>%
    mutate(anio=ifelse(anio==paste0(partido,14), 2014, 2018))
  
  par(las=1, bty="l", family="mono", font=1, bg="transparent")
  
  return(
    beeswarm(votos ~ anio, data=x, col=color, pch=16, method="hex", 
             cex=0.8, horizontal=TRUE, ylab="", xlab=paste("Porcentaje de votos del", toupper(partido)), 
             main=paste("Porcentaje de votos del", toupper(partido)), xlim=c(0, 60))
  )
}


#Mostrar gráfico y definir color
grafico_votos("pac", "red")
grafico_votos("prn", "yellow")

####MAPAS 
library("sf")
library("raster")
library("ggplot2")
library("readxl")
library("dplyr")

codigohasc = read_excel("C:/Users/USUARIO/Documents/Análisis Multivariado - Valeria Valera/CLASE 01/Pract. parte 01/codigohasc2.xlsx")
View(codigohasc)

# Para mapas
mapa14 = left_join(resultados2014_porcen, codigohasc, by="codigo")
mapa18 = left_join(resultados2018_porcen, codigohasc, by="codigo")

# Gráfico
cr = getData("GADM", country = "CRI", level = 2)
print(class(cr))

cr_sf = st_as_sf(cr)

mapa_resultados <- function(dataframe_mapa, partido, color_high, titulo) {
  
  cr_mapa = full_join(cr_sf, dataframe_mapa, by = c("HASC_2" = "HASC"))
  
  print(names(cr_mapa))
  
  
  if ("order" %in% names(cr_mapa)) {
    cr_mapa = arrange(cr_mapa, desc(order))
  } else {
    
    warning("Columna 'order' no encontrada, ajusta el código de ordenamiento.")
  }
  
  return(
    ggplot() +
      geom_sf(data = cr_mapa, aes(fill = {{ partido }}), color = "white") +
      coord_sf() +
      scale_fill_gradient(low = "#E0E0E0", high = color_high, limits = c(0, 70)) +
      labs(x = NULL, y = NULL, title = titulo) +
      theme_void()
  )
}
mapa_resultados(mapa14, pln14, "yellow", "PLN 2014")
mapa_resultados(mapa14, pln14, "skyblue", "PLN 2014")
mapa_resultados(mapa18, pln18, "red", "PLN 2018")


#TAREA - PREGUNTAS DE LA PRACTICA SESIÓN 01

#1. ¿Cómo logró un partido político, que contaba con un solo diputado en 2014, obtener el mayor porcentaje de votos en las elecciones de 2018?

# - Perdienron 26 municipios, PLN 9 PAC Y 2 FRENTE AMPLIO

# 2. ¿Cuántos cantones ganó cada partido político y cómo se compara este resultado con las elecciones de 2014?
table(winner2018$partido18)

# 3. ¿Cómo cambió la distribución de los cantones ganados por cada partido político en comparación con las elecciones de 2014?
table(cambio$cambio)

# 4. ¿En qué cantones logró el PRN superar a los demás partidos políticos?
  
table(cambio$robo)

# 5. ¿Cómo varió el porcentaje de votos obtenidos por el PLN, PUSC, PAC, PRN y PIN?
#Metodo grafico_votos esta mas arribita

#-Para variar el porcentaje se utilizó el siguiente código y para identificar cada uno se separo por colores:

grafico_votos("pln", "yellow")
grafico_votos("pusc", "green")
grafico_votos("pac", "purple")
grafico_votos("prn", "skyblue")
grafico_votos("pin", "orange")

# 6. ¿Qué partido político resultó ganador en los seis cantones con la mayor cantidad de electores?

# -El partido fue PRN, Fabricio Alvarado se impuso en los seis cantones con más electores.

# 7. ¿Qué partidos políticos resultaron ganador en cada uno de los cantones?
library("gpclib")
library("raster")
library("maptools")
library("broom")
library(mapproj)
library(rlang)
gpclibPermit()

cantones= resultados2018%>%
  filter(codigo==101 | codigo==103 |codigo==119|codigo==201|codigo==210|codigo==301)
sum(cantones$votos_validos)/sum(resultados2018$votos_validos)*100

resultados2018_porcen%>%
  filter(codigo==101 | codigo==103 |codigo==119|codigo==201|codigo==210|codigo==301)%>%
  gather(partido, votos, -codigo)%>%
  group_by(codigo)%>%
  filter(votos==max(votos))
