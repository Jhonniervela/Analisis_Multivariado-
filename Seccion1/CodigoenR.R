library("dplyr")
library("readr")
library("tidyr")
library("beeswarm")
library("ggplot2")
library("readxl")

## cargar datos 
resultados2014 <- read_delim("C:/Users/USUARIO/Documents/Rstudio/Analisis_Multivariado-/Seccion1/Datos/resultados2014.csv", ",", escape_double = FALSE, trim_ws = TRUE)
head(resultados2014, 5)

resultados2018 <- read_delim("C:/Users/USUARIO/Documents/Rstudio/Analisis_Multivariado-/Seccion1/Datos/resultados2018.csv", ",", escape_double = FALSE, trim_ws = TRUE)
head(resultados2018, 5)

## dimensión de las columnas
dim(resultados2014)

## información sobre las columnas
str(resultados2014)

## estadísticas descriptivas
summary(resultados2014)

## nombres de las columnas
names(resultados2014)
names(resultados2018)

## cambiar nombres de columnas
partidos_nombre <- c('pase18', 'pac18', 'adc18', 'pt18', 'fa18', 'pin18', 'pln18', 'pml18', 'png18', 'prc18', 'prsc18', 'prn18', 'pusc18')

cam_nombre <- function(dataframe) {
  for (i in 1:length(partidos_nombre)) {
    names(dataframe)[names(dataframe) == paste0('votos', i)] <- partidos_nombre[i]
  }
  return(dataframe)
}

resultados2018 <- cam_nombre(resultados2018)

## calcular porcentaje de votos 
votos_porcentaje <- function(dataframe) {
  # Tu implementación para calcular porcentajes de votos aquí
}

por_resultados2014 <- votos_porcentaje(resultados2014)
por_resultados2018 <- votos_porcentaje(resultados2018)

## ganador por zona 
winner <- function(dataframe, periodo) {
  x <- dataframe %>%
    gather(partido, votos, -codigo) %>%
    group_by(codigo) %>%
    filter(votos == max(votos)) %>%
    separate(partido, c(paste0("partido", periodo)), sep = "1") %>%
    select(-votos)
  return(x)
}

winner2014 <- winner(por_resultados2014, 14)
winner2018 <- winner(por_resultados2018, 18)


###############
#Como cambio la distribucion de los cantones ganados por 
#cada partido politico en comparación con las elecciones 2014

cambio <- winner2018 %>%
  left_join(winner2014, by = "codigo") %>%
  mutate(
    cambio = ifelse(partido18 == partido14, "sin cambio", "cambio"),
    robo = ifelse(
      cambio == "cambio",
      paste(partido18, partido14, sep = " al "),
      "sin cambio"
    )
  )

table(cambio$cambio)
table(cambio$robo)


###
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
### grafico y color
grafico_votos("pac", "red")
grafico_votos("prn", "blue")

cantones= resultados2018%>%
  filter(codigo==101 | codigo==103 |codigo==119|codigo==201|codigo==210|codigo==301)
sum(cantones$votos_validos)/sum(resultados2018$votos_validos)*100

por_resultados2018%>%
  filter(codigo==101 | codigo==103 |codigo==119|codigo==201|codigo==210|codigo==301)%>%
  gather(partido, votos, -codigo)%>%
  group_by(codigo)%>%
  filter(votos==max(votos))



####MAPAS 
library("sf")
library("raster")
library("ggplot2")
library("readxl")
library("dplyr")

# Ahora puedes leer el archivo Excel
codigohasc <- read_excel("C:/Users/USUARIO/Documents/Rstudio/Analisis_Multivariado-/Seccion1/Datos/codigohasc2.xlsx")

View(codigohasc)

# Para mapas
mapa14 <- left_join(resultados2014_porcen, codigohasc, by = "codigo")
mapa18 <- left_join(resultados2018_porcen, codigohasc, by = "codigo")

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
  
  # Generar el mapa
  return(
    ggplot() +
      geom_sf(data = cr_mapa, aes(fill = {{ partido }}), color = "white") +
      coord_sf() +
      scale_fill_gradient(low = "#E0E0E0", high = color_high, limits = c(0, 70)) +
      labs(x = NULL, y = NULL, title = titulo) +
      theme_void()
  )
}
mapa_resultados(mapa14, pln14, "red", "PLN 2014")
mapa_resultados(mapa14, pln14, "black", "PLN 2014")
mapa_resultados(mapa18, pln18, "blue", "PLN 2018")


library("gpclib")
library("raster")
library("maptools")
library("broom")
library(mapproj)
library(rlang)

cantones= resultados2018%>%
  filter(codigo==101 | codigo==103 |codigo==119|codigo==201|codigo==210|codigo==301)
sum(cantones$votos_validos)/sum(resultados2018$votos_validos)*100

resultados2018_porcen%>%
  filter(codigo==101 | codigo==103 |codigo==119|codigo==201|codigo==210|codigo==301)%>%
  gather(partido, votos, -codigo)%>%
  group_by(codigo)%>%
  filter(votos==max(votos))


