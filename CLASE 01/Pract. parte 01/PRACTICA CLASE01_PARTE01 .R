library(dplyr)
library(readxl)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(beeswarm)

#----------IMPORTAR ARCHIVOS--------

 resultados2014= read_delim("C:/Users/USUARIO/Downloads/OneDrive_2024-03-22/AM - IV/resultados2014.csv",",",
 escape_double= FALSE, trim_ws=TRUE)
 head(resultados2014,5)

 View(resultados2014)
 
 #---NUMERO DE FILAS Y COLUMNAS----
 dim(resultados2014)
 
 #---ESTRUCTURA/CARACTERISTICA DE LOS DATOS-----
 str(resultados2014)
 
 #----DATOS DESCRPTIVOS DE LAS VARIABLES-----
 summary(resultados2014)
 
 #----NOMBRES QUE TIENEN LAS VARIABLES------
 names(resultados2014)
 
 resultados2018= read_delim("C:/Users/USUARIO/Downloads/OneDrive_2024-03-22/AM - IV/resultados2018.csv",",",
 escape_double= FALSE, trim_ws=TRUE)
 head(resultados2018,5)
 
 names(resultados2018)
 
 partidos_nombre = c('pase18', 'pac18', 'adc18', 'pt18', 'fa18', 'pin18','pln18', 'pml18', 'png18', 'prc18', 'prsc18', 'prn18', 'pusc18')
 
 cam_nombre = function(dataframe)
   {
   for (i in 1:length(partidos_nombre))
   { names(dataframe)[names(dataframe)
   == paste0('votos', i)] = partidos_nombre[i]
   }
   return(dataframe)
   }

   resultados2018=cam_nombre(resultados2018)
   
   head(resultados2018,5)
   names(resultados2018)

###calcular porcentaje de votos

votos_porcentaje= function(dataframe){
  x=dataframe%>%
       group_by(codigo)%>%
  mutate_all(funs((. / votos_validos)*100))%>%
  select(-votos_validos)
  return(x)
  }

por_resultados2014 = votos_porcentaje(resultados2014)

por_resultados2018 = votos_porcentaje(resultados2018)

###calcular partido ganador por zona

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

winner2014=winner(por_resultados2014, 14)
winner2018 =winner(por_resultados2018, 18)

table(winner2014$partido14)
table(winner2018$partido18)


###----

cambio = winner2018%>%
  
  left_join(winner2014, by="codigo")%>%
  
  mutate(cambio=ifelse(partido18==partido14,"sin
cambio", "cambio"),
         
         robo=ifelse(cambio=="cambio",
                     paste(partido18, partido14, sep=" al "), "sin
cambio"))


table(cambio$cambio)