# Independientes 

EA = c(2, 3, 1, 4, 2, 3, 2, 1, 3, 2) 

EB = c(3, 2, 1, 3, 2, 2, 4, 2, 3, 1) 


boxplot(EA, EB, names=c("EA", "EB")) 

medias = c(mean(EA),mean(EB)) 

points(medias,pch=18,col="red") 

par(mar = c(2, 2, 2, 2)) 
par(mfrow = c(1, 2)) 
qqnorm(EA, xlab = "", ylab = "", main = "EA") 
qqline(EA) 
qqnorm(EB, xlab = "", ylab = "", main = "EB") 
qqline(EB) 


t.test(EA, EB, paired = FALSE) 

#Dependientes 

PuntajeAntesTerapia = c(7, 6, 5, 6, 7) 

PuntajeDespuesTerapia = c(8, 7, 8, 8, 9) 





boxplot(PuntajeAntesTerapia, PuntajeDespuesTerapia, names=c(" Terapia Antes", " Terapia Despues")) 



mediasTerapia = c(mean(TerapiaAntes),mean(TerapiaDespues)) 

points(mediasTerapia,pch=18,col="red") 



par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 2))
qqnorm(PuntajeAntesTerapia, xlab = "", ylab = "", main = " Terapia Antes") 
qqline(PuntajeAntesTerapia)
qqnorm(PuntajeDespuesTerapia, xlab = "", ylab = "", main = " Terapia Despues") 
qqline(PuntajeDespuesTerapia)



t.test(PuntajeAntesTerapia, PuntajeDespuesTerapia, paired = TRUE)


