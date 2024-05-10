##1.Identificar colinealidad
##
## - Matriz de correlación

# Primero, seleccionamos solo las columnas numéricas del data frame
beisbol_numeric <- beisbol[, sapply(beisbol, is.numeric)]

# Luego, calculamos la correlación de Pearson
correlation_matrix <- round(cor(beisbol_numeric, method = "pearson"), 3)

# Imprimimos la matriz de correlación
print(correlation_matrix)

#No hay correlaciones altas

##Por gráfica

library(psych)

multi.hist(x = beisbol_numeric, dcol = c("purple", "red"),dlty = c("dotted", "solid"), main = "")

library(GGally) 

ggpairs(beisbol_numeric, lower = list(continuous = "smooth"), diag = list(continuous = "barDiag"), axisLabels ="none") 

## 2. Identificar mejore predictores 

##modelox = lm(dependiente ~ independiente..


modelox <- lm(Age ~ Games + `Minutes Played` + `Fields Goal` + `3-points Field Goal`, data = beisbol_numeric)


step(object = modelox, direction = "both", trace=1)


## Calculamos la colinealidad en el modelo

library(car)

vif(modelox)

## 3. Gráfica 
## En 3D


library(rgl)

## x.x - y
plot3d(beisbol$Points, beisbol$`Minutes Played`, beisbol$Age, pch = ".", size = 0.5)
