## 1. Identificar colinealidad
##
## - Por matriz de correlación

# Primero, seleccionamos solo las columnas numéricas del data frame
bookstore_numerico <- bookstore[, sapply(bookstore, is.numeric)]

# Luego, calculamos la correlación de Pearson
correlation_matrix <- cor(numeric_data, method = "pearson")

# Imprimimos la matriz de correlación
round(correlation_matrix, 3)

#Graficos

library(psych)
multi.hist(x = numeric_data, dcol = c("blue", "red"), dlty = c("dotted", "solid"), main = "")

library(GGally)
ggpairs(bookstore_numerico, lower = list(continuous = "smooth"), diag = list(continuous = "barDiag"), axisLabels ="none") 


## identificar mejores predictores

modelox = lm(bookstore_numerico$price ~ bookstore_numerico$pages+bookstore_numerico$reviews+bookstore_numerico$n_reviews+bookstore_numerico$star5+bookstore_numerico$star4+bookstore_numerico$star3
             +bookstore_numerico$star2 +bookstore_numerico$star1 +bookstore_numerico$weight)

step(object = modelox, direction = "both", trace=1)

##Calcular colinealidad en el modelo

library(car)

vif(modelox)

## 3. Gráfica
##grafica 3D
library(rgl)

plot3d(bookstore_numerico$star5, bookstore_numerico$pages, bookstore_numerico$price, pch = ".", size = 0.5)
