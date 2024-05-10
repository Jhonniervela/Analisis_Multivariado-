##1.Identificar colinealidad
##
## - Por matriz de correlación

# Primero, seleccionamos solo las columnas numéricas del data frame
Data_universidad_numeric <- Data_universidad[, sapply(Data_universidad, is.numeric)]

# Luego, calculamos la correlación de Pearson
correlation_matrix <- round(cor(Data_universidad_numeric, method = "pearson"), 3)

# Imprimimos la matriz de correlación
print(correlation_matrix)

#Si hay correlaciones altas

##Por gráfica

library(psych)

multi.hist(x = Data_universidad_numeric, dcol = c("purple", "red"),dlty = c("dotted", "solid"), main = "")

library(GGally) 

ggpairs(Data_universidad_numeric, lower = list(continuous = "smooth"), diag = list(continuous = "barDiag"), axisLabels ="none") 

## 2. Identificar mejore predictores 

##modelox = lm(dependiente ~ independiente..


modelox <- lm(Data_universidad_numeric$UK_rank ~ Data_universidad_numeric$World_rank + Data_universidad_numeric$score + 
              Data_universidad_numeric$Minimum_IELTS_score+ Data_universidad_numeric$Student_satisfaction+ Data_universidad_numeric$`Estimated_cost_of_living_per_year_(in_pounds)`)


step(object = modelox, direction = "both", trace=1)


## Calculamos la colinealidad en el modelo

library(car)

vif(modelox)

## 3. Gráfica 
## En 3D


library(rgl)

## x.x - y
plot3d(Data_universidad_numeric$UK_rank, Data_universidad_numeric$score, Data_universidad_numeric$Student_satisfaction, pch = ".", size = 0.5)
