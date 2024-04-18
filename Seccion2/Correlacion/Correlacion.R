library(readr)
library(corrplot)
library(RColorBrewer)

# Lee los datos desde el archivo
mtcars <- read_delim("C:/Users/USUARIO/Documents/Rstudio/Analisis_Multivariado-/Seccion2/Correlacion/mtcars.csv", 
                     ",", escape_double = FALSE, trim_ws = TRUE)

# Visualiza los primeros registros del dataframe
View(mtcars)

# Muestra la estructura del dataframe
str(mtcars) 


mtcars_numeric <- mtcars[, sapply(mtcars, is.numeric)]


my_colors <- colorRampPalette(c("red", "blue", "black"))(200)


M <- cor(mtcars_numeric)


 
corrplot(M, method = "number", col = my_colors)   
corrplot(M, method = "shade", col = my_colors)    
corrplot(M, method = "pie", col = my_colors) 
corrplot(M, method = "color", col = my_colors)
corrplot(M, method = "circle", col = my_colors)   
corrplot(M, method = "square", col = my_colors) 
corrplot(M, method = "ellipse", col = my_colors)  