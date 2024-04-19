library("beeswarm")
library("ggplot2")
library("readxl")

# Leer los datos del archivo
births <- read_xlsx("C:/Users/USUARIO/Documents/Rstudio/Analisis_Multivariado-/Seccion2/Tarea/births.xlsx")

# Filtrar los datos para fumadores y no fumadores
fumar <- filter(births, smoke == "smoker")
nofumar <- filter(births, smoke == "nonsmoker")

# Realizar la prueba t de Student
t_test_result <- t.test(fumar$weight, nofumar$weight)

# Imprimir el resultado de la prueba t
print(t_test_result)

# Crear un gráfico
plot_data <- rbind(data.frame(weight = fumar$weight, group = "Fumador"),
                   data.frame(weight = nofumar$weight, group = "No Fumador"))

# Crear un gráfico de densidad
ggplot(plot_data, aes(x = weight, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de pesos para fumadores y no fumadores",
       x = "Peso",
       y = "Densidad") +
  scale_fill_manual(values = c("Fumador" = "blue", "No Fumador" = "red")) +
  theme_minimal()


# pregunta 2 

# Filtrar los datos para género femenino y masculino
B.femenino <- filter(births, sex_baby == "female")
B.masculino <- filter(births, sex_baby == "male")

# Realizar la prueba t de Student
t_test_result <- t.test(B.femenino$weight, B.masculino$weight)

# Mostrar el resultado
print(t_test_result)

# Crear un dataframe para el gráfico
plot_data <- rbind(data.frame(weight = B.femenino$weight, sex = "Femenino"),
                   data.frame(weight = B.masculino$weight, sex = "Masculino"))

# Graficar
ggplot(plot_data, aes(x = sex, y = weight, fill = sex)) +
  geom_boxplot() +
  labs(x = "Sexo", y = "Peso", title = "Distribución de pesos por género") +
  theme_minimal()