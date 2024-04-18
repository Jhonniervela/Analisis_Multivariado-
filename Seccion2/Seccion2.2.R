install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

edad=c(26,18,20,19,25,22,37,56,78)
talla =c(1.56,1.72,1.65,1.44,1.69,1.66,1.51,1.62,1.42)
pairs(edad~talla,col="blue")


df = data.frame(edad,talla)
chart.Correlation(df)

## nivel de significancia
shapiro.test(edad)=      0.008444
shapiro.test(talla)=       0.4239
## valor de la correlacion 
cor(edad,talla)
cor.test(edad,talla)