install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

Peso=c(51,59,49,54,50,55,48,53,52,57)
largo =c(33.5,38,32,37.5,31.5,33,31,36.5,34,35)
pairs(largo~Peso,col="blue")


df = data.frame(Peso,largo)
chart.Correlation(df)
## nivel de significancia mayor a 0.05 R de Pearsen tienen dsitribucion parametricas menos RsPRI
shapiro.test(Peso)
shapiro.test(largo)
## valor de la correlacion 
cor(Peso,largo)
cor.test(Peso,largo)
