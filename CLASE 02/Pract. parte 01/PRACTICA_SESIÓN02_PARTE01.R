install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

Peso=c(51,59,49,54,50,55,48,53,52,57)
largo =c(33.5,38,32,37.5,31.5,33,31,36.5,34,35)
pairs(largo~Peso, col="red")

df = data.frame(Peso,largo)
chart.Correlation(df)

#correlaciòn de spearman

shapiro.test(Peso)
shapiro.test(largo)

cor(Peso,largo)
cor.test(Peso,largo)

##########################################
         
Edad=c(26,18,20,19,25,22,37,56,78)
Talla =c(1.56,1.72,1.65,1.44,1.69,1.66,1.51,1.62,1.42)
pairs(largo~Peso, col="yellow")         
         
df = data.frame(Edad,Talla)
chart.Correlation(df)         
         
shapiro.test(Edad)
shapiro.test(Talla)        
         
cor(Edad,Talla)
cor.test(Edad,Talla)         
         