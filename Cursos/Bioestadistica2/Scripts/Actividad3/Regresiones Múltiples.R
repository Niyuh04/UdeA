#Regresión Múltiple

install.packages("MASS")
install.packages("lmtest")
install.packages("stepwise")
install.packages("nortest")
install.packages("leaps")
install.packages("relaimpo")
install.packages("boot")
install.packages("mitools")
library(MASS)
library(lmtest)
library(stepwise)
library(nortest)
library(leaps)
library(relaimpo)
library(boot)
library(mitools)

Datos1 <- Datos
Datos1

#Paso 1
RegModel.1 <- lm(Clorofila.a~Ekman+Plataforma.continental+Temperatura+Turbulencia+LnEkman+LnPlataforma+LnTemperatura+LnTurbulencia, data=Datos1)
summary(RegModel.1)

#Paso 2
library(stepwise)
Mod<-stepwise(RegModel.1, direction='forward/backward', criterion='AIC')

#Paso 3
RegModel.2 <- lm(Clorofila.a~Ekman+Plataforma.continental+Temperatura+Turbulencia, data=Datos1)
summary(RegModel.2)

#Paso 4
RegModel.3 <- lm(LnClorofila.a~Ekman+LnEkman+LnPlataforma+LnTemperatura+LnTurbulencia+Plataforma.continental+Temperatura+Turbulencia, data=Datos1)
summary(stepwise(RegModel.3, direction='forward/backward', criterion='AIC'))

#Paso 5
Modelo.final<- lm(LnClorofila.a~Ekman+Plataforma.continental+Temperatura+Turbulencia, data=Datos1)
summary(Modelo.final)

#Paso 6

library(MASS)
library(lmtest)
library(stepwise)
library(nortest)
library(leaps)
library(relaimpo)
library(boot)
library(mitools)


imprel<-calc.relimp(Modelo.final,type=c("lmg","last","first"),rela=TRUE)
boot <- boot.relimp(Modelo.final, b = 1000, type = c("lmg","last", "first"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot)
x11()
plot(booteval.relimp(boot,sort=TRUE))

#Paso 7
Residuos<-residuals(Modelo.final)
write.csv2(Residuos,file="Salida Cuadro XI.2.csv")

#Paso 8
Shapiro<-shapiro.test(Residuos) #Normalidad
Shapiro
Lillie<-lillie.test(Residuos)
Lillie

#Paso 9 INdependencia

install.packages("lmtest")
library(lmtest)
DW<-dwtest(Modelo.final)
DW

#Paso 10 Heterocedasticidad
BreuschPagan<-bptest(Modelo.final)
BreuschPagan

#Paso 11 #Factor de inflación de la varianza

install.packages("car")
library(car)
VIF<-vif(Modelo.final)
VarInflFact<-paste("Factores de Inflacción de la Varianza para la estima de la colinealidad")
VIF

#Paso 12
{
 sink("Salida Cuadro XI.2.txt")
 print(summary(Modelo.final))
 print(Shapiro)
 print(Lillie)
 print(BreuschPagan)
 print(DW)
 print(VarInflFact)
 print(VIF)
 sink()
}

