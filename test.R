library(rpart)
library(rpart.plot)

library(tidyverse)
library(caret)
library(e1071)
library(tree)

set.seed(581)

Data <- read.csv(file="./DatosInforme19.csv", header=TRUE, sep=";")

#Data$VTR <- as.numeric(Data$VTR)
Data$VTR <- factor(Data$VTR, levels=c(0:2), ordered=TRUE)
#Data$TiempoLibre <- as.factor(Data$TiempoLibre)
Data$TiempoLibre <- factor(Data$TiempoLibre, levels=c("Nada", "Poco", "Normal", "Mucho", "Demasiado"), ordered=TRUE)
#Data$Salud <- as.factor(Data$Salud)
Data$Salud <- factor(Data$Salud, levels=c("Muy Mala", "Suficiente", "Normal", "Buena", "Muy Buena"), ordered=TRUE)
#Data$Carrete <- as.factor(Data$Carrete)
Data$Carrete <- factor(Data$Carrete, levels=c("Nada", "Poco", "Normal", "Mucho", "Demasiado"), ordered=TRUE)
#Data$Inasistencias <- as.factor(Data$Inasistencias)
Data$Inasistencias <- factor(Data$Inasistencias, levels=c(0:10), ordered=TRUE)
#Data$HorasEstudioSemanal <- as.factor(Data$HorasEstudioSemanal)
Data$HorasEstudioSemanal <- factor(Data$HorasEstudioSemanal, levels=c("<2 hr", "2-5 hr", "5-10 hr", ">10 hr"), ordered=TRUE)
#Data$NotaFinal <- as.factor(Data$NotaFinal)
Data$NotaFinal <- factor(Data$NotaFinal, levels=c("<55", ">=55"), ordered=TRUE)
Data$Sexo <- as.factor(Data$Sexo)



Data_Sample <- sample_frac(Data, .8)
Data_Test <- setdiff(Data, Data_Sample)

# Entera
arbol <- rpart(NotaFinal ~ Sexo + HorasEstudioSemanal + VTR + TiempoLibre + Carrete
 + Salud + Inasistencias, data = Data_Sample, method = "class")

#arbol <- tree(NotaFinal ~ Sexo + HorasEstudioSemanal + VTR + TiempoLibre + Carrete
#               + Salud + Inasistencias, data = Data_Sample)

# Sin VTR
#arbol <- rpart(NotaFinal ~ Sexo + HorasEstudioSemanal + TiempoLibre + Carrete
#               + Salud + Inasistencias, data = Data_Sample, method = "class")

#arbol <- tree(NotaFinal ~ Sexo + HorasEstudioSemanal + TiempoLibre + Carrete
#               + Salud + Inasistencias, data = Data_Sample)

# Sin Inasistencias
#arbol <- rpart(NotaFinal ~ Sexo + HorasEstudioSemanal + VTR + TiempoLibre + Carrete
# + Salud, data = Data_Sample, method = "class")

#arbol <- tree(NotaFinal ~ Sexo + HorasEstudioSemanal + VTR + TiempoLibre + Carrete
#               + Salud, data = Data_Sample)

arbol
summary(arbol)
#plot(arbol)
#text(arbol, pretty = 2)
rpart.plot(arbol, type = 4, extra = 103)

prediccion_1 <- predict(arbol, newdata = Data_Test, type = "class")
confusionMatrix(prediccion_1, Data_Test[["NotaFinal"]])