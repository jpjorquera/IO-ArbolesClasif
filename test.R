library(rpart)
library(rpart.plot)

library(tidyverse)
library(caret)
library(e1071)

set.seed(8512)

Data <- read.csv(file="./DatosInforme19.csv", header=TRUE, sep=";")
Data_Sample <- sample_frac(Data, .7)
Data_Test <- setdiff(Data, Data_Sample)

# Entera
arbol <- rpart(NotaFinal ~ Sexo + HorasEstudioSemanal + VTR + TiempoLibre + Carrete
 + Salud + Inasistencias, data = Data_Sample, method = "class")

# Sin VTR
#arbol <- rpart(NotaFinal ~ Sexo + HorasEstudioSemanal + TiempoLibre + Carrete
#               + Salud + Inasistencias, data = Data_Sample, method = "class")

# Sin Inasistencias
#arbol <- rpart(NotaFinal ~ Sexo + HorasEstudioSemanal + VTR + TiempoLibre + Carrete
# + Salud, data = Data_Sample, method = "class")

arbol
rpart.plot(arbol, type = 4, extra = 103)
#plot(dtm)
#text(dtm)

prediccion_1 <- predict(arbol, newdata = Data_Test, type = "class")
confusionMatrix(prediccion_1, Data_Test[["NotaFinal"]])