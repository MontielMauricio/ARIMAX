################################################
# Con este código se pueden pronósticar series de tiempo multivariadas
#
## Funciones de transferencia utilizando el Indice Global de la Activida Económica
#
# Hecho por Mauricio Montiel el 10/04/2020
###############################################

# Preliminares
remove(list = ls())
setwd("~/GitHub/Forecasting/ARIMAX")

#librerías
library(readxl)
library(tidyverse)
library(tseries)
library(urca)
library(forecast)

# Importar datos
sales_x <- read_excel("data.xlsx")
sales_x <- ts(sales_x, start = c(2014,01), frequency = 12)

# Grafico de datos en niveles
ts.plot(scale(sales_x), col = c(1, 2),  lwd = c(1, 2)) #la roja IGAE

# Prueba de estacionariedad para las ventas
ur.kpss(sales_x[,1]) %>% summary() #los datos en niveles no son estacionarios
ur.kpss(diff(sales_x[,1])) %>% summary() # los datos diferenciados si son estacionarios

# Prueba de estacionariedad para el IGAE
ur.kpss(sales_x[,2]) %>% summary() #los datos en niveles no son estacionarios
ur.kpss(diff(sales_x[,2])) %>% summary() # los datos diferenciados si son estacionarios

##
# Modelo ARIMAX
##

# AR representa una regresion de la variable cotra valores pasados de ella misma
# MA representa la media movil ponderada de los errores pasados del pronóstico.
# X representa una variable o un conjunto de variables explicativas

# Combinando los dos modelos
# p = parte autorregresiva
# d = grado de difenrenciacion
# q = parte media movil

# Selección automatica del modelo ARIMA
modelo_x <- auto.arima(sales_x[,1], xreg = sales_x[,2], seasonal=T, stepwise=T, approximation=T)
summary(modelo_x)
checkresiduals(modelo_x) # Los residuales no estan correlacionados

# Pronóstico
autoplot(forecast(modelo_x, xreg = rep(mean(sales_x[,2]),9), h=9))

