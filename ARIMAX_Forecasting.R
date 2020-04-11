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
sales_x <- ts(sales, start = c(2014,01), frequency = 12)

# Grafico de datos en niveles
ts.plot(scale(sales_x), col = c(1, 2),  lwd = c(1, 2)) #la roja IGAE

# Prueba de estacionariedad
ur.kpss(sales_x$SALES) %>% summary() #los datos en niveles no son estacionarios
ur.kpss(diff(sales_x$...2)) %>% summary() # los datos diferenciados si son estacionarios

##
# Modelo ARIMA
##

# AR representa una regresion de la variable cotra valores pasados de ella misma
# MA representa la media movil ponderada de los errores pasados del pronóstico.

# Combinando los dos modelos
# p = parte autorregresiva
# d = grado de difenrenciacion
# q = parte media movil

# Selección automatica del modelo ARIMA
igae <- sales_x$...2
modelo_x <- auto.arima(sales_x[,"SALES"], xreg = igae, seasonal=T, stepwise=T, approximation=T)
checkresiduals(modelo_x) # Los residuales estan correlacionados

# Pronóstico
autoplot(forecast(modelo_x, xreg = igae, h=9))

?forecast

