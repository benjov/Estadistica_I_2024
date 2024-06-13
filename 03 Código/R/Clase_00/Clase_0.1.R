###Como alternativa podemos usar bibliotecas que contengan un proceso como el que ocuparemos 
install.packages("AER")

# Sin Dependencias
library(AER)

# Con  Dependencias
library(car)
library(carData)
library(lmtest)
library(zoo)
library(sandwich)
library(survival)
library(AER)

#****************************************************************************************
#****************************************************************************************
# Caso Práctico (Cálculo de riesgo sistémico de AMZN)

### Librería para descargar series financieras y graficarlas
install.packages("quantmod")
install.packages("highcharter")
install.packages("ggplot2")

library(xts)
library(TTR)
library(quantmod)
library(highcharter)
library(ggplot2)

options("getSymbols.warning4.0" = FALSE)

### Descargamos la serie de las acciones de Amazon 
getSymbols("AMZN")

head(AMZN, 2)

### Gráfico
hchart(AMZN)

### Descargamos la serie de Nasdaq 
getSymbols("NDAQ")

head(NDAQ, 2)

### Gráfico
hchart(NDAQ)

## 
class(AMZN)


#
