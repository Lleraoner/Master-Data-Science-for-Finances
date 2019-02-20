library(openxlsx)
library(forecast)
library(xts)
library(ggplot2)
library(ggfortify)
library(TSA)
library(lmtest)

weeklyData <- to.weekly(d.data)
zWeeklyData <- as.zoo(weeklyData$d.data.Close)
names(zWeeklyData) <- 'Semanal'

#Primera aproximacion semanal
autoplot(zWeeklyData) + geom_point() +
  ylab("Ventas") + ggtitle("Cuota semanal") + xlab("Semanas") + 
  ggtitle('Representacion semanal')


monthlyData <- to.monthly(d.data)
zMonthlyData <- as.zoo(monthlyData$d.data.Close)
names(zMonthlyData) <- 'Mensual'

#Primera aproximacion mensual
autoplot(zMonthlyData) + geom_point() +
  ylab("Ventas") + ggtitle("Cuota mensual") + xlab("Semanas") + 
  ggtitle('Representacion mensual')

cOmit = 0
nObsWeek = length(zWeeklyData)
nObsMonth = length(zMonthlyData)

oVentasWeekly <- window(zWeeklyData, start = index(zWeeklyData[1]), end = index(zWeeklyData[nObsWeek - cOmit]))
oVentasMonthly <- window(zMonthlyData, start = index(zMonthlyData[1]), end = index(zMonthlyData[nObsMonth - cOmit]))

#Obsevamos que no podemos usar el modelo arima con la serie semanal asi que usaremos modelos ETS
# PRIMERO COMENZAMOS CON UN ARIMA
#ARIMA MODEL
fit.semanal = auto.arima(oVentasWeekly,lambda=0, ic = "aic") #lamba 0 es aplicar el logaritmo
summary(fit.semanal) #ARIMA(0,1,0)  

fit.mensual = auto.arima(oVentasMonthly,lambda=0, ic = "aic")
summary(fit.mensual) #ARIMA(2,0,2)(2,0,0) # TIENE COMPONENTE ESTACIONAL

#residual analysis
ggtsdisplay(fit.semanal$residuals)
ggtsdisplay(fit.mensual$residuals)
# no significativas las autocorrelaciones segun test BLJ

#box-Ljung Test
Box.test(fit.semanal$residuals,lag=4, fitdf=3, type="Lj") # NO OBTENEMOS SOLUCION
Box.test(fit.mensual$residuals,lag=4, fitdf=3, type="Lj") # ruido blanco

fsemanal.arima=forecast(fit.semanal, h=4)
fmensual.arima=forecast(fit.mensual, h=1)

Box.test(fsemanal.arima$residuals,lag=4, fitdf=3, type="Lj") # NO OBTENEMOS SOLUCION
Box.test(fmensual.arima$residuals,lag=4, fitdf=3, type="Lj") # ruido blanco

plot(fsemanal.arima)
plot(fmensual.arima)
fsemanal.arima #modelo con la prediccion de 4 semanas = agosto 2018
fmensual.arima # modelo con la prediccion de 1 mes = agosto 2018


# DESPUES PROBAMOS CON MODELO ETS
## Select automatic ETS
etsfit<- ets(oVentasWeekly, ic = "aic") # Estimacion del modelo automatica
etsfit2 <- ets(oVentasMonthly, ic = "aic")


etsfit # nuestro modelo semanal: ETS(A,N,N) error aditivo, tendencia ninguna, componente estacional ninguno.
etsfit2 # nuestro modelo mensual: ETS(M,N,M) error multiplicativo, tendencia ninguna, componente estacional multiplicativo.


#forecast model
fsemanal.ets = forecast(etsfit, h = 4) # Aqui tienes las predicciones
fmensual.ets = forecast(etsfit2, h = 1)
#Results
summary(fsemanal.ets) # Los resultados de las predicciones y sus intervalos de confianza
summary(fmensual.ets)

#Plot
plot(fsemanal.ets) # malas predicciones
plot(fmensual.ets) # mejores predicciones
fsemanal.ets
fmensual.ets

summary(fsemanal.ets) # Los resultados de las predicciones y sus intervalos de confianza
summary(fmensual.ets)
zMonthlyData