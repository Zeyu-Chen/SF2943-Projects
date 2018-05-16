library(itsmr)
library(astsa)
library(forecast)


data <- read.csv("D:\\MEGA\\KTH Courses\\Year 1\\17-18 P4\\SF2943 Time Series Analysis\\Project\\Part A\\CO2.csv", header = T)

CO2 <- data$CO2
CO2 <- ts(CO2, frequency = 12)
plotc(CO2)


CO2smooth<-smooth.ma(CO2,q=12)
plotc(CO2, CO2smooth)

dCO2 <- CO2-CO2smooth

sCO2 <- dCO2-season(dCO2,12)
plot(sCO2)


acf(sCO2,lag.max = 30)

pacf(sCO2, lag.max = 30)

model <- sarima(sCO2, p = 1, d = 0, q = 0)

model <- sarima(sCO2, p = 2, d = 0, q = 0)


model_02 <- auto.arima(CO2)
plot(forecast(model_02,h=24))