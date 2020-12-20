library('forecast')
library('tseries')
library(readxl)

#import data
tscd19 = read_excel((file.choose()))

death = tscd19$death
confirm = tscd19$confirm

#death forecasting 
plot(death)
abline(reg = lm(death~time(death)))

d_ts = ts(death, start = c(2020, 23), frequency = 366)

decomp1 = stl(d_ts, s.window="periodic")
plot(decomp1)

#build models
model1 = auto.arima(d_ts)
auto.arima(death, ic = 'aic', trace = TRUE)

forecast1 = forecast(model1, level = c(95), h = 180)
plot(forecast1)

#plot residuals 
plot(forecast1$residuals)
qqnorm(forecast1$residuals)

#accuracy of models
summary(model1)

#confirmed case forecasting 
plot(confirm)
abline(reg = lm(confirm~time(confirm)))

c_ts = ts(confirm, start = c(2020, 23), frequency = 366)

decomp2 = stl(c_ts, s.window="periodic")
plot(decomp2)


#build models
model2 = auto.arima(c_ts)
auto.arima(confirm, ic = 'aic', trace = TRUE)

forecast2 = forecast(model2, level = c(95), h = 180)
plot(forecast2)

#plot residuals 
plot(forecast2$residuals)
qqnorm(forecast2$residuals)

#accuracy of models
summary(model2)


#export forecasting to csv
X = forecast[c('ds','yhat_lower','yhat_upper','yhat')]
write.csv(forecast1,"C:\\Users\\alex7\\Desktop\\DeathP.csv", row.names = FALSE)
write.csv(forecast2,"C:\\Users\\alex7\\Desktop\\confirmP.csv", row.names = FALSE)
