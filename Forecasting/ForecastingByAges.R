library('forecast')
library('tseries')
library(readxl)


#import data
Covid2544<- read_excel("C:/Users/alex7/Downloads/25-44_COVID DeathsxWeek.xlsx")
Covid45<- read_excel("C:/Users/alex7/Downloads/_45_COVID DeathsxWeek.xlsx")
Covidall<- read_excel("C:/Users/alex7/Downloads/Allages_COVID DeathsxWeek.xlsx")

death25 = Covid2544$`COVID-19 Deaths`
death45 = Covid45$`COVID-19 Deaths`
deathall = Covidall$`COVID-19 Deaths`

#death forecasting 
plot(death25)
abline(reg = lm(death25~time(death25)))
plot(death45)
abline(reg = lm(death45~time(death45)))
plot(deathall)
abline(reg = lm(deathall~time(deathall)))

ts25 = ts(death25, start = c(202, 5), frequency = 53)
ts45 = ts(death45, start = c(2020, 5), frequency = 53)
tsall = ts(deathall, start = c(2020, 5), frequency = 53)

#decompose but failed
decomp25 = stl(ts25, s.window="periodic")
decomp45 = stl(ts45, s.window="periodic")
decompall = stl(tsall, s.window="periodic")
plot(decomp25)
plot(decomp45)
plot(decompall)

#build models
model1 = auto.arima(ts25)
model2 = auto.arima(ts45)
model3 = auto.arima(tsall)
auto.arima(death25, ic = 'aic', trace = TRUE)
auto.arima(death45, ic = 'aic', trace = TRUE)
auto.arima(deathall, ic = 'aic', trace = TRUE)

forecast1 = forecast(model1, level = c(95), h = 26)
forecast2 = forecast(model2, level = c(95), h = 26)
forecast3 = forecast(model3, level = c(95), h = 26)
autoplot(forecast1)
autoplot(forecast2)
autoplot(forecast3)

#plot residuals 
plot(forecast1$residuals)
qqnorm(forecast1$residuals)
plot(forecast2$residuals)
qqnorm(forecast2$residuals)
plot(forecast3$residuals)
qqnorm(forecast3$residuals)

#accuracy of models
summary(model1)
summary(model2)
summary(model3)


#export forecasting to csv
write.csv(forecast1,"C:\\Users\\alex7\\Desktop\\death25.csv", row.names = FALSE)
write.csv(forecast2,"C:\\Users\\alex7\\Desktop\\death45.csv", row.names = FALSE)
write.csv(forecast3,"C:\\Users\\alex7\\Desktop\\deathall.csv", row.names = FALSE)
