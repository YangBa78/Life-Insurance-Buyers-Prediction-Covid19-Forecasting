library(dplyr)
library(prophet)
library(lubridate)
library(ggplot2)

#data
library(readxl)
timeseries_DeathRate <- read_excel("C:/Users/alex7/Desktop/timeseries_DeathRate.xlsx")
View(timeseries_DeathRate)
DD <- timeseries_DeathRate

DD$date <- mdy(DD$date)
DD$death <- as.numeric(DD$death)

# Forecasting
ds <- DD$date
y <- DD$death
df <- data.frame(ds, y)

m <- prophet(df)

# Prediction
future <- make_future_dataframe(m, periods = 93)
forecast <- predict(m, future)

# Plot forecast
plot(m, forecast)
prophet_plot_components(m, forecast)
dyplot.prophet(m, forecast)

#model performance
pred = forecast$yhat[1:252]
actual = m$history$y
plot(actual, pred)
summary(lm(pred~actual))

#export data
X = forecast[c('ds','yhat_lower','yhat_upper','yhat')]
write.csv(X,"C:\\Users\\alex7\\Desktop\\DeathPred.csv", row.names = FALSE)


x = c(1, 2, 3, 4)