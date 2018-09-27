#import library and raw data
library(fpp)
data <- read.csv("jeju.csv", header = FALSE, col.names = c("date","numbers"))
data


#unit chage
jejuair <- data$numbers/1000000
jejuair


#data set process
jejuair_ts <- ts(jejuair,frequency=12, start = c(1997,1), end = c(2018,7))
str(jejuair_ts)


#plotting the general data
plot(jejuair_ts,xlab="Time",ylab="Number of Passenger Arrivals in Jeju(million)",main="Number of Passengers Arrived in Jeju (Jan.1997 - Jul.2018)")
jejuair_ts


#data cleaning (from 2000)
jejuair_ts_new <- window(jejuair_ts, start = c(2000,1))
plot(jejuair_ts_new,xlab="Time",ylab="Number of Passenger Arrivals in Jeju(million)",main="Number of Passengers Arrived in Jeju (Jan.2000 - Jul.2018)")

#ACF
jeju_acf <- Acf(jejuair_ts, lag.max = 36, main = "ACF of Jeju air passenger")
jeju_pacf <- Pacf(jejuair_ts, lag.max = 36, main = "PACF of Jeju air passenger")

tsdisplay(jejuair_ts, main = "Jeju time series display")


#seasonal plot
ggseasonplot(jejuair_ts, main = "Seasonal plot", year.labels = TRUE, ylab = "Number of air passengers in Jeju (million)")


#train and test data set (75% train, 25% test)
jeju_train <- window (jejuair_ts_new, start = 2000, end = c(2014,12))
jeju_train
jeju_test <- window(jejuair_ts_new, start = 2015, end = c(2018,7))
jeju_test


#train and test
jeju_ses <- ses(jeju_train, h = 43, initial = "optimal") #SES
jeju_ses
summary(jeju_ses)

jeju_holt <- holt(jeju_train, h = 43, initial = "optimal") #Holt linear
jeju_holt
summary(jeju_holt)

jeju_exp <- holt(jeju_train, h = 43, exponential = TRUE, initial = "optimal") #exponential
jeju_exp
summary(jeju_exp)

jeju_damped_add <- holt(jeju_train, h = 43, damped = TRUE, initial = "optimal") #Additive Damped trend
jeju_damped_add
summary(jeju_damped_add)

jeju_damped_multi <- holt(jeju_train, h = 43, damped = TRUE, initial = "optimal", exponential = TRUE) #Multiplicative
jeju_damped_multi
summary(jeju_damped_multi)

jeju_hw <- hw(jeju_train, h = 43, seasonal = "multiplicative", initial = "optimal") #holt-Winters
jeju_hw
summary(jeju_hw)

jeju_hw_damped <- hw(jeju_train, h = 43, seasonal = "multiplicative", initial = "optimal", damped = TRUE) #holt-Winters
jeju_hw_damped
summary(jeju_hw_damped)

jeju_ets <- ets(jeju_train) #ets without damped
summary(jeju_ets)
plot(jeju_ets)
jeju_ets

jeju_ets_forecast <- forecast(jeju_ets, h = 43) #ets forecast
jeju_ets_forecast
summary(jeju_ets_forecast)
plot(jeju_ets_forecast)

jeju_ets_damped <- ets(jeju_train, damped = TRUE) #ets with damped
summary(jeju_ets_damped)
plot(jeju_ets_damped)
jeju_ets_damped

jeju_ets_forecast_damped <- forecast(jeju_ets_damped, h = 43) #ets with damped forecast
jeju_ets_forecast_damped
summary(jeju_ets_forecast_damped)
plot(jeju_ets_forecast_damped)




#train forecasting plot

plot(jejuair_ts_new, main = "Methods without Seasonality", xlab = "Time", ylab = "Number of Passenger Arrivals in Jeju(million)",  ylim = c(0,3.5))
lines(jeju_ses$mean, col = 2)
lines(jeju_holt$mean, col = 3)
lines(jeju_exp$mean, col = 4)
lines(jeju_damped_add$mean, col = 5)
lines(jeju_damped_multi$mean, col = 6)
legend("topleft", lty = 1, col = c(1,2,3,4,5,6), legend = c("Actual","SES", "Holt's Linear","Exponential", "Additive-damped","Multiplicative-damped"))


plot(jejuair_ts_new, main = "Methods with Seasonality", xlab = "Time", ylab = "Number of Passenger Arrivals in Jeju(million)",ylim = c(0,3.5))
lines(jeju_hw$mean, col = 2)
lines(jeju_hw_damped$mean, col = 3)
lines(jeju_ets_forecast$mean, col = 4)
lines(jeju_ets_forecast_damped$mean, col = 6)
legend("topleft", lty = 1, col = c(1,2,3,4,6), legend = c("Actual","Holt-winter","Holt-winter multiplicative damped","ETS(M,A,M)","ETS(M,Ad,A)"))


jejuair_ts_new_zoom <- window(jejuair_ts_new, start= c(2014,1))
plot(jejuair_ts_new_zoom, main = "Methods with Seasonality", xlab = "Time", ylab = "Number of Passenger Arrivals in Jeju(million)",ylim = c(1.5,3.5), xlim= c(2014,2019))
lines(jeju_hw$mean, col = 2)
lines(jeju_hw_damped$mean, col = 3)
lines(jeju_ets_forecast$mean, col = 4)
lines(jeju_ets_forecast_damped$mean, col = 6)
legend("topleft", lty = 1, col = c(1,2,3,4,6), legend = c("Actual","Holt-winter","Holt-winter multiplicative damped","ETS(M,A,M)","ETS(M,Ad,A)"))

#accuracy test
accuracy(jeju_ses, jeju_test)
accuracy(jeju_holt, jeju_test)
accuracy(jeju_exp, jeju_test)
accuracy(jeju_damped_add, jeju_test)
accuracy(jeju_damped_multi, jeju_test)
accuracy(jeju_hw, jeju_test)
accuracy(jeju_hw_damped, jeju_test)
accuracy(jeju_ets_forecast,jeju_test)
accuracy(jeju_ets_forecast_damped, jeju_test)


#ets(M,Ad,A) stats
ets_stats <- jeju_ets_forecast_damped$model$states[,1:3]
colnames(ets_stats) <- c("level", "slope", "seaonal")
plot(ets_stats, main = "ETS(M,Ad,A) Stats")

#ets(M,A,M) stats
ets_mam_stats <- jeju_ets_forecast$model$states[,1:3]
colnames(ets_mam_stats) <- c ("level","slope","seasonal")
plot(ets_mam_stats, main = "ETS(M,A,M) Stats")



#ets forecast full data
jeju_forecast_ets <- ets(jejuair_ts_new, model = "MAA", damped = TRUE)
jeju_forecast_ets
plot(jeju_forecast_ets)

#ets forecast for h = 24
jeju_forecast_ets_future <- forecast(jeju_forecast_ets, h = 24)
plot(jeju_forecast_ets_future)
summary(jeju_forecast_ets_future)

accuracy(jeju_forecast_ets_future)


#linear regression for Q8

reg <- tslm(jejuair_ts_new ~ trend + season, lamda = "auto")
summary (reg)
reg_forecast <- forecast(reg, h = 24)
reg_forecast

plot(reg_forecast, main = "Out of sample forecast for Air passenger in Jeju with linear regression", 
     xlab = "Year", ylab = "Number of air passengers in Jeju (miilion)")

accuracy(reg_forecast)
