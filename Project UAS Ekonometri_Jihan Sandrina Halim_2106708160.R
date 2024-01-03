library(readxl)
library(lmtest)
library(tseries)
library(forecast)
library(TSA)
library(fGarch)
library(rugarch)
library(FinTS)

oil <- read_excel("D:/Downloads/crude-oil-300.xlsx"); oil

# buat variabel time series
ts <- ts(oil[,2], start = c(2002,1), frequency = 12); ts
tsdisplay(ts)

plot(ts, main = "Time Series Plot", ylab = "Values")

# membagi data train dan data test
ts_train <- window(ts, end = c(2019, 6)); ts_train
ts_test <- window(ts, start = c(2019, 7)); ts_test

tsdisplay(ts_train, lag.max = 20)

# cek stasioneritas
adf.test(ts)
adf.test(ts_train)
adf.test(ts_test)

# differencing sekali
dts_t <- diff(ts_train, 1)
tsdisplay(dts_t, lag.max = 20)
plot(dts_t)
plot(ts_train)

# EACF
eacf(dts)
eacf(dts_t)

# ARIMA(0,1,1), ARIMA(0,1,2), dan ARIMA(1,1,0)
model1 <- arima(ts_train, order = c(0,1,1))
model2 <- arima(ts_train, order = c(0,1,2))
model3 <- arima(ts_train, order = c(1,1,0))
cbind(model1, model2, model3)

coeftest(model1)
coeftest(model2)
coeftest(model3)

# pilih model ARIMA(0,1,2) karena AIC terkecil
coeftest(model2)

# Overfitting
# menambah order MA atau AR
overfit1 <- arima(ts_train, order = c(1,1,2))
overfit2 <- arima(ts_train, order = c(0,1,3))
summary(overfit1)
summary(overfit2)

coeftest(overfit1)
coeftest(overfit2)
# tetap ARIMA(0,1,2)

# Analisis residual
# Ljung-Box
checkresiduals(model2) # non-autokorelasi terpenuhi
# ^^ dr grafik histogramnya keliatan normal
adf.test(model2$residuals) # stasioner terpenuhi, p-value < 0.05

qqnorm(model2$residuals)
qqline(model2$residuals, col = "red", lwd = 2)
plot(model2$residuals)
tsdisplay(model2$residuals, lag.max = 20)

# Uji lagrange-multiplier
ArchTest(model2$residuals, demean = TRUE, lags = 4)
ArchTest(model2$residuals, demean = TRUE, lags = 12)
ArchTest(model2$residuals, demean = TRUE, lags = 20)

arch.test(model2)

# Model ARCH
arch <- garch(dts_t,order = c(0,2))
summary(arch)

garch12 <- garch(dts_t,c(1,2))
summary(garch12)

garch22 <- garch(dts_t,c(2,2))
summary(garch22)

cbind(AIC(arch), AIC(garch12), AIC(garch22))

checkresiduals(arch)
qqnorm(arch$residuals)
qqline(arch$residuals, col = "red", lwd = 2)
tsdisplay(arch$residuals, lag.max = 25)

# Peramalan
## ARIMA+ARCH
AR_ARCH <- garchFit(~arma(0,2)+garch(2,0), data = dts_t, trace = F)
summary(AR_ARCH)

predict(AR_ARCH, n.ahead = 36, trace = F, plot = T)

## ARIMA(0,1,2)
a <- forecast(model2, h = length(ts_test))
forecast_ARIMA <- as.data.frame(forecast(model2, h = length(ts_test)))
forecast_ARIMA
plot(forecast(model2, h = length(ts_test)))

data_forecast1 <- forecast_ARIMA$`Point Forecast`
peramalan1 <- as.data.frame(data_forecast1)
head(peramalan1)
accuracy_arima <- accuracy(peramalan1$data_forecast1, ts_test)
accuracy_arima

oil2 <- read_excel("D:/Downloads/oil_arch.xlsx"); oil2
MAPE <- 1/36*sum(abs((dataframe$Price-oil2$Predict1)/dataframe$Price)*100)
MAPE
