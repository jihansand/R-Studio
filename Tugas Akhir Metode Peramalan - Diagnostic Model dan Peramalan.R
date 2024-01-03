library(TSA)
library(tseries)
library(forecast)
library(lmtest)
library(ggplot2)

# masukan data
garuda <- read.csv("C:\\Users\\jihan\\Downloads\\GIAA.JK.csv"); garuda

# buat variabel time series
ts <- ts(garuda[,5], start = c(2018,6), frequency = 12); ts
tsdisplay(ts)
# karena dari plot terlihat meannya tidak konstan, maka akan dilakukan differencing
dts <- diff(ts, 1)
tsdisplay(dts)
plot(dts)

# cek stasioneritas
adf.test(dts)  # data stasioner

# ACF PACF belum bisa menentukan model, mari liat EACF jg
eacf(dts)

# dari EACF didapatkan calon model yaitu ARIMA(0,1,1), ARIMA(1,1,1), dan ARIMA(0,0,2)
model1 <- Arima(ts, order = c(0,1,1), include.constant = TRUE)
model2 <- Arima(ts, order = c(1,1,1), include.constant = TRUE)
model3 <- Arima(ts, order = c(0,1,2), include.constant = TRUE)
cbind(model1, model2, model3)
# dr AIC pilih model ARIMA(0,1,1)

# Analisis residual
checkresiduals(model1) # non-autokorelasi terpenuhi
# ^^ dr grafik histogramnya keliatan normal
adf.test(model1$residuals) # stasioner terpenuhi, p-value < 0.05
jarque.bera.test(model1$residuals) # normal, p value > 0.05

# Overfitting
# menambah order MA atau AR
overfit1 <- Arima(ts, order = c(1,1,1), include.constant = TRUE)
overfit2 <- Arima(ts, order = c(0,1,2), include.constant = TRUE)
coeftest(overfit1)
coeftest(overfit2)
# kedua model, parameternya tidak berbeda signifikan dr 0
coeftest(model1) # estimasi parameter tidak berbeda jauh dari model awal
# kesimpulan: dengan prinsip parsimony, model awal paling bagus yaitu ARIMA(0,1,1)

# Forecast
peramalan <- forecast(model1, h = 5);peramalan
autoplot(peramalan, main = 'Peramalan Indeks Harga Saham 5 Bulan Berikutnya') + theme(title = element_text(size = 10)
)
# yg warnanya lebih tua 80% CI, yg lebih muda 95% CI

# plot yang diperbesar
value <- window(ts, start = c(2021,1))
autoplot(value, main = "Peramalan Indeks Harga Saham 5 Bulan Berikutnya")+theme(title = element_text(size = 10))+
  autolayer(peramalan)

# Cross validation
# buat variabel time series sampa januari 2023
data_train <- window(ts, end = c(2023,1)); train
model_cross <- Arima(data_train, order = c(0,1,1), include.constant = TRUE)
peramalan2 <- forecast(model_cross, h = 5); peramalan2
actual <- window(ts, start = c(2023,2), frequency = 12); actual
cbind(actual, peramalan2)

autoplot(peramalan2)
plot(peramalan2)

