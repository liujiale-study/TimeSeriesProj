
# Load Given Drug Sales Data
path_to_file = "data/drug.txt"
data=read.csv(path_to_file) 

# Clean Date Labels: Remove the Last 3 Characters of Labels
data$date = substr(data$date,1,nchar(data$date)-3)

# Plot Time Series
dates = c(data$date[1], data$date[50], data$date[100], data$date[150], data$date[200])
plot(1:204,y = data$value, xlim=c(1,204), ylim=c(0,30), xlab = "Year-Month", ylab = "Drug Sales", main = "Monthly Drug Sales", xaxt="n")
axis(1, at=c(1, 50, 100, 150, 200), labels=dates)
lines(1:204, data$value, type="l")


# Apply Lag-12 Differencing to Remove Seasonality Component
D1 = diff(data$value, lag=12)
# Plot Time Series After Differencing
plot(1:192,y = D1, xlim=c(1,192), ylim=c(-3,10), main = "Time Series After Differencing (d=0, D=1)")
lines(1:192, D1, type="l")

# Apply Lag-1 Differencing to Remove Trend Component
d1_D1 = diff(D1)
# Plot Time Series After Differencing
plot(1:191,y = d1_D1, xlim=c(1,191), ylim=c(-7,10), main = "Time Series After Differencing (d=1, D=1)")
lines(1:191, d1_D1, type="l")

# View ACF and PACF Plots
# ACF
# (based on lag 12, 24, 36 etc.) P = 0 & Q = 2, (based on lag 1 to 11) p = 0 & q = 6
acf(d1_D1, lag.max=100)
# PACF
# (based on lag 12, 24, 36 etc.) P = 0 & Q = 0, (based on lag 1 to 11) p = 11 & q = 0
pacf(d1_D1, lag.max=100)

# SARIMA(p,d,q,P,D,Q,s) fits:
# SARIMA(0,1,6,0,1,2,12)
fit1 = arima(data$value, order=c(0,1,6), seasonal=list(order=c(0,1,2), period=12))
tsdiag(fit1)

# SARIMA(11,1,0,0,1,2,12)
fit2 = arima(data$value, order=c(11,1,0), seasonal=list(order=c(0,1,2), period=12))
tsdiag(fit2)

# SARIMA(0,1,6,0,1,0,12)
fit3 = arima(data$value, order=c(0,1,6), seasonal=list(order=c(0,1,0), period=12))
tsdiag(fit3)

# SARIMA(11,1,0,0,1,0,12)
fit4 = arima(data$value, order=c(11,1,0), seasonal=list(order=c(0,1,0), period=12))
tsdiag(fit4)

# Check AIC
print(fit1) # AIC: 523.17
print(fit2) # AIC: 526.73
print(fit3) # AIC: 551.88
print(fit4) # AIC: 544.9
# fit1 has lowest AIC 

# Select Fit
# SARIMA(0,1,6,0,1,2,12)
fit_select=fit1

# Improve selected model based on ACF of Residuals
# Significant Residuals at time lag 23, change q to 23
fit_select = arima(data$value, order=c(0,1,23), seasonal=list(order=c(0,1,2), period=12))
print(fit_select) # Fit AIC improved from 523.17 to 512.94
tsdiag(fit_select)

# Plot Time Series
dates = c(data$date[1], data$date[50], data$date[100], data$date[150], data$date[200])
plot(1:204,y = data$value, xlim=c(1,210), ylim=c(0,30), xlab = "Year-Month", ylab = "Drug Sales", main = "Monthly Drug Sales", xaxt="n")
axis(1, at=c(1, 50, 100, 150, 200), labels=dates)
lines(1:204, data$value, type="l")

# Plot fitted model with red line
lines(1:204, data$value-fit_select$residuals, type="l", col="red")


# Forecast for 3 months ahead
forecast = predict(fit_select, n.ahead=3)
forecast_line = c((data$value-fit_select$residuals)[204],forecast$pred)
lines(204:207, forecast_line, col="purple")
lines(205:207, forecast$pred, type="o", col="purple")
lines(205:207, forecast$pred-1.96*forecast$se, col="blue")
lines(205:207, forecast$pred+1.96*forecast$se, col="blue")
