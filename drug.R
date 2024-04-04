
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

# Split dataset 
full_set = data$value
len_fullset = length(full_set)
len_valid = 36 # 36 for 3 years forecast
train_set = head(full_set, len_fullset-len_valid)
valid_set = tail(full_set, len_valid)


# Apply Lag-12 Differencing to Remove Seasonality Component
D1 = diff(train_set, lag=12)
# Plot Time Series After Differencing
plot(1:156,y = D1, xlim=c(1,156), ylim=c(-3,10), main = "Time Series After Differencing (d=0, D=1)")
lines(1:156, D1, type="l")

# Apply Lag-1 Differencing to Remove Trend Component
d1_D1 = diff(D1)
# Plot Time Series After Differencing
plot(1:155,y = d1_D1, xlim=c(1,155), ylim=c(-7,10), main = "Time Series After Differencing (d=1, D=1)")
lines(1:155, d1_D1, type="l")

# View ACF and PACF Plots
# ACF
# (based on lag 12, 24, 36 etc.) P = 0 & Q = 2, (based on lag 1 to 11) p = 0 & q = 4
acf(d1_D1, lag.max=50)
# PACF
# (based on lag 12, 24, 36 etc.) P = 0 & Q = 0, (based on lag 1 to 11) p = 4 & q = 0
pacf(d1_D1, lag.max=50)

# SARIMA(p,d,q,P,D,Q,s) fits:
# SARIMA(0,1,4,0,1,2,12)
fit1 = arima(train_set, order=c(0,1,4), seasonal=list(order=c(0,1,2), period=12))
tsdiag(fit1)

# SARIMA(0,1,4,0,1,0,12)
fit2 = arima(train_set, order=c(0,1,4), seasonal=list(order=c(0,1,0), period=12))
tsdiag(fit2)

# SARIMA(4,1,0,0,1,2,12)
fit3 = arima(train_set, order=c(4,1,0), seasonal=list(order=c(0,1,2), period=12))
tsdiag(fit3)

# SARIMA(4,1,0,0,1,0,12)
fit4 = arima(train_set, order=c(4,1,0), seasonal=list(order=c(0,1,0), period=12))
tsdiag(fit4)

# Check AIC
print(fit1) # AIC: 276.86
print(fit2) # AIC: 285.38
print(fit3) # AIC: 281
print(fit4) # AIC: 287.65
# fit1 has lowest AIC 

# Select Fit
# SARIMA(0,1,4,0,1,2,12)
fit_select=fit1

# View ACF of Residuals
acf(residuals(fit_select), lag.max = 50)

# Plot Time Series
dates = c(data$date[1], data$date[50], data$date[100], data$date[150], data$date[200])
plot(1:204,y = data$value, xlim=c(1,210), ylim=c(0,30), xlab = "Year-Month", ylab = "Drug Sales", main = "Monthly Drug Sales", xaxt="n")
axis(1, at=c(1, 50, 100, 150, 200), labels=dates)
lines(1:204, data$value, type="l")

# Fitted Model
fitted_model = train_set-fit_select$residual

# Plot fitted model with red line
lines(1:168, fitted_model, type="l", col="red")


# Forecast for 3 years ahead
forecast = predict(fit_select, n.ahead=36)
forecast_line = c(fitted_model[168],forecast$pred)
lines(168:204, forecast_line, col="purple")
lines(169:204, forecast$pred, type="o", col="purple")

# Print Overall Accuracy
print(accuracy(c(fitted_model, forecast$pred), full_set))