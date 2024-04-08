
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
len_valid = 12 # 12 for 1 year forecast
train_set = head(full_set, len_fullset-len_valid)
valid_set = tail(full_set, len_valid)

# Find Lambda for Box-Cox 
library(forecast)
lambda = BoxCox.lambda(train_set)
print(paste0("Lambda: ", lambda))

# Apply Box-Cox Transformation to Remove Increasing Variance
train_set = BoxCox(train_set,lambda)
# Plot Time Series after Box-Cox Transformation
plot(1:192,y = train_set, xlim=c(1,192), ylim=c(0.5,3.5), main = "Time Series after Box-Cox Transformation")
lines(1:192, train_set, type="l")

# Apply Lag-12 Differencing to Remove Seasonal Component
D1 = diff(train_set, lag=12)
# Plot Time Series after Differencing
plot(1:180,y = D1, xlim=c(1,180), ylim=c(-0.3,0.3), main = "Time Series after Differencing (d=0, D=1)")
lines(1:180, D1, type="l")

# Apply Lag-1 Differencing to Remove Trend Component
d1_D1 = diff(D1)
# Plot Time Series after Differencing
plot(1:179,y = d1_D1, xlim=c(1,179), ylim=c(-0.3,0.3), main = "Time Series after Differencing (d=1, D=1)")
lines(1:179, d1_D1, type="l")

# View ACF and PACF Plots
# ACF
# (based on lag 12, 24, 36 etc.) P = 0 & Q = 6, (based on lag 1 to 11) p = 0 & q = 10
acf(d1_D1, lag.max=100)
# PACF
# (based on lag 12, 24, 36 etc.) P = 0 & Q = 0, (based on lag 1 to 11) p = 8 & q = 0
pacf(d1_D1, lag.max=100)

# SARIMA(p,d,q,P,D,Q,s) fits:
# SARIMA(0,1,10,0,1,6,12)
fit1 = arima(train_set, order=c(0,1,10), seasonal=list(order=c(0,1,6), period=12))
tsdiag(fit1)

# SARIMA(0,1,10,0,1,0,12)
fit2 = arima(train_set, order=c(0,1,10), seasonal=list(order=c(0,1,0), period=12))
tsdiag(fit2)

# SARIMA(8,1,0,0,1,6,12)
fit3 = arima(train_set, order=c(8,1,0), seasonal=list(order=c(0,1,6), period=12))
tsdiag(fit3)

# SARIMA(8,1,0,0,1,0,12)
fit4 = arima(train_set, order=c(8,1,0), seasonal=list(order=c(0,1,0), period=12))
tsdiag(fit4)

# Compare AIC
print("Compare AIC Values")
print(AIC(fit1)) # AIC: -557.67
print(AIC(fit2)) # AIC: -541.05
print(AIC(fit3)) # AIC: -560.1
print(AIC(fit4)) # AIC: -531.31
# fit3 has lowest AIC

# Compare BIC
print("Compare BIC Values")
print(BIC(fit1)) # BIC: -503.4869
print(BIC(fit2)) # BIC: -505.986
print(BIC(fit3)) # BIC: -512.2897
print(BIC(fit4)) # BIC: -502.6282
# fit3 has lowest BIC 

# Select Fit
# SARIMA(8,1,0,0,1,6,12)
fit_select=fit3


# View ACF of Residuals
acf(residuals(fit_select), lag.max = 100) # Residuals not significant


# Plot Original Time Series
dates = c(data$date[1], data$date[50], data$date[100], data$date[150], data$date[200])
plot(1:204,y = data$value, xlim=c(1,210), ylim=c(0,40), xlab = "Year-Month", ylab = "Drug Sales", main = "Monthly Drug Sales", xaxt="n")
axis(1, at=c(1, 50, 100, 150, 200), labels=dates)
lines(1:204, data$value, type="l")

# Fitted Model
fitted_model = InvBoxCox(train_set-fit_select$residual, lambda=lambda)

# Add fitted model as red line to plot
lines(1:192, fitted_model, type="l", col="red")

# Forecast for 1 year ahead and add to plot
forecast_pred = InvBoxCox(predict(fit_select, n.ahead=12)$pred, lambda=lambda)
forecast_line = c(fitted_model[192], forecast_pred)
lines(192:204, forecast_line, col="purple")
lines(193:204, forecast_pred, type="o", col="purple")

# Print Overall Accuracy
print(accuracy(c(fitted_model, forecast_pred), full_set))