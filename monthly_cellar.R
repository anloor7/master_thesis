

########## TIME SERIES ANALYSIS. MONTHLY ANALYSIS 1 


install.packages('TSA')
install.packages('tseries')
install.packages('ggplot2')
install.packages('forecast')
install.packages('lubridate')

library(TSA)
library(tseries)
library(ggplot2)
library(forecast)
library(lubridate)


########## DATA PREPARATION ##########


# We are going to study the following monthly annual series. First, we prepare the data. We have monthly sales observations from
# January of 2008 to August of 2019. 

data <- read.table("monthly_cellar.txt", header = T, sep =';')
colnames(data) <- c('MES', 'MES DESCRIPTION', 'SALES')
head(data)
nrow(data)

# We are going to divide the series into the training period and the validation period. We are going to choose the last 12 months as the
# validation period

total_series <- ts(data$SALES, start = c(2008, 1), frequency = 12)
training_series <- window(total_series, end=end(total_series) - c(0,12), frequency=12)
validation_series <- window(total_series, start=end(training_series) + c(0,1), frequency=12)





########## SARIMA MODEL


########## PREPROCESSING


# Total series plotplot(total_series, type="o", lwd=3, col = 'red', xlab='month', ylab='Litres of beer')

plot(total_series, type="o", lwd=3, col = 'orange', xlab='Month', ylab='Litres of beer')

# Analyzing training series
# Time series plot, acf and pacf

plot(training_series, type="o", lwd=3, col = 'orange', xlab='Month', ylab='Litres of beer')
acf(training_series)
pacf(training_series)

# It seems that we have a trend, seasonality and heterocedascity, remarkably at the end of the series
# We try to stabilize the variance by using the log transformation

# Time series plot of the transformed series

plot(log(training_series), type="o", lwd=3, col = 'red', xlab='Month', ylab='log(Litres of beer)')

# Indeed, log transformation stabilizes the variance

log_training_series <- log(training_series)

# The series has a trend. Rigorously the trend is analyzed through the times series plot and the acf
# We see that acf decreases slowly to zero  

plot(log_training_series, type="o", lwd=3, col = 'red')
acf(log_training_series, lag.max=length(log_training_series)/4, main="", lwd=3, col = 'brown')

# The presence of trend leads us to apply a regular differencing. Lets see if the trend gets removed 

dif_log_training_series <- diff(log_training_series, lag=1)
plot(dif_log_training_series, type="o", lwd=3, col = 'red')
acf(dif_log_training_series, lag.max=length(dif_log_training_series)/2, main="", lwd=3)

# The trend has been removed. In this case, we have d = 1 as a parameter in the SARIMA model

# We can also observe in the acf the presence of seasonality as a monthly series. We apply then a seasonal differencing with s = 12

dif_dif_log_training_series <- diff(dif_log_training_series, lag=12)
par(mfrow = c(2,1))
plot(dif_dif_log_training_series, type="o", lwd=3, col = 'red', ylab ='')
acf(dif_dif_log_training_series, lag.max=length(dif_dif_log_training_series)/2, main="", lwd=3, xlab ='Lag')

# Seasonality has been removed. We have D = 1 as a parameter in the SARIMA model
# It seems that the new series is stationary. We now try to adapt a SARIMA model by identifying the parameters p, q, P and Q

# Representations for latex document

par(mfrow = c(3, 1))
acf(log_training_series, lag.max=length(dif_log_training_series)/2, main="", lwd=3, col = 'brown')
acf(dif_log_training_series, lag.max=length(dif_log_training_series)/2, main="", lwd=3, col = 'brown')
acf(dif_dif_log_training_series, lag.max=length(dif_dif_log_training_series)/2, main="", lwd=3, xlab ='Lag', col = 'brown')

plot(log_training_series, type="o", lwd=3, col = 'red', ylab = 'Litres of beer')
plot(dif_log_training_series, type="o", lwd=3, col = 'red', ylab = '')
plot(dif_dif_log_training_series, type="o", lwd=3, col = 'red', ylab ='')
par(mfrow = c(1, 1))

########## MODEL FITTING


auto.arima(log_training_series, max.p = 3, max.q = 3, d = 1, D = 1, max.P = 2, max.Q = 2)

# La funcion ARIMA nos devuelve un modelo distinto al que hemos ajustado, así que ajustaremos este modelo 
# SARIMA(2, 1, 1) x (0, 1, 2)_12

fitting <- arimax(x=log_training_series, order=c(2, 1, 1), seasonal=list(order=c(0, 1, 2)))
fitting
abs(fitting$coef)/(1.96*sqrt(diag(fitting$var.coef)))
fitting$coef[3] <- 0
fitting$coef[5] <- 0

# The parameters ma1 and sma2 are not significatively different from zero. We could give them the value of zero


########## DIAGNOSIS

# Graphical methods
# Time series plot of the residuals

par(mfrow = c(1, 1))
plot(residuals(fitting), type="o", lwd=3, col = 'red')
abline(h=0, lwd=3)

# QQ-plot

qqnorm(residuals(fitting), lwd=3) 
qqline(residuals(fitting), lwd=3)

# It seems that there is underlying normality 

#  Hypothesis tests

#  Fas y Ljung-Box

tsdiag(fitting, lwd=3)

# We accept incorrelation

# Test H0: mean=0

t.test(residuals(fitting), mu=0)

# We accept the null hypithesis with alpha = 0.01...

# Normality tests

jarque.bera.test(residuals(fitting))
shapiro.test(residuals(fitting))

# We accept the normality of residuals

# Conclusion: The fitting model SARIMA(2, 1, 1) x (0, 1, 2)_12 (without constant) can be used as a generator of the 
# time series log(sales). We have gaussian innovations


########## FORECASTING


# We now predict the values of the series for the following 12 months. We then compare the forecasts with the real values

par(mfrow = c(3, 1))
detach("package:forecast", unload=TRUE)
n.ahead <- 12
plot(fitting, n.ahead=n.ahead, transform=exp, col = "blue", lty=1, pch=0, lwd = 3, ylab='Litres of beer', 
     xlab = 'Month', xlim = c(2018, 2020))
lines(validation_series, col="green", type="o", lty=1, pch=19, lwd = 2)
legend(x="topleft", legend = c("Training series", "SARIMA forecasts", "Validation series"), 
       col=c("black","black", "green"), lty=c(1,1,1), pch=c(1, 0, 19), bty="n", lwd=1.5)


# Puntual forecasts

output <- plot(fitting, n.ahead=n.ahead, transform=exp, Plot=FALSE)
output$pred 

# Confidence intervals

output$lpi # extremos inferiores
output$upi # extremos superiores


########## PERFORMANCE EVALUATION


# We calculate now the four error measures of predictive accuracy 

validation_series; output$pred

# MAE (mean absolute error)

mae_sarima <- (1/length(validation_series))*sum(abs(validation_series-output$pred)); mae_sarima 

# Average error

average_error_sarima <- (1/length(validation_series))*sum(validation_series-output$pred); average_error_sarima

# MAPE (mean absolute percentage error)

mape_sarima <- 100*(1/length(validation_series))*sum(abs((validation_series - output$pred)/validation_series)); mape_sarima

# RMSE (root-mean-squared error)

rmse_sarima <- sqrt((1/length(validation_series))*sum((validation_series-output$pred)^2)); rmse_sarima

# Lets see the previous measures of performance evaluation with a Naive Forecast. A naive forecast is the most recent value of the series

naive <- training_series[length(training_series)]
naive <- rep(naive, length(validation_series)) 
mae_naive <- (1/length(validation_series))*sum(abs(validation_series-naive)); mae_naive
average_error_naive <- (1/length(validation_series))*sum(validation_series-naive); average_error_naive
mape_naive <- 100*(1/length(validation_series))*sum(abs((validation_series - naive)/validation_series)); mape_naive
rmse_naive <- sqrt((1/length(validation_series))*sum((validation_series-naive)^2)); rmse_naive

naive_series <- ts(naive, start = c(2018, 9), frequency = 12)
# plot(training_series, type = 'o', xlim = c(2015, 2020), ylim = c(0, 1400000))
# lines(validation_series, col="green", type="o", lty=1, pch=19, lwd=0.5)
# lines(naive_series, col = 'red', type ='o')
# legend(x="topleft", legend = c("Training Series", "Naive forecasts", "Valores observados"), 
       # col=c("black","red", "green"), lty=c(1, 1, 1), pch=c(1, 1, 19), bty="n", lwd=3)





##########  EXPONENTIAL SMOOTHING 


# We have seen that our series has heterocedascity, trend and seasonality. We are going to adapt smoothing methods taking into account
# the previous information. To begin with, we are going to adapt simple exponential smoothing


########## SIMPLE EXPONENTIAL SMOOTHING 


# We are goint to use the HoltWinters() function of the forecast package. To fit a simple exponential smoothing, we we need 
# to set the parameters beta=FALSE and gamma=FALSE. The function tries to find the optimal values of α and/or β and/or γ by 
# minimizing the squared one-step prediction error 

simple_exponential_smoothing <- HoltWinters(log_training_series, beta=FALSE, gamma=FALSE)
simple_exponential_smoothing
forecasts_1 <- exp(predict(simple_exponential_smoothing, n.ahead = 12))

# Plotting true values and forecasts

# plot(training_series, type = 'o', xlim = c(2017, 2020), ylim = c(0, 1400000))
# lines(validation_series, col="green", type="o", lty=1, pch=19, lwd=0.5)
# lines(forecasts_1, col = 'red', type = 'o')
# legend(x="topleft", legend = c("Training Series", "Forecasts with simple exponential smoothing", "Observed values"), 
       # col=c("black","red", "green"), lty=c(1, 1, 1), pch=c(1, 1, 19), bty="n", lwd=3)

# Measures of predictive accuracy for simple exponential smoothing

mae_1 <- (1/length(validation_series)) * sum(abs(validation_series - forecasts_1)); mae_1
average_error_1 <- (1/length(validation_series)) * sum(validation_series - forecasts_1); average_error_1
mape_1 <- 100*(1/length(validation_series)) * sum(abs((validation_series - forecasts_1)/validation_series)); mape_1
rmse_1 <- sqrt((1/length(validation_series)) * sum((validation_series - forecasts_1)^2)); rmse_1


########## ADVANCED EXPONENTIAL SMOOTHING

# Additive seasonality

additive_seasonality <- HoltWinters(log_training_series, seasonal = 'additive')
additive_seasonality
forecasts_2 <- exp(predict(additive_seasonality, n.ahead = 12))

# Diagnosis

t.test(additive_seasonality$fitted[, 1] - additive_seasonality$x, mu = 0)
jarque.bera.test(additive_seasonality$fitted[, 1] - additive_seasonality$x)
shapiro.test(additive_seasonality$fitted[, 1] - additive_seasonality$x)

# Plotting true values and forecasts

plot(training_series, type = 'o', xlim = c(2018, 2020), ylim = c(0, 1500000), ylab = 'Litres of beer', lwd = 3)
lines(validation_series, col="green", type="o", lty=1, pch=19, lwd = 2)
lines(forecasts_2, col = 'blue', type = 'o', pch = 0)
legend(x="topleft", legend = c("Training series", "Forecasts with additive seasonality", "Validation series"), 
       col=c("black","blue", "green"), lty=c(1, 1, 1), pch=c(1, 0, 19), bty="n", lwd=1.5)

# Measures of predictive accuracy for additive seasonality

mae_2 <- (1/length(validation_series)) * sum(abs(validation_series - forecasts_2)); mae_2
average_error_2 <- (1/length(validation_series)) * sum(validation_series - forecasts_2); average_error_2
mape_2 <- 100*(1/length(validation_series)) * sum(abs((validation_series - forecasts_2)/validation_series)); mape_2
rmse_2 <- sqrt((1/length(validation_series)) * sum((validation_series - forecasts_2)^2)); rmse_2

# Multiplicative seasonality 

multiplicative_seasonality <- HoltWinters(log_training_series, seasonal = 'multiplicative')
multiplicative_seasonality
forecasts_3 <- exp(predict(multiplicative_seasonality, n.ahead = 12))

# Diagnosis

t.test(multiplicative_seasonality$fitted[, 1] - multiplicative_seasonality$x, mu = 0)
jarque.bera.test(multiplicative_seasonality$fitted[, 1] - multiplicative_seasonality$x)
shapiro.test(multiplicative_seasonality$fitted[, 1] - multiplicative_seasonality$x)
# qqnorm(multiplicative_seasonality$fitted[, 1] - multiplicative_seasonality$x)
# qqline(multiplicative_seasonality$fitted[, 1] - multiplicative_seasonality$x)

# Plotting true values and forecasts

plot(training_series, type = 'o', xlim = c(2018, 2020), ylim = c(0, 1500000), ylab = 'Litres of beer', lwd = 3)
lines(validation_series, col="green", type="o", lty=1, pch=19, lwd = 2)
lines(forecasts_3, col = 'red', type = 'o', pch = 0)
legend(x="topleft", legend = c("Training series", "Forecasts with multiplicative seasonality", "Validation series"), 
       col=c("black","red", "green"), lty=c(1, 1, 1), pch=c(1, 0, 19), bty="n", lwd=1.5)

# Measures of predictive accuracy for additive seasonality

mae_3 <- (1/length(validation_series)) * sum(abs(validation_series - forecasts_3)); mae_3
average_error_3 <- (1/length(validation_series)) * sum(validation_series - forecasts_3); average_error_3
mape_3 <- 100*(1/length(validation_series)) * sum(abs((validation_series - forecasts_3)/validation_series)); mape_3
rmse_3 <- sqrt((1/length(validation_series)) * sum((validation_series - forecasts_3)^2)); rmse_3





########## PREDICTION FOR THE FOLLOWING 12 MONTHS 

par(mfrow = c(1,1))
log_total_series <- log(total_series)
total_fitting <- arimax(x = log_total_series, order=c(2, 1, 1), seasonal=list(order=c(0, 1, 2)))
n.ahead <- 12
plot(total_fitting, n.ahead=n.ahead, transform=exp, col = "blue", lty=1, pch=0, lwd = 3, ylab='Litres of beer', 
     xlab = 'Month', xlim = c(2018, 2021))
legend(x="topleft", legend = c("Total series", "SARIMA forecasts"), 
       col=c("black","black"), lty=c(1,1), pch=c(1, 0), bty="n", lwd=1.5)


# Puntual forecasts

total_output <- plot(total_fitting, n.ahead=n.ahead, transform=exp, Plot=FALSE)
total_output$pred 

# Confidence intervals

total_output$lpi # extremos inferiores
total_output$upi # extremos superiores



