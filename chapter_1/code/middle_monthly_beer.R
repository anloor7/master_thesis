
# Vamos a hacer un estudio de la serie de tiempo mensual de ventas totales de cerveza de bodega en la delegacion de Ferrol
# desde 2008

library(TSA)
library(tseries)
library(ggplot2)
library(lubridate)
# library(forecast)
library(ggfortify)
library(gridExtra)

data <- read.table("mensuales_ferrol.txt", header = T, sep =';')
colnames(data) <- c('MONTH', 'MONTH DESCRIPTION', 'SALES')
head(data)
data


total_series <- ts(data$SALES, start = c(2008, 1), frequency = 12)
training_series <- window(total_series, end=end(total_series) - c(0,12), frequency=12)
validation_series <- window(total_series, start=end(training_series) + c(0,1), frequency=12)

########## SARIMA MODEL


########## PREPROCESSING 


# Total series plotplot(total_series, type="o", lwd=3, col = 'red', xlab='month', ylab='Litres of beer')

autoplot(total_series, colour = 'orange', xlab = 'Time', ylab = 'Litres of beer',  ts.geom = 'line', ts.size = 1) +
  scale_x_date(date_breaks = "years", date_labels = '%Y') + 
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1))  +
  ylim(0, 6e4) +
  scale_y_continuous(limits = c(1.5e4, 6e4), breaks = seq(0, 6e4, by = 1.5e4)) +
  ggtitle('Sales of Cellar Beer regarding a middle level in the hierarchy') +
  theme(plot.title=element_text(size=20, 
                                face="bold", 
                                family="American Typewriter",
                                color="orange",
                                hjust= 0.5,
                                lineheight=3,
                                margin=margin(0,0,8,0)))



# Analyzing training series
# Time series plot, acf and pacf

plot(training_series, type="o", lwd=3, col = 'orange', xlab='Month', ylab='Litres of beer')
autoplot(training_series, colour = 'orange', xlab = 'Time', ylab = 'Litres of beer',  ts.geom = 'line', ts.size = 1) +
  scale_x_date(date_breaks = "years", date_labels = '%Y') + 
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1)) 


# It seems that we have a trend, seasonality and (a bit) heterocedascity, remarkably at the end of the series
# We try to stabilize the variance by using the log transformation

# Time series plot of the transformed series

plot(log(training_series), type="o", lwd=3, col = 'red', xlab='Month', ylab='log(Litres of beer)')
transformed_series1 <- autoplot(log(training_series), colour = 'red', xlab = 'Time', 
                                ylab = 'Log(Litres of beer)',  ts.geom = 'line', ts.size = 1) +
  scale_x_date(date_breaks = "years", date_labels = '%Y') + 
  ggtitle('Transformed series') +
  theme(plot.title=element_text(size=20, 
                                face="bold", 
                                family="American Typewriter",
                                color="red",
                                hjust= 0.5,
                                lineheight=3,
                                margin=margin(0,0,20,0)),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1))


# Indeed, log transformation stabilizes the variance

log_training_series <- log(training_series)

# The series has a trend. Rigorously the trend is analyzed through the times series plot and the acf
# We see that acf decreases slowly to zero  

plot(log_training_series, type="o", lwd=3, col = 'red')
acf(log_training_series, lag.max=length(log_training_series)/4, main="", lwd=3, 
    col = 'brown')
acf1 <- ggAcf(log_training_series, size = 1, col = 'brown') + 
  scale_x_continuous(breaks = seq(0, length(log_training_series)/3, by = 1)) +
  ggtitle('Autocorrelation functions') +
  theme(plot.title=element_text(size=20, 
                                face="bold", 
                                family="American Typewriter",
                                color="brown",
                                hjust= 0.5,
                                lineheight=3,
                                margin=margin(0,0,20,0)),
        axis.text.x = element_text(size = 15))


# The presence of trend leads us to apply a regular differencing. Lets see if the trend gets removed 

dif_log_training_series <- diff(log_training_series, lag=1)
plot(dif_log_training_series, type="o", lwd=3, col = 'red')
transformed_series2 <- autoplot(dif_log_training_series, colour = 'red', xlab = 'Time', ylab = 'Regular differencing',  
                                ts.geom = 'line', ts.size = 1) +
  scale_x_date(date_breaks = "years", date_labels = '%Y') + 
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1)) 
acf2 <- ggAcf(dif_log_training_series, size = 1, col = 'brown') + 
  scale_x_continuous(breaks = seq(0, length(log_training_series)/4, by = 1)) +
  theme(axis.text.x = element_text(size = 15)) + ggtitle('')


# The trend has been removed. In this case, we have d = 1 as a parameter in the SARIMA model

# We can also observe in the acf the presence of seasonality as a monthly series. We apply then a seasonal differencing with s = 12

dif_dif_log_training_series <- diff(dif_log_training_series, lag=12)
transformed_series3 <- autoplot(dif_dif_log_training_series, colour = 'red', xlab = 'Time', ylab = 'Seasonal differencing',  
                                ts.geom = 'line', ts.size = 1) +
  scale_x_date(date_breaks = "years", date_labels = '%Y') + 
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1)) 
acf3 <- ggAcf(dif_dif_log_training_series, size = 1, col = 'brown') + 
  scale_x_continuous(breaks = seq(0, length(log_training_series)/4, by = 1)) +
  theme(axis.text.x = element_text(size = 15)) + ggtitle('')
grid.arrange(transformed_series1, transformed_series2, transformed_series3) 
grid.arrange(acf1, acf2, acf3) 



# Seasonality has been removed. We have D = 1 as a parameter in the SARIMA model
# It seems that the new series is stationary. We now try to adapt a SARIMA model by identifying the parameters p, q, P and Q

# Representations for latex document


########## MODEL FITTING


auto.arima(log_training_series, max.p = 3, max.q = 3, d = 1, D = 1, max.P = 2, max.Q = 2)

fitting <- arimax(x=log_training_series, order=c(0, 1, 1), seasonal=list(order=c(0, 1, 1)))
fitting
abs(fitting$coef)/(1.96*sqrt(diag(fitting$var.coef)))


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

# Conclusion: The fitting model SARIMA(0, 1, 1) x (0, 1, 1)_12 (without constant) can be used as a generator of the 
# time series log(sales). We have gaussian innovations


########## FORECASTING


# We now predict the values of the series for the following 12 months. We then compare the forecasts with the real values

predictions_arima <- exp(predict(fitting, n.ahead = 12)$pred)
subset_arima <- window(total_series, start = c(2018, 1), end = c(2019, 8) )
df11 <- data.frame(Time = time(subset_arima), series = as.vector(subset_arima)) 
df12 <- data.frame(Time = time(predictions_arima), series = as.vector(predictions_arima))
predictions  <- ggplot(data = df11,  aes(x=Time, y=series) )   +       
  ylab('Litres of beer')             +
  geom_line(aes(colour = 'Real series'), col = 'orange', size = 1)                      +
  geom_point(aes(colour = 'Real series'), col = 'orange', pch=16, cex = 4) 
predictions1 <- predictions  + geom_line(aes(Time, series, colour = 'Predictions'), col = 'green', df12, size = 1)   +
  geom_point(aes(Time, series, colour = 'Predictions'), col = 'green', df12, pch = 16, cex = 4) +
  ggtitle('Predictions') +
  theme(plot.title=element_text(size=20, 
                                face="bold", 
                                family="American Typewriter",
                                color="Green",
                                hjust= 0.5,
                                lineheight=3,
                                margin=margin(0,0,20,0)),
        axis.text.x = element_text(size = 15))

# Puntual forecasts

n.ahead <- 12
output <- plot(fitting, n.ahead = n.ahead, transform=exp, Plot=FALSE)
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

naive <- training_series[(length(training_series)-11):length(training_series)]
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

df22 <- data.frame(Time = time(forecasts_2), series = as.vector(forecasts_2))
predictions2 <- predictions  + geom_line(aes(Time, series, colour = 'Predictions'), col = 'green', df22, size = 1)   +
  geom_point(aes(Time, series, colour = 'Predictions'), col = 'green', df22, pch = 16, cex = 4) +
  theme(axis.text.x = element_text(size = 15))


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

df32 <- data.frame(Time = time(forecasts_3), series = as.vector(forecasts_3))
predictions3  <- predictions + geom_line(aes(Time, series, colour = 'Predictions'), col = 'green', df32, size = 1)   +
  geom_point(aes(Time, series, colour = 'Predictions'), col = 'green', df32, pch = 16, cex = 4) +
  theme(axis.text.x = element_text(size = 15))

grid.arrange(predictions1, predictions2, predictions3)

# Measures of predictive accuracy for multiplicative seasonality

mae_3 <- (1/length(validation_series)) * sum(abs(validation_series - forecasts_3)); mae_3
average_error_3 <- (1/length(validation_series)) * sum(validation_series - forecasts_3); average_error_3
mape_3 <- 100*(1/length(validation_series)) * sum(abs((validation_series - forecasts_3)/validation_series)); mape_3
rmse_3 <- sqrt((1/length(validation_series)) * sum((validation_series - forecasts_3)^2)); rmse_3


########## PREDICTION FOR THE FOLLOWING 12 MONTHS 


n.ahead <- 12
par(mfrow = c(1,1))
log_total_series <- log(total_series)
total_fitting <- HoltWinters(log_total_series, alpha = 0.2069475, beta = 0.04617612, gamma =0.3543078 , seasonal = 'additive')
output <- exp(predict(total_fitting, 12, level = 0.95, prediction.interval = TRUE))
df_pred <- data.frame(Time = time(output[,1]), series = as.vector(output[,1]))
df_lpi <- data.frame(Time = time(output[,3]), series = as.vector(output[,3]))
df_upi <- data.frame(Time = time(output[,2]), series = as.vector(output[,2]))  

predictions_total <- ggplot(data = df_pred,  aes(x=Time, y=series) )   +       
  ylab('Litres of beer')             +
  geom_line(aes(colour = 'Real series'), col = 'blue', size = 1) +
  geom_point(aes(colour = 'Real series'), col = 'blue', pch=16, cex = 4) +
  ggtitle('Predictions for the following 12 months') +
  ylim(2e4, 8.5e4) + 
  scale_y_continuous(limits = c(2e4, 8.5e4), breaks = seq(0, 8.5e4, by = 1e4)) +
  scale_x_continuous(limits = c(2019.6, 2020.6), breaks = c(2020, 2020.6)) +
  geom_line(aes(x=Time, y=series), data = df_lpi, col = 'blue', size = 1, linetype = "dashed") +
  geom_line(aes(x=Time, y=series), data = df_upi, col = 'blue', size = 1, linetype = "dashed") +
  theme(plot.title=element_text(size=20, 
                                face="bold", 
                                family="American Typewriter",
                                color="blue", hjust = 0.5,
                                margin=margin(0,0,8,0)))
predictions_total

