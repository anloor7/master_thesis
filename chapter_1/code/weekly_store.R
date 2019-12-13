
library(TSA)
library(tseries)
library(ggplot2)
library(lubridate)
library(forecast)
library(ggfortify)
library(gridExtra)

data <- read.csv('store.txt', header = T, sep = ';')
head(data)
data_estab <- data[which(data$LOCAL_ID == 53),]
index <- which(data_estab$SEMANA == 201254)
data_estab <- data_estab[-index,]
ventas <- data_estab$VENTAS
ventas[which(ventas != 475 & ventas != 0)] <- 475
ventas
series <- ts(ventas, start = c(2008, 1), frequency = 53)
plot(series)


autoplot(series, colour = 'orange', xlab = 'Time', ylab = 'Litres of beer',  ts.geom = 'line', ts.size = 1) +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1))  +
  #scale_x_date(date_breaks = "years", date_labels = '%Y') + 
  ylim(0, 500) +
  ggtitle('Sales of Cellar Beer regarding a given store') +
  theme(plot.title=element_text(size=20, 
                                face="bold", 
                                family="American Typewriter",
                                color="orange",
                                hjust= 0.5,
                                lineheight=3,
                                margin=margin(0,0,8,0)))


training_series <- window(series, end=end(series) - c(0,53), frequency=53)
validation_series <- window(series, start=end(training_series) + c(0,1), frequency=53)

auto.arima(training_series, max.p = 3, max.q = 3, max.P = 2, max.Q = 2)
fitting <- arimax(x=training_series, order=c(0, 1, 3), seasonal=list(order=c(1, 0, 0)))
fitting



df_segment <- data.frame(x1 = 2018, y1 = 237.5, x2=2019.7, y2 = 237.5)
predictions_arima <- predict(fitting, n.ahead = 52)$pred
subset_arima <- window(series, start = c(2018, 1), end = c(2019, 34) )
df11 <- data.frame(Time = time(subset_arima), series = as.vector(subset_arima)) 
df12 <- data.frame(Time = time(predictions_arima), series = as.vector(predictions_arima))
predictions  <- ggplot(data = df11,  aes(x=Time, y=series) )   +       
  ylab('Litres of beer')             +
  geom_line(aes(colour = 'Real series'), col = 'orange', size = 1)                      +
  geom_point(aes(colour = 'Real series'), col = 'orange', pch=16, cex = 4) 
predictions1 <- predictions  + geom_line(aes(Time, series, colour = 'Predictions'), col = 'green', df12, size = 1)   +
  geom_point(aes(Time, series, colour = 'Predictions'), col = 'green', df12, pch = 16, cex = 4) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), col = 'orange', data = df_segment, size = 1)+
  ggtitle('Sales of cellar beer regarding a given store') +
  theme(plot.title=element_text(size=20, 
                                face="bold", 
                                family="American Typewriter",
                                color="Orange",
                                hjust= 0.5,
                                lineheight=3,
                                margin=margin(0,0,10,0)),
        axis.text.x = element_text(size = 15))

