# Exploratory analysis of local 8121

data <- read.csv2('totales1.txt', header = T, dec = '.')
num_estab <- 24543 # We change this number when changing the establishment 
head(data)
nrow(data)

# We extract data from num_estab establishment

data_estab_previous <- data[which(data$LOCAL == num_estab),]
head(data_estab_previous)
data_estab_previous$VENTAS <- as.numeric(data_estab_previous$VENTAS)
index_local_8121 <- which(data_estab_previous$VENTAS == -200)
init_val_index <-min(which(data_estab_previous$VENTAS!=0))
data_estab <- data_estab_previous[init_val_index:nrow(data_estab_previous),]

# We prepare data so that the sales column takes the values 0 or 475   

data_estab$VENTAS[which(data_estab$VENTAS != 0)] <- data_estab$VENTAS[1] # We complete with the first non-null element 
data_estab$VENTAS[index_local_8121] <- 0

# We need to make an exploratory analysis about the sales 

data_estab$num_semana <- as.numeric(substr(data_estab$SEMANA, start = 5, stop = 6)) # Number of week in year 
plot(data_estab$num_semana, data_estab$VENTAS)

# We want to know the total number of sales in a given week, in order to find a pattern depending on the week of the year 

library(sqldf)
histogram_data <- sqldf('select num_semana, sum(VENTAS) from data_estab group by num_semana')
plot(histogram_data$`sum(VENTAS)`~histogram_data$num_semana, col ='red', pch = 16)
training_set_len <- floor((nrow(data_estab)-2)/2)

# Given a training set length off 292, we can assume that establishments having only 0 or 1 sale in 6 or 7 weeks
# are going to be 0 sales in the following weeks with the same number. These weeks are, in this case 

histogram_data$num_semana[which(histogram_data$`sum(VENTAS)` < 300)] 

# The same for the weeks having 6 or 7 sales in 6 or 7 weeks 

histogram_data$num_semana[which(histogram_data$`sum(VENTAS)` > 1900)] 



names(histogram_data) <- c('Semana', 'Ventas')
a <- 470 * 5
histogram_data$Ventas <- histogram_data$Ventas/a
histogram_data$Ventas[histogram_data$Semana %in% c(29, 30, 31, 32, 33, 34, 35)] <- 0
histogram_data$Ventas[histogram_data$Semana %in% c(28, 36)] <- 1/5

ggplot(histogram_data, aes(x = Semana, y = Ventas)) + geom_point(col = 'blue', size = 2) + xlab('Week') + 
  ylab('Rate of Purchases') + scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
  scale_x_continuous(breaks = seq(1, 54, 1)) +
  theme(axis.text.y = element_text(size = 15), axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
