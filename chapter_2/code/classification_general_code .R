
########## GENERAL CODE FOR CLASSIFICATION IN A GIVEN ESTABLISHMENT ####################### 


#################### DATA ANALYSIS ####################

# t <- proc.time()

##########  DATA LOADING 

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

########## DEFINITION OF TRAINING SET 

# We create the training set. We have nrow(data_estab) observations. We are going to consider by default 50% of observations 
# in training set and other 50% in the test set, This could de changed manually 

# We first create variables indicating sales or not sales in the 1 or 2 previous weeks 

nrow(data_estab)-2 # We remove the first two elements, since we do not known anything about previous weeks  
training_set_len <- floor((nrow(data_estab)-2)/2) # Length of training set 
sales_values <- data_estab$VENTAS[3:(nrow(data_estab)-(training_set_len+1))]
length(sales_values)
var1 <- numeric(length(sales_values)) # Empty categorical variables
var2 <- numeric(length(sales_values))
data_frame <- data.frame(sales_values, var1, var2)

# Completion of first values 

if (data_estab$VENTAS[1]!= 0)
{data_frame$var2[1] <- 'Sales two weeks before'
   } else {
data_frame$var2[1]  <- 'No sales two weeks before'}

if (data_estab$VENTAS[2]!= 0)
{data_frame$var1[1] <- 'Sales previous week'
data_frame$var2[2]  <- 'Sales two weeks before'
} else {
  data_frame$var1[1]  <- 'No sales previous week'
  data_frame$var2[2]  <- 'No sales two weeks before' }


# We complete var1 and var2 with loops 

for (i in 2:(length(sales_values))) {
  if(sales_values[i-1] != 0) data_frame$var1[i] = 'Sales previous week'
  else data_frame$var1[i] = 'No sales previous week'
  
}

for (i in 3:(length(sales_values))) {
  if(sales_values[i-2] != 0) data_frame$var2[i] = 'Sales two weeks before'
  else data_frame$var2[i] = 'No sales two weeks before'
}

data_frame


########## DEFINITION OF TEST SET 


test_set_length <- nrow(data_estab)-2-training_set_len
new_sales_values <- data_estab$VENTAS[(length(data_estab$VENTAS)-training_set_len):length(data_estab$VENTAS)]
new_week <- data_estab$SEMANA[(length(data_estab$VENTAS)-training_set_len):length(data_estab$VENTAS)]
var3 <- numeric(length(new_sales_values)) # Categoric variables
var4 <- numeric(length(new_sales_values))
new_data_frame <- data.frame(new_sales_values, var3, var4, new_week)

# Completion of first values 


if (data_frame$sales_values[length(data_frame$sales_values)-1]!= 0)
{new_data_frame$var4[1] <- 'Sales two weeks before'
} else {
  new_data_frame$var4[1]  <- 'No sales two weeks before'}

if (data_frame$sales_values[length(data_frame$sales_values)]!= 0)
{new_data_frame$var3[1] <- 'Sales previous week'
new_data_frame$var4[2]  <- 'Sales two weeks before'
} else {
  new_data_frame$var3[1]  <- 'No sales previous week'
  new_data_frame$var4[2]  <- 'No sales two weeks before' }



# We complete var1 and var2 with loops 

for (i in 2: (length(new_sales_values))) {
  if(new_sales_values[i-1] != 0)
    new_data_frame$var3[i] = 'Sales previous week'
  else new_data_frame$var3[i] = 'No sales previous week'
  
}

for (i in 3:(length(new_sales_values))) {
  if(new_sales_values[i-2] != 0) new_data_frame$var4[i] = 'Sales two weeks before'
  else new_data_frame$var4[i] = 'No sales two weeks before'
  
}


new_data_frame

# We make categorical variables and change columns names in order to make predictions 

data_frame$sales_values<- factor(data_frame$sales_values)
data_frame$var1 <- factor(data_frame$var1)
data_frame$var2 <- factor(data_frame$var2)

new_data_frame$new_sales_values <- factor(new_data_frame$new_sales_values)
new_data_frame$var3 <- factor(new_data_frame$var3)
new_data_frame$var4 <- factor(new_data_frame$var4)
colnames(new_data_frame) <- c('sales_values', 'var1', 'var2')


#################### SUPERVISED LEARNING TECHNIQUES #####################

naive_performance <- max(table(new_data_frame$sales_values))/sum(table(new_data_frame$sales_values))

########## TREE AND RANDOM FOREST  

# Tree 

library(tree)
library(ROCR)

tree <- tree(sales_values~., data = data_frame)
plot(tree)
text(tree, pretty = 0)
pred_tree_class <- predict(tree, newdata = new_data_frame, type = 'class') # Predictions in the test set with binary response 
tree_table <- table(pred_tree_class,new_data_frame$sales) # Confusion matrix
performance_tree <- (sum(diag(tree_table)))/(sum(tree_table)) # Performance
pred_tree <- predict(tree, newdata = new_data_frame) # Predictions with probabilities
pred <- prediction(as.vector(pred_tree[,2]), new_data_frame$sales )
perf <- performance(pred,"tpr","fpr") 
plot(perf,colorize=TRUE) # ROC curve representation
area <- performance(pred,"tpr","fpr", measure = 'auc') 
area # AUC


# Random Forest 

library(randomForest)
rf <-randomForest(sales_values~.,  data = data_frame)
pred_rf_class <- predict(rf, newdata = new_data_frame, type = 'class') # Predictions in the test set with binary response 
rf_table <- table(pred_rf_class, new_data_frame$sales_values) # Confusion matrix
performance_rf <- (sum(diag(rf_table)))/(sum(rf_table)) # Performance
pred_rf <- predict(rf, newdata = new_data_frame, type ='prob') # Predictions with probabilities
pred_rf <- prediction(as.vector(pred_rf[,2]), new_data_frame$sales_values)
perf_rf <- performance(pred_rf,"tpr","fpr")
plot(perf_rf,colorize=TRUE) # ROC curve representation
area_rf <- performance(pred_rf,"tpr","fpr", measure = 'auc') 
area_rf # AUC


# Neural network 

library(nnet)
neural <- nnet(sales_values ~., data = data_frame, size = 5, maxit = 1000, decay = .001, rang = 0.05)
pred_neural_class <- predict(neural, newdata = new_data_frame, type = 'class') # Predictions in the test set with binary response 
neural_table <- table(pred_neural_class, new_data_frame$sales_values) # Confusion matrix
performance_neural <- (sum(diag(neural_table)))/(sum(neural_table)) # Performance

# Logistic regression

reg_log <- glm(sales_values ~., data = data_frame, family = binomial(link = 'logit'))
pred_reg_log <- predict(reg_log, newdata = new_data_frame, type = 'response')  # Predictions with probabilities
pred_reg_log_class <- rep('0', length(pred_reg_log)) 
pred_reg_log_class[pred_reg_log > 0.5] <- '475' # Predictions in the test set with binary response
reg_log_table <- table(pred_reg_log_class, new_data_frame$sales_values) # Confusion matrix 
performance_reg_log <- (sum(diag(reg_log_table)))/(sum(reg_log_table)) # Performance

# KNN

library(class)
train.X <- cbind(data_frame$var1, data_frame$var2)
test.X <- cbind(new_data_frame$var1, new_data_frame$var2)
train_sales_values <- data_frame$sales_values

knn_pred <- knn(train.X, test.X, train_sales_values, k = 5)
knn_table <- table(knn_pred, new_data_frame$sales_values) # Confusion matrix 
performance_knn <- (sum(diag(knn_table)))/(sum(knn_table)) # Performance

# Linear discriminant analysis

library(MASS)
lda <- lda(sales_values ~., data = data_frame)
pred_lda_class <- predict(lda, newdata = new_data_frame, type = 'response')$class
lda_table <- table(pred_lda_class, new_data_frame$sales_values) # Confusion matrix 
performance_lda <- (sum(diag(lda_table)))/(sum(lda_table)) # Performance

# Quadratic discriminant analysis

qda <- qda(sales_values ~., data = data_frame)
pred_qda_class <- predict(qda, newdata = new_data_frame, type = 'response')$class
qda_table <-table(pred_qda_class, new_data_frame$sales_values) # Confusion matrix 
performance_qda <- (sum(diag(qda_table)))/(sum(qda_table)) # Performance

########## We repeat the analysis considering the variable season

# We create a categorical variable in both data frames 

data_frame_season<- data_frame
data_frame_season$SEMANA <- data_estab$SEMANA[3:(nrow(data_estab)-(training_set_len+1))]
data_frame_season$SEMANA <- substr(data_frame_season$SEMANA, 5, 6) # We select the two last digits of the week 
data_frame_season$SEMANA <- as.numeric(data_frame_season$SEMANA) # We convert to numeric 
summer_index <- which(data_frame_season$SEMANA %in% seq(27, 34)) # Positions to complete with season 
fall_index <- which(data_frame_season$SEMANA %in% seq(35, 50)) # Positions to complete with fall 
winter_index <- which(data_frame_season$SEMANA %in% c(seq(50, 55), seq(1,15))) # Positions to complete with winter
spring_index <- which(data_frame_season$SEMANA %in% seq(15, 26)) # Positions to complete with spring
season <- numeric(nrow(data_frame_season))
data_frame_season$season <- season
data_frame_season$season[summer_index] <- 'Summer'
data_frame_season$season[fall_index] <- 'Fall'
data_frame_season$season[spring_index] <- 'Spring'
data_frame_season$season[winter_index] <- 'Winter'
data_frame_season$season <- factor(data_frame_season$season) # We make a categorical variable 


new_data_frame_season <- new_data_frame
new_data_frame_season$SEMANA <-  data_estab$SEMANA[(length(data_estab$VENTAS)-training_set_len):length(data_estab$VENTAS)]
new_data_frame_season$SEMANA <- substr(new_data_frame_season$SEMANA, 5, 6) # We select the two last digits of the week 
new_data_frame_season$SEMANA <- as.numeric(new_data_frame_season$ SEMANA) # We convert to numeric 
new_summer_index <- which(new_data_frame_season$SEMANA %in% seq(27, 34))  # Positions to complete with season 
new_fall_index <- which(new_data_frame_season$SEMANA %in% seq(35, 50)) # Positions to complete with fall 
new_winter_index <- which(new_data_frame_season$SEMANA %in% c(seq(50, 55), seq(1,15))) # Positions to complete with winter
new_spring_index <- which(new_data_frame_season$SEMANA %in% seq(15, 26)) # Positions to complete with spring
season <- numeric(nrow(new_data_frame_season))
new_data_frame_season$season <- season
new_data_frame_season$season[new_summer_index] <- 'Summer'
new_data_frame_season$season[new_fall_index] <- 'Fall'
new_data_frame_season$season[new_spring_index] <- 'Spring'
new_data_frame_season$season[new_winter_index] <- 'Winter'
new_data_frame_season$season <- factor(new_data_frame_season$season) # We make a categorical variable 


# Tree and random forest  

tree_season <- tree(sales_values~. -SEMANA, data = data_frame_season)
plot(tree_season)
text(tree_season, pretty = 0)
pred_tree_season_class <- predict(tree_season, newdata = new_data_frame_season, type = 'class') # Predictions in the test set with binary response 
tree_table_season <- table(pred_tree_season_class,new_data_frame$sales) # Confusion matrix
performance_tree_season <- (sum(diag(tree_table_season)))/(sum(tree_table_season)) # Performance
pred_tree_season <- predict(tree_season, newdata = new_data_frame_season) # Predictions with probabilities
pred_season <- prediction(as.vector(pred_tree_season[,2]), new_data_frame_season$sales_values)
perf_season <- performance(pred_season,"tpr","fpr") 
plot(perf_season,colorize=TRUE) # ROC curve representation
area_season <- performance(pred_season,"tpr","fpr", measure = 'auc') 
area_season # AUC

rf_season <-randomForest(sales_values~. -SEMANA ,  data = data_frame_season, n.trees = 100)
pred_rf_season_class <- predict(rf_season, newdata = new_data_frame_season, type = 'class')  # Predictions in the test set with binary response 
rf_table_season <- table(pred_rf_season_class, new_data_frame$sales_values) # Confusion matrix
performance_rf_season <- (sum(diag(rf_table_season)))/(sum(rf_table_season)) # Performance
pred_rf_season <- predict(rf_season, newdata = new_data_frame_season, type ='prob') # Predictions with probabilities
pred_rf_season <- prediction(as.vector(pred_rf_season[,2]), new_data_frame_season$sales_values)
perf_rf_season <- performance(pred_rf_season,"tpr","fpr")
plot(perf_rf_season,colorize=TRUE) # ROC curve representation
area_rf_season <- performance(pred_rf_season,"tpr","fpr", measure = 'auc') 
area_rf_season # AUC


# Neural network  

library(nnet)
neural_season <- nnet(sales_values ~. -SEMANA, data = data_frame_season, size = 5, maxit = 500, decay = .001, rang = 0.05)
pred_neural_season_class <- predict(neural_season, newdata = new_data_frame_season, type = 'class')  # Predictions in the test set with binary response 
neural_table_season <- table(pred_neural_season_class, new_data_frame$sales_values) # Confusion matrix
performance_neural_season <- (sum(diag(neural_table_season)))/(sum(neural_table_season)) # Performance

# Logistic regression 

reg_log_season <- glm(sales_values ~. -SEMANA, data = data_frame_season, family = binomial(link = 'logit'))
pred_reg_log_season <- predict(reg_log_season, newdata = new_data_frame_season, type = 'response') # Predictions with probabilities
pred_reg_log_season_class <- rep('0', length(pred_reg_log_season)) 
pred_reg_log_season_class[pred_reg_log_season > 0.5] <- '475' # Predictions with binary response 
reg_log_table_season <- table(pred_reg_log_season_class, new_data_frame$sales_values) # Confusion matrix 
performance_reg_log_season <- (sum(diag(reg_log_table_season)))/(sum(reg_log_table_season)) # Performance

# KNN

library(class)
train.X_season <- cbind(data_frame_season$var1, data_frame_season$var2, data_frame_season$season)
test.X_season <- cbind(new_data_frame_season$var1, new_data_frame_season$var2, new_data_frame_season$season)
train_sales_values_season <- data_frame_season$sales_values

knn_pred_season <- knn(train.X_season, test.X_season, train_sales_values_season, k = 5)
knn_table_season <- table(knn_pred_season, new_data_frame$sales_values) # Confusion matrix 
performance_knn_season <- (sum(diag(knn_table_season)))/(sum(knn_table_season)) # Performance

# Linear discriminant analysis

lda_season <- lda(sales_values ~. -SEMANA, data = data_frame_season)
pred_lda_season_class <- predict(lda_season, newdata = new_data_frame_season, type = 'response')$class
lda_table_season <- table(pred_lda_season_class, new_data_frame_season$sales_values) # Confusion matrix 
performance_lda_season <- (sum(diag(lda_table_season)))/(sum(lda_table_season)) # Performance

# Quadratic discriminant analysis

qda_season <- qda(sales_values ~. -SEMANA, data = data_frame_season)
pred_qda_season_class <- predict(qda_season, newdata = new_data_frame_season, type = 'response')$class
qda_table_season <- table(pred_qda_season_class, new_data_frame_season$sales_values)
performance_qda_season <-(sum(diag(qda_table_season)))/(sum(qda_table_season)) # Performance


########## We repeat the analysis considering a variable indicating public holiday or not public holiday in the 
########## same week and in the previous week

public_holiday_previous <- data_estab$LABORABLESCA
public_holiday <- public_holiday_previous[3:length(public_holiday_previous)]
training_public_holiday <- public_holiday[1:((length(public_holiday)-(training_set_len+1)))]
test_public_holiday <- public_holiday[((length(public_holiday)-training_set_len)):length(public_holiday)]
length(training_public_holiday) + length(test_public_holiday)

# Completing the training set and the test set 

training_public_holiday1 <- as.numeric(training_public_holiday)
index_training_public_holiday1 <- which(training_public_holiday1 == 5)
index_training_following_week1 <- index_training_public_holiday1 + 1
training_public_holiday1[index_training_following_week1] <- 'No public holiday previous week'
training_public_holiday1[-index_training_following_week1] <- 'Public holiday previous week'
training_public_holiday1[1] <- 'Public holiday previous week'
training_public_holiday1 <- training_public_holiday1[1:(length(training_public_holiday1)-1)] # Removing the last component added by R

test_public_holiday1 <- as.numeric(test_public_holiday)
index_test_public_holiday1 <- which(test_public_holiday1 == 5)
index_test_following_week1 <- index_test_public_holiday1 + 1
test_public_holiday1[index_test_following_week1] <- 'No public holiday previous week'
test_public_holiday1[-index_test_following_week1] <- 'Public holiday previous week'
test_public_holiday1[1] <- 'No public holiday previous week'
test_public_holiday1 <- test_public_holiday1[1:(length(test_public_holiday1)-1)] # Removing the last component adding by R

training_public_holiday2 <- as.numeric(training_public_holiday)
index_training_public_holiday2 <- which(training_public_holiday2 == 5)
training_public_holiday2[index_training_public_holiday2] <- 'No public holiday this week'
training_public_holiday2[-index_training_public_holiday2] <- 'Public holiday this week'

test_public_holiday2 <- as.numeric(test_public_holiday)
index_test_public_holiday2 <- which(test_public_holiday2 == 5)
test_public_holiday2[index_test_public_holiday2] <- 'No public holiday this week'
test_public_holiday2[-index_test_public_holiday2] <- 'Public holiday this week'

# Checking visually if we have created correctly the previous two variables 

public_holiday_checking1 <- c(1,1,training_public_holiday1, test_public_holiday1)
public_holiday_checking2 <- c(1,1,training_public_holiday2, test_public_holiday2)
data_estab$public_holiday_checking1 <- public_holiday_checking1
data_estab$public_holiday_checking2 <- public_holiday_checking2
data_estab[,c('LABORABLESCA', 'public_holiday_checking1', 'public_holiday_checking2')]

# We add the new variables to data_frame_season and new_data_frame_season 

data_frame_season_holiday <- data_frame_season
data_frame_season_holiday$public_holiday1 <- training_public_holiday1
new_data_frame_season_holiday <- new_data_frame_season
new_data_frame_season_holiday$public_holiday1 <- test_public_holiday1
data_frame_season_holiday$public_holiday1 <- factor(data_frame_season_holiday$public_holiday1)
new_data_frame_season_holiday$public_holiday1 <- factor(new_data_frame_season_holiday$public_holiday1)

data_frame_season_holiday$public_holiday2 <- training_public_holiday2
new_data_frame_season_holiday$public_holiday2 <- test_public_holiday2
data_frame_season_holiday$public_holiday2 <- factor(data_frame_season_holiday$public_holiday2)
new_data_frame_season_holiday$public_holiday2 <- factor(new_data_frame_season_holiday$public_holiday2)

# Tree and random forest  

tree_season_holiday <- tree(sales_values~. -SEMANA, data = data_frame_season_holiday)
plot(tree_season_holiday)
text(tree_season_holiday, pretty = 0)
pred_tree_season_holiday_class <- predict(tree_season_holiday, newdata = new_data_frame_season_holiday, type = 'class') # Predictions in the test set with binary response 
tree_table_season_holiday <- table(pred_tree_season_holiday_class,new_data_frame$sales) # Confusion matrix
performance_tree_season_holiday <- (sum(diag(tree_table_season_holiday)))/(sum(tree_table_season_holiday)) # Performance
pred_tree_season_holiday <- predict(tree_season_holiday, newdata = new_data_frame_season_holiday) # Predictions with probabilities
pred_season_holiday <- prediction(as.vector(pred_tree_season_holiday[,2]), new_data_frame_season_holiday$sales_values)
perf_season_holiday <- performance(pred_season_holiday,"tpr","fpr") 
plot(perf_season_holiday,colorize=TRUE) # ROC curve representation
area_season <- performance(pred_season_holiday,"tpr","fpr", measure = 'auc') 
area_season # AUC

rf_season_holiday <-randomForest(sales_values~. -SEMANA,  data = data_frame_season, n.trees = 100)
pred_rf_season_holiday_class <- predict(rf_season_holiday, newdata = new_data_frame_season_holiday, type = 'class')  # Predictions in the test set with binary response 
rf_table_season_holiday <- table(pred_rf_season_holiday_class, new_data_frame$sales_values) # Confusion matrix
performance_rf_season_holiday <- (sum(diag(rf_table_season_holiday)))/(sum(rf_table_season_holiday)) # Performance
pred_rf_season_holiday <- predict(rf_season_holiday, newdata = new_data_frame_season, type ='prob') # Predictions with probabilities

# Neural network

library(nnet)
neural_season_holiday <- nnet(sales_values ~. -SEMANA, data = data_frame_season_holiday, size = 5, maxit = 500, decay = .001, rang = 0.05)
pred_neural_season_holiday_class <- predict(neural_season_holiday, newdata = new_data_frame_season_holiday, type = 'class')  # Predictions in the test set with binary response 
neural_table_season_holiday <- table(pred_neural_season_holiday_class, new_data_frame_season$sales_values) # Confusion matrix
performance_neural_season_holiday <- (sum(diag(neural_table_season_holiday)))/(sum(neural_table_season_holiday)) # Performance

# Logistic regression 

reg_log_season_holiday <- glm(sales_values ~. -SEMANA, data = data_frame_season_holiday, family = binomial(link = 'logit'))
pred_reg_log_season_holiday <- predict(reg_log_season_holiday, newdata = new_data_frame_season_holiday, type = 'response') # Predictions with probabilities
pred_reg_log_season_holiday_class <- rep('0', length(pred_reg_log_season_holiday)) 
pred_reg_log_season_holiday_class[pred_reg_log_season_holiday > 0.5] <- '475' # Predictions with binary response 
reg_log_table_season_holiday <- table(pred_reg_log_season_holiday_class, new_data_frame_season_holiday$sales_values) # Confusion matrix 
performance_reg_log_season_holiday <- (sum(diag(reg_log_table_season_holiday)))/(sum(reg_log_table_season_holiday)) # Performance

# Linear discriminant analysis

lda_season_holiday <- lda(sales_values ~. -SEMANA , data = data_frame_season_holiday)
pred_lda_season_holiday_class <- predict(lda_season_holiday, newdata = new_data_frame_season_holiday, type = 'response')$class
lda_table_season_holiday <- table(pred_lda_season_holiday_class, new_data_frame_season_holiday$sales_values) # Confusion matrix 
performance_lda_season_holiday <- (sum(diag(lda_table_season_holiday)))/(sum(lda_table_season_holiday)) # Performance

# Quadratic discriminant analysis

qda_season_holiday <- qda(sales_values ~.-SEMANA, data = data_frame_season_holiday)
pred_qda_season_holiday_class <- predict(qda_season_holiday, newdata = new_data_frame_season_holiday, type = 'response')$class
qda_table_season_holiday <- table(pred_qda_season_holiday_class, new_data_frame_season_holiday$sales_values)
performance_qda_season_holiday <- (sum(diag(qda_table_season_holiday)))/(sum(qda_table_season_holiday)) # Performance


performance_vector <- c(performance_tree, performance_tree_season, performance_tree_season_holiday, performance_rf, 
                          performance_rf_season, performance_rf_season_holiday, performance_neural, performance_neural_season,
                          performance_rf_season_holiday, performance_reg_log, performance_reg_log_season, 
                          performance_reg_log_season_holiday, performance_lda, performance_lda_season,
                          performance_lda_season_holiday, performance_qda, performance_qda_season,
                          performance_qda_season_holiday)

names <- c('performance_tree', 'performance_tree_season', 'performance_tree_season_holiday', 'performance_rf', 
                                 'performance_rf_season', 'performance_rf_season_holiday', 'performance_neural', 'performance_neural_season',
                                 'performance_rf_season_holiday', 'performance_reg_log', 'performance_reg_log_season', 
                                 'performance_reg_log_season_holiday', 'performance_lda', 'performance_lda_season',
                                 'performance_lda_season_holiday', 'performance_qda', 'performance_qda_season',
                     
                       'performance_qda_season_holiday')
best_performance <- max(performance_vector)
best_performance_index <- which(performance_vector == best_performance)

paste0('For the establishment ', num_estab,  ' , best performance is achieved in ', 
       names[best_performance_index], ' and the performance rate is ', round(best_performance, 2),
       '. The naive classificator achieves ', round(naive_performance, 2), '.')

# proc.time() - t
