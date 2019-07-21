library (tidyverse)
library(neuralnet)



data <- read.csv("C:/Projects/HoneyWell/data/winter_heating_rates.csv")

year_train <- 2017
month_train <- 7

year_test <- 2017
month_test <- 8

for(i in 1:nrow(data)){
  date <- strsplit(as.character(data[i,"start_time"])," ")[[1]][1]
  data[i,"year"] <- as.numeric(strsplit(date ,"-")[[1]][1])
  data[i,"month"] <- as.numeric(strsplit(date ,"-")[[1]][2])
  data[i,"dayNum"] <- as.numeric(strsplit(date ,"-")[[1]][3])
}

train_data <- data %>% filter(year==year_train & month==month_train)
test_data <- data %>% filter(year==year_test & month==month_test)

train_data$normalized_start_inside_tem <- (train_data$start_temp-min(train_data$start_temp))/(max(train_data$start_temp)-min(train_data$start_temp))
train_data$normalized_start_outside_tem <- (train_data$start_outside_temp-min(train_data$start_outside_temp))/(max(train_data$start_outside_temp)-min(train_data$start_outside_temp))

test_data$normalized_start_inside_tem <- (test_data$start_temp-min(test_data$start_temp))/(max(test_data$start_temp)-min(test_data$start_temp))
test_data$normalized_start_outside_tem <- (test_data$start_outside_temp-min(test_data$start_outside_temp))/(max(test_data$start_outside_temp)-min(test_data$start_outside_temp))





#Stacking with SVM, Regression and Neural Network
validation_data <- test_data[1:20,]
new_test_data <- test_data[21:41,]

svm <-  svm(rate.per.15.min. ~ normalized_start_inside_tem + normalized_start_outside_tem,data = train_data, gamma=0.1, C=1)
validation_data$Pred_1 <- predict(svm,validation_data)
test_data$Pred_1 <- predict(svm,test_data)

lr <- lm(rate.per.15.min. ~ normalized_start_inside_tem + normalized_start_outside_tem, data = train_data)
coefficients(lr)
validation_data$Pred_2 <- coefficients(lr)[[1]] + coefficients(lr)[[2]]*validation_data$normalized_start_inside_tem + coefficients(lr)[[3]]*validation_data$normalized_start_outside_tem
test_data$Pred_2 <- coefficients(lr)[[1]] + coefficients(lr)[[2]]*test_data$normalized_start_inside_tem + coefficients(lr)[[3]]*test_data$normalized_start_outside_tem

nn <- neuralnet(rate.per.15.min. ~ Pred_1 + Pred_2, data = validation_data, threshold = 0.01, linear.output=TRUE, rep=10, hidden=c(3,3,3))
Predict <- neuralnet::compute(nn,test_data)
test_data$prediction <- Predict$net.result

#RMSE - 0.1126115    When use without hidden layers RMSE = 0.1159939
sqrt(sum((test_data$rate.per.15.min.-test_data$prediction)^2)/nrow(test_data))





# Residual based Boosting
#Method 1  - Error Prediction with Test Data
nn1 <- neuralnet(rate.per.15.min. ~ normalized_start_inside_tem + normalized_start_outside_tem, data = train_data, threshold = 0.01, linear.output=TRUE, rep=10)
Predict1 <- neuralnet::compute(nn1,test_data)
test_data$predicted_vals_1 <- Predict1$net.result
test_data$abs_error <- test_data$rate.per.15.min. - test_data$predicted_vals_1

nn2 <- neuralnet(abs_error ~ normalized_start_inside_tem + normalized_start_outside_tem, data = test_data, threshold = 0.01, linear.output=TRUE, rep=10)
Predict2 <- neuralnet::compute(nn2,test_data)
test_data$predicted_vals_2 <- Predict2$net.result

test_data$predicted <- test_data$predicted_vals_1 + test_data$predicted_vals_2


#RMSE - 0.1044605
sqrt(sum((test_data$rate.per.15.min.-test_data$predicted)^2)/nrow(test_data))



#Method 2 - Error Prediction with Train Data
nn1 <- neuralnet(rate.per.15.min. ~ normalized_start_inside_tem + normalized_start_outside_tem, data = train_data, threshold = 0.01, linear.output=TRUE, rep=10)
Predict1 <- neuralnet::compute(nn1,train_data)
train_data$predicted_vals_1 <- Predict1$net.result
train_data$abs_error <- train_data$rate.per.15.min. - train_data$predicted_vals_1

nn2 <- neuralnet(abs_error ~ normalized_start_inside_tem + normalized_start_outside_tem, data = train_data, threshold = 0.01, linear.output=TRUE, rep=10)
Predict2 <- neuralnet::compute(nn2,test_data)
test_data$predicted_vals_1 <- Predict2$net.result

nn1 <- neuralnet(rate.per.15.min. ~ normalized_start_inside_tem + normalized_start_outside_tem, data = train_data, threshold = 0.01, linear.output=TRUE, rep=10)
Predict3 <- neuralnet::compute(nn1,test_data)
test_data$predicted_vals_2 <- Predict3$net.result

test_data$predicted <- test_data$predicted_vals_1 + test_data$predicted_vals_2


#RMSE - 0.1094748
sqrt(sum((test_data$rate.per.15.min.-test_data$predicted)^2)/nrow(test_data))





#Method 3 - Error Prediction with Validation Data
validation_data <- test_data[1:20,]
new_test_data <- test_data[21:41,]

nn1 <- neuralnet(rate.per.15.min. ~ normalized_start_inside_tem + normalized_start_outside_tem, data = train_data, threshold = 0.01, linear.output=TRUE, rep=10)
Predict1 <- neuralnet::compute(nn1,validation_data)
validation_data$predicted_vals <- Predict1$net.result
validation_data$abs_error <- validation_data$rate.per.15.min. - validation_data$predicted_vals

nn2 <- neuralnet(abs_error ~ normalized_start_inside_tem + normalized_start_outside_tem, data = validation_data, threshold = 0.01, linear.output=TRUE, rep=10)
Predict2 <- neuralnet::compute(nn2,new_test_data)
new_test_data$predicted_vals_1 <- Predict2$net.result

nn1 <- neuralnet(rate.per.15.min. ~ normalized_start_inside_tem + normalized_start_outside_tem, data = train_data, threshold = 0.01, linear.output=TRUE, rep=10)
Predict3 <- neuralnet::compute(nn1,new_test_data)
new_test_data$predicted_vals_2 <- Predict3$net.result

new_test_data$predicted <- new_test_data$predicted_vals_1 + new_test_data$predicted_vals_2


#RMSE - 0.1354017
sqrt(sum((new_test_data$rate.per.15.min.-new_test_data$predicted)^2)/nrow(new_test_data))






#Ensemble with Changing Hyperparameters

for(i in 1:10){
  nn <- neuralnet(rate.per.15.min. ~ normalized_start_inside_tem + normalized_start_outside_tem, data = train_data, threshold = 0.01, linear.output=TRUE, rep=i)
  predict <- neuralnet::compute(nn,train_data)
  train_data[[paste("predicted_vals",i,sep="_")]] <- predict$net.result
  test_predict <- neuralnet::compute(nn,test_data)
  test_data[[paste("predicted_vals",i,sep="_")]] <- test_predict$net.result
}

svm <- svm(rate.per.15.min. ~ predicted_vals_1 + predicted_vals_2 + predicted_vals_3 + predicted_vals_4 + predicted_vals_5,data = train_data, gamma=0.1, C=1)
test_data$prediction <- predict(svm,test_data)


#RMSE - 0.1192725     (with 10 iterations - 0.1211601)
sqrt(sum((test_data$rate.per.15.min. - test_data$prediction)^2)/nrow(test_data))

# for(i in 1:3){
#   assign(paste("a", i, sep = ""), i)    
# }

