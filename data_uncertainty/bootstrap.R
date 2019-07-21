library(boot)
library(e1071)


temperature_data <- read.csv("C:/Projects/HoneyWell/new_project/data/heating_rate1.csv")

# Use this to normalize rates
#temperature_data$indoor_rate <- temperature_data$indoor_rate/15

#80-20 split
sample_size <- floor(0.8 * nrow(temperature_data))

#set seed to get reproducible training and test sets
set.seed(1)
train_ind <- sample(seq_len(nrow(temperature_data)), size = sample_size)

train <- temperature_data[train_ind, ]
test <- temperature_data[-train_ind, ]



regression <- function(formula, data, indices) {
  d <- train[indices,] 
  fit <- lm(formula, data=d)
  c_vals <- coefficients(fit)
  predictions <- c_vals[2]*test$start_temp + c_vals[3]*test$outside_temp + c_vals[1]
  return(sqrt(sum((test$indoor_rate - predictions)^2)))
} 


svm_bootstrap <- function(formula, data, indices){
  d <- train[indices,] 
  fit <- svm(formula, data=d, gamma=0.1, C=1)
  predictions <- predict(fit, test)
  return(sqrt(sum((test$indoor_rate - predictions)^2)))
}


# bootstrapping with 2000 replications 
reg_results <- boot(data=temperature_data, statistic=regression, R=2000, formula=indoor_rate ~ start_temp + outside_temp)
svm_results <- boot(data=temperature_data, statistic=svm_bootstrap, R=2000, formula=indoor_rate ~ start_temp + outside_temp)



# view results
#reg_results 
#plot(reg_results)

# get 95% confidence interval 
#type=c("norm", "basic", "stud", "perc", "bca")
boot.ci(reg_results)
boot.ci(reg_results, conf=0.8)
boot.ci(svm_results)
boot.ci(svm_results, conf=0.8)


#Plot
name <- c()
result <- c(reg_results$t,svm_results$t)

for(x in 1:2000){
  name <- c(name, "Regression")
}
for(x in 1:2000){
  name <- c(name, "SVM")
}

tbl <- data.frame(
  names = name,
  results = result
)

boxplot(results~names, data=tbl, xlab="Model", ylab="RMSE", par(cex.axis=1.5), par(cex.lab=1.5))

# boxplot(indoor_rate~start_temp, data=temperature_data, xlab="Start Temperature", ylab="RMSE", par(cex.axis=1.5), par(cex.lab=1.5))
# 
#boxplot(indoor_rate~outside_temp, data=temperature_data, xlab="Start Temperature", ylab="RMSE", par(cex.axis=1.5), par(cex.lab=1.5))

