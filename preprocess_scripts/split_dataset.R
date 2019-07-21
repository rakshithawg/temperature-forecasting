OUTPUT_DIR = "/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/text_data"

file <- read.csv(file = "/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/text_data/temperature_data.csv")

max_forecast_horizon <- 12

model_data <- file[1:(nrow(file)-12),]

test_data <- file[(nrow(file)-max_forecast_horizon+1):nrow(file),]

test_data <- test_data$OAPU_G1Temp

eval_original_data <- model_data$OAPU_G1Temp

write.csv(model_data, paste0(OUTPUT_DIR,"/temperature_model_data.csv"))

write.csv(test_data, paste0(OUTPUT_DIR,"/temperature_test_data.csv"))

write.csv(eval_original_data, paste0(OUTPUT_DIR,"/temperature_original_data.csv"))