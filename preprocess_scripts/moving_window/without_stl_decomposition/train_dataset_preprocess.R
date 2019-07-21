OUTPUT_DIR = "/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/text_data/moving_window/without_stl_decomposition"

file <- read.csv(file = "/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/text_data/temperature_model_data.csv")

max_forecast_horizon <- 12
seasonality_period <- 96
INPUT_SIZE_MULTIP <- 1.25
input_size <- round(seasonality_period * INPUT_SIZE_MULTIP)

OUTPUT_PATH <- paste(OUTPUT_DIR, "/temperature_", max_forecast_horizon, "i", input_size, ".txt", sep = '')

time_series_length <- nrow(file)
time_series_length <- time_series_length - max_forecast_horizon

inside_temperatures <- as.numeric(file$OAPU_G1Temp)
temperature_dataset <- as.data.frame(inside_temperatures)


for (idr in 1 : ncol(temperature_dataset)) {

  mean <- mean(temperature_dataset[,idr])
  time_series <- temperature_dataset[,idr]/mean
  time_series_log <- log(time_series + 1)

  for (inn in input_size:(time_series_length-max_forecast_horizon)) {
    sav_df <- data.frame(id=paste(idr,'|i',sep=''));

    for (ii in 1:input_size) {
      sav_df[,paste('r',ii,sep='')] <- time_series_log[inn-input_size+ii]  #inputs: past values normalized by the level
    }

    sav_df[,'o'] <- '|o'
    for (ii in 1:max_forecast_horizon) {
      sav_df[,paste('o',ii,sep='')] <- time_series_log[inn+ii] #outputs: future values normalized by the level.
    }

    write.table(sav_df, file=OUTPUT_PATH, row.names = F, col.names=F, sep=" ", quote=F, append = TRUE)

  } 
  print(idr)
}#through all series from one file
