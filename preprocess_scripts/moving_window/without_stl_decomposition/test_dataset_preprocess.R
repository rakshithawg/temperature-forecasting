OUTPUT_DIR = "/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/text_data/moving_window/without_stl_decomposition"

file <- read.csv(file = "/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/text_data/temperature_model_data.csv")

max_forecast_horizon <- 12
seasonality_period <- 96
INPUT_SIZE_MULTIP <- 1.25
input_size <- round(seasonality_period * INPUT_SIZE_MULTIP)

OUTPUT_PATH <- paste(OUTPUT_DIR, "/temperature_test_", max_forecast_horizon, "i", input_size, ".txt", sep = '')

inside_temperatures <- as.numeric(file$OAPU_G1Temp)
temperature_dataset <- as.data.frame(inside_temperatures)

time_series_length <- nrow(temperature_dataset)


for (idr in 1 : ncol(temperature_dataset)) {
  mean <- mean(temperature_dataset[,idr])
  time_series <- temperature_dataset[,idr]/mean
  time_series_log <- log(time_series + 1)

  stl_result <-  tryCatch({
    sstl <- stl(ts(time_series_log,frequency=96),"period")
    seasonal_vect <- as.numeric(sstl$time.series[,1])
    levels_vect <- as.numeric(sstl$time.series[,2])
    values_vect <- as.numeric(sstl$time.series[,2]+sstl$time.series[,3]) # this is what we are going to work on: sum of the smooth trend and the random component (the seasonality removed)
    cbind(seasonal_vect,levels_vect,values_vect)
  }, error = function(e) {
    seasonal_vect <- rep(0,length(time_series_log))   #stl() may fail, and then we would go on with the seasonality vector=0
    levels_vect <- time_series_log
    values_vect <- time_series_log
    cbind(seasonal_vect, levels_vect, values_vect)
  })


  seasonality_12 <- tryCatch({
    forecast <- stlf(ts(stl_result[,1] , frequency = 96), "period",h=max_forecast_horizon)
    seasonality_12_vector <- as.numeric(forecast$mean)
    cbind(seasonality_12_vector)
  }, error = function(e) {
    seasonality_12_vector  <- rep(0, max_forecast_horizon)   #stl() may fail, and then we would go on with the seasonality vector=0
    cbind(seasonality_12_vector)
  })

  for (inn in input_size:time_series_length) {
    sav_df <- data.frame(id=paste(idr,'|i',sep=''));

    for (ii in 1:input_size) {
      sav_df[,paste('r',ii,sep='')] <- time_series_log[inn-input_size+ii]  #inputs: past values normalized by the level
    }

    sav_df[,'nyb'] <- '|#'
    sav_df[,'level'] <- mean

    for (ii in 1:max_forecast_horizon) {
      sav_df[, paste('s', ii, sep = '')] = seasonality_12[ii]
    }

    write.table(sav_df, file=OUTPUT_PATH, row.names = F, col.names=F, sep=" ", quote=F, append = TRUE)

  }
  print(idr)
}#through all series from one file