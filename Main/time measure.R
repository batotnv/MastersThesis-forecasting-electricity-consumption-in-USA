library("readr")
library("tidyverse")
library("tsibble")
library("lubridate")
library("ggplot2") 
library("feasts")
library("fpp3")
library("kableExtra")
library("dplyr")
library("zoo")
library("fable.prophet")
library("tictoc")



#### PARAMETERS


epochs <- 10



####


#tourism_full <- tourism %>%
#  aggregate_key((State/Region) * Purpose, Trips = sum(Trips))

dane_usa <- read_delim("~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/dane nowe/dane_usa.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


#time series with types of sectors

dane_sectors <- pivot_longer (dane_usa, cols = 8:12, names_to = "Sector", values_to = "MWh" )


#time series without types of sectors

dane <- dane_usa[-c(8, 9, 10, 11)]

colnames(dane)[8] <- "MWh"
colnames(dane)[6] <- "RegionName"
colnames(dane)[5] <- "StateName"
#sprawdzanie czy wstepuja dane na
sum(is.na(dane))


#types of data


str(dane)

#deleting whitespaces
dane$MWh <- gsub(" ", "", dane$MWh, fixed = TRUE)

dane$MWh <- as.numeric(dane$MWh)


#creating date columns

#year
dane %>% mutate(Date = make_date(Year,Month)) -> dane
#quarter
dane %>% mutate(Quarter = yearquarter(Date)) -> dane
#month
dane %>% mutate(Month = yearmonth(Date)) -> dane
#deleting date column
dane %>% select(-Date) -> dane

#filtering out 2022 as it we do not have the whole year

dane %>% filter(Year != 2022) -> dane


str(dane)


#dane quarter

dane %>% select(-c(Year,Month,State,Country)) -> dane_quarter

str(dane_quarter)

#grupowanie dla kazdego kwartalu (bo dane ogolnie sa miesieczne i trzeba bylo zgrupowac kazde 3 miesiace dla kwartalu)

dane_quarter %>% group_by(Quarter, StateName, RegionName) %>% summarise(MWh = sum(MWh)) -> dane_quarter



#to tsibble
ts.dane_quarter <-as_tsibble(dane_quarter, index = "Quarter", key = c(StateName,RegionName))



#creating hierarchy structure, aggregation
dane_quarter_hts <- ts.dane_quarter %>%
  aggregate_key(RegionName/StateName, MWh = sum(MWh))


#visualization

dane_quarter_hts %>%
  filter(is_aggregated(StateName)) %>%
  autoplot(MWh) +
  labs(y = "MWh",
       title = "USA energy consumption quarterly") +
  facet_wrap(vars(RegionName), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

############

# #dane yearly 
# 
# dane %>% select(-c(Quarter,Month,State,Country)) -> dane_year
# 
# str(dane_year)
# 
# #grupowanie dla kazdego roku(bo dane ogolnie sa miesieczne i trzeba bylo zgrupowac kazde 12 miesiace dla roku)
# 
# dane_year %>% group_by(Year, StateName, RegionName) %>% summarise(MWh = sum(MWh)) -> dane_year
# 
# 
# 
# #to tsibble
# ts.dane_year <-as_tsibble(dane_year, index = "Year", key = c(StateName,RegionName))
# 
# 
# dane_year_hts <- ts.dane_year %>%
#   aggregate_key(RegionName/StateName, MWh = sum(MWh))
# 
# 
# 
# #visualization
# 
# dane_year_hts %>%
#   filter(is_aggregated(StateName)) %>%
#   autoplot(MWh) +
#   labs(y = "MWh",
#        title = "USA energy consumption yearly") +
#   facet_wrap(vars(RegionName), scales = "free_y", ncol = 3) +
#   theme(legend.position = "none")
# 
# 
##############################
# #dane month
# 
# dane %>% select(-c(Quarter,Year,State,Country)) -> dane_month
# 
# str(dane_month)
# 
# 
# 
# #to tsibble
# ts.dane_month <-as_tsibble(dane_month, index = "Month", key = c(StateName,RegionName))
# 
# 
# dane_month_hts <- ts.dane_month %>%
#   aggregate_key(RegionName/StateName, MWh = sum(MWh))
# 
# 
# 
# #visualization
# 
# dane_month_hts %>%
#   filter(is_aggregated(StateName)) %>%
#   autoplot(MWh) +
#   labs(y = "MWh",
#        title = "USA energy consumption monthly") +
#   facet_wrap(vars(RegionName), scales = "free_y", ncol = 3) +
#   theme(legend.position = "none")



############ PROPHET, ETS, ARIMA - bez rekoncyliacji



#facebook prophet without reconciliation
#prophet model is always giving different results



non_hierarchical_prophet <- function (period, order, type){
  
  for (i in 1:epochs) {
    print(paste("Epoch: ", i))
    measure <- "Non-hierarchical prophet: "
    tic(measure)  
    fit <- dane_quarter_hts %>%
      filter(year(Quarter) <= 2017) %>%
      model(base = prophet(MWh ~ season(period = period, order = order,
                                        type = type)))
    #bulding forecast
    fc <- fit %>% forecast(h = "4 years")
    toc(log = TRUE)
  } 
  
  fc %>%
    filter(is_aggregated(StateName)) %>%
    autoplot(
      dane_quarter_hts %>% filter(year(Quarter) >= 2011),
      level = NULL
    ) +
    labs(y = "MWh") +
    facet_wrap(vars(RegionName), scales = "free_y")
  
  
  results_train <- get_results_train(fit)
  results_test <- get_results_test(fc)
  log.txt <- tic.log(format = TRUE)
  time_results <- as.data.frame(unlist(log.txt))
  tic.clearlog()
  
  results <- list(results_train, results_test, time_results)
  results
  
}



### forecasting without reconciliation for country, regions, states


non_hierarchical_forecast <- function(FUN, epochs, ...){
  
  for (i in 1:epochs) {
    print(paste("Epoch: ", i))
    measure <- paste("Non-hierarchical: ", as.character(substitute(FUN)))
    tic(measure)
    fit <- dane_quarter_hts %>%
      filter(year(Quarter) <= 2017) %>%
      model(base = FUN(MWh)) 
    #bulding forecast
    fc <- fit %>% forecast(h = "4 years")
    toc(log = TRUE)
  }
  
  fc %>%
    filter(is_aggregated(StateName)) %>%
    autoplot(
      dane_quarter_hts %>% filter(year(Quarter) >= 2011),
      level = NULL
    ) +
    labs(y = "MWh") +
    facet_wrap(vars(RegionName), scales = "free_y")
  
  
  results_train <- get_results_train(fit)
  results_test <- get_results_test(fc)
  log.txt <- tic.log(format = TRUE)
  time_results <- as.data.frame(unlist(log.txt))
  tic.clearlog()
  
  results <- list(results_train, results_test, time_results)
  results
  
}


non_hierarchical_forecast_drift <- function(...){
  
  for (i in 1:epochs) {
    print(paste("Epoch: ", i))
    measure <- "Non-hierarchical drift: "
    tic(measure)
    fit <- dane_quarter_hts %>%
      filter(year(Quarter) <= 2017) %>%
      model(base = RW(MWh ~ drift())) 
    #bulding forecast
    fc <- fit %>% forecast(h = "4 years")
    toc(log = TRUE)
  }
  
  fc %>%
    filter(is_aggregated(StateName)) %>%
    autoplot(
      dane_quarter_hts %>% filter(year(Quarter) >= 2011),
      level = NULL
    ) +
    labs(y = "MWh") +
    facet_wrap(vars(RegionName), scales = "free_y")
  
  
  results_train <- get_results_train(fit)
  results_test <- get_results_test(fc)
  log.txt <- tic.log(format = TRUE)
  time_results <- as.data.frame(unlist(log.txt))
  tic.clearlog()
  
  results <- list(results_train, results_test, time_results)
  results
  
}



#naiwne
results_non_hierarchical_mean <- non_hierarchical_forecast(MEAN, epochs)
results_non_hierarchical_naive <-non_hierarchical_forecast(NAIVE, epochs)
results_non_hierarchical_arima <- non_hierarchical_forecast(ARIMA, epochs)
results_non_hierarchical_ets <- non_hierarchical_forecast(ETS, epochs)
results_non_hierarchical_snaive <- non_hierarchical_forecast(SNAIVE, epochs)

results_non_hierarchical_prophet <- non_hierarchical_prophet(period = 4, order = 2, type = "multiplicative")
results_non_hierarchical_drift <- non_hierarchical_forecast_drift()

#csv train measures
write_csv(results_non_hierarchical_mean[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_mean_train.csv")
write_csv(results_non_hierarchical_naive[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_naive_train.csv")
write_csv(results_non_hierarchical_arima[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_arima_train.csv")
write_csv(results_non_hierarchical_ets[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_ets_train.csv")
write_csv(results_non_hierarchical_snaive[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_snaive_train.csv")

write_csv(results_non_hierarchical_drift[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_drift_train.csv")
write_csv(results_non_hierarchical_prophet[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_prophet_train.csv")

#csv test measures
write_csv(results_non_hierarchical_mean[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_mean.csv")
write_csv(results_non_hierarchical_naive[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_naive.csv")
write_csv(results_non_hierarchical_arima[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_arima.csv")
write_csv(results_non_hierarchical_ets[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_ets.csv")
write_csv(results_non_hierarchical_snaive[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_snaive.csv")

write_csv(results_non_hierarchical_drift[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_drift.csv")
write_csv(results_non_hierarchical_prophet[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_prophet.csv")

#time duration results
write_csv(results_non_hierarchical_mean[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_non_hierarchical_mean.csv")
write_csv(results_non_hierarchical_naive[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_non_hierarchical_naive.csv")
write_csv(results_non_hierarchical_arima[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_non_hierarchical_arima.csv")
write_csv(results_non_hierarchical_ets[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_non_hierarchical_ets.csv")
write_csv(results_non_hierarchical_snaive[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_non_hierarchical_snaive.csv")
write_csv(results_non_hierarchical_drift[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_non_hierarchical_drift.csv")
write_csv(results_non_hierarchical_prophet[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_non_hierarchical_prophet.csv")



####################
# RECONCILIATION
###################


#Prophet function
#facebook prophet with reconciliation

#h_forecast_prophet(period = 4, order = 2, type = "multiplicative")


h_forecast_prophet <- function (period, order, type){
  
  fit <- dane_quarter_hts %>%
    filter(year(Quarter) <= 2017) %>%
    model(base = prophet(MWh ~ season(period = period, order = order,
                                      type = type))) %>%
    reconcile(
      bu = bottom_up(base),
      ols = min_trace(base, method = "ols"),
      mint = min_trace(base, method = "mint_shrink"),
      td = top_down(base, method = "forecast_proportions")
    )
  
  
  #bulding forecast
  fc <- fit %>% forecast(h = "4 years")
  
  
  fc %>%
    filter(is_aggregated(StateName)) %>%
    autoplot(
      dane_quarter_hts %>% filter(year(Quarter) >= 2011),
      level = NULL
    ) +
    labs(y = "MWh") +
    facet_wrap(vars(RegionName), scales = "free_y")
  
  
  #uwaga
  results_train <- get_results_test_hierarchical(fc)
  results_test <- get_results_test_hierarchical(fc)
  
  results <- list(results_train, results_test)
  results
}




######################################
#function for hierarchical forecast
#ETS
#hierarchical_forecast(ETS)

#Arima
#hierarchical_forecast(ARIMA)





hierarchical_forecast <- function(FUN, ...){
  
  tic("test")
  fit <- dane_quarter_hts %>%
    filter(year(Quarter) <= 2017) %>%
    model(base = FUN(MWh)) %>%
    reconcile(
      bu = bottom_up(base),
      ols = min_trace(base, method = "ols"),
      mint = min_trace(base, method = "mint_shrink"),
      td = top_down(base, method = "forecast_proportions")
    )
  toc()
  
  #bulding forecast
  fc <- fit %>% forecast(h = "4 years")
  
  
  fc %>%
    filter(is_aggregated(StateName)) %>%
    autoplot(
      dane_quarter_hts %>% filter(year(Quarter) >= 2011),
      level = NULL
    ) +
    labs(y = "MWh") +
    facet_wrap(vars(RegionName), scales = "free_y")
  
  #uwaga
  results_train <- get_results_test_hierarchical(fc)
  results_test <- get_results_test_hierarchical(fc)
  
  results <- list(results_train, results_test)
  results
  
}

hierarchical_forecast_drift <- function(...){
  
  tic("test")
  fit <- dane_quarter_hts %>%
    filter(year(Quarter) <= 2017) %>%
    model(base = RW(MWh ~ drift())) %>%
    reconcile(
      bu = bottom_up(base),
      ols = min_trace(base, method = "ols"),
      mint = min_trace(base, method = "mint_shrink"),
      td = top_down(base, method = "forecast_proportions")
    )
  toc()
  
  #bulding forecast
  fc <- fit %>% forecast(h = "4 years")
  
  
  fc %>%
    filter(is_aggregated(StateName)) %>%
    autoplot(
      dane_quarter_hts %>% filter(year(Quarter) >= 2011),
      level = NULL
    ) +
    labs(y = "MWh") +
    facet_wrap(vars(RegionName), scales = "free_y")
  
  #uwaga
  results_train <- get_results_test_hierarchical(fc)
  results_test <- get_results_test_hierarchical(fc)
  
  results <- list(results_train, results_test)
  results
  
}




#naiwne
results_hierarchical_mean <- hierarchical_forecast(MEAN)
results_hierarchical_naive <- hierarchical_forecast(NAIVE)
results_hierarchical_snaive <- hierarchical_forecast(SNAIVE)
results_hierarchical_drift <- hierarchical_forecast_drift()


#results
results_hierarchical_arima <- hierarchical_forecast(ARIMA)
results_hierarchical_ets <- hierarchical_forecast(ETS)
results_hierarchical_prophet <- h_forecast_prophet(period = 4, order = 2, type = "multiplicative")

#csv train
write_csv(results_hierarchical_arima[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_arima_train.csv")
write_csv(results_hierarchical_ets[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_ets_train.csv")
write_csv(results_hierarchical_snaive[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_snaive_train.csv")
write_csv(results_hierarchical_prophet[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_prophet_train.csv")
write_csv(results_hierarchical_mean[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_mean_train.csv")
write_csv(results_hierarchical_naive[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_naive_train.csv")
write_csv(results_hierarchical_drift[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_drift_train.csv")

#csv test
write_csv(results_hierarchical_arima[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_arima.csv")
write_csv(results_hierarchical_ets[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_ets.csv")
write_csv(results_hierarchical_snaive[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_snaive.csv")
write_csv(results_hierarchical_prophet[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_prophet.csv")
write_csv(results_hierarchical_mean[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_mean.csv")
write_csv(results_hierarchical_naive[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_naive.csv")
write_csv(results_hierarchical_drift[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_drift.csv")


##### auxillary functions


convert_to_vector <- function (results){
  
  vector <- round(c(unlist(results[,2]), unlist(results[,3]), unlist(results[,4]), unlist(results[,5])),2)
  vector
}



get_results_train <- function (fit){
  
  fit %>%
    filter(is_aggregated(StateName), is_aggregated(RegionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_total
  
  
  ### measures for each region (regions)
  fit %>%
    filter(is_aggregated(StateName), !is_aggregated(RegionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_regions
  
  ### measures for each region (states) - bottom level
  fit %>%
    filter(!is_aggregated(StateName),!is_aggregated(RegionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_states
  
  
  
  total_vector <- convert_to_vector(results_total)
  region_vector <- convert_to_vector(results_regions)
  states_vector <- convert_to_vector(results_states)
  
  results <- as.data.frame(matrix(NA,1,4))
  colnames(results) <- c("RMSE","MASE","MAPE", "CRPS")
  results[1,] <- total_vector
  results[2,] <- region_vector
  results[3,] <- states_vector
  rownames(results) <- c("Country", "Regions", "States")
  
  results
  
}


get_results_test <- function (fc){
  ### measures for whole country (total)
  
  fc %>%
    filter(is_aggregated(StateName), is_aggregated(RegionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_total
  
  
  ### measures for each region (regions)
  fc %>%
    filter(is_aggregated(StateName), !is_aggregated(RegionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_regions
  
  ### measures for each region (states) - bottom level
  fc %>%
    filter(!is_aggregated(StateName),!is_aggregated(RegionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_states
  
  
  
  total_vector <- convert_to_vector(results_total)
  region_vector <- convert_to_vector(results_regions)
  states_vector <- convert_to_vector(results_states)
  
  results <- as.data.frame(matrix(NA,1,4))
  colnames(results) <- c("RMSE","MASE","MAPE", "CRPS")
  results[1,] <- total_vector
  results[2,] <- region_vector
  results[3,] <- states_vector
  rownames(results) <- c("Country", "Regions", "States")
  
  results
  
}


get_results_train_hierarchical <- function (fc){
  ### measures for whole country (total)
  
  fc %>%
    filter(is_aggregated(StateName), is_aggregated(RegionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_total
  
  
  ### measures for each region (regions)
  fc %>%
    filter(is_aggregated(StateName), !is_aggregated(RegionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_regions
  
  ### measures for each region (states) - bottom level
  fc %>%
    filter(!is_aggregated(StateName),!is_aggregated(RegionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_states
  
  
  
  total_vector <- convert_to_vector(results_total)
  region_vector <- convert_to_vector(results_regions)
  states_vector <- convert_to_vector(results_states)
  
  results <- as.data.frame(matrix(NA,1,20))
  colnames(results) <- rep(c("RMSE", "MASE","MAPE","CRPS"),times = c(5,5,5,5))
  results[1,]  <- rep(c("BASE", "BU","MinT","OLS","TD"),times = 4)
  results[2,] <- total_vector
  results[3,] <- region_vector
  results[4,] <- states_vector
  rownames(results) <- c("Methods","Country", "Regions", "States")
  
  results
  
}

get_results_test_hierarchical <- function (fc){
  ### measures for whole country (total)
  
  fc %>%
    filter(is_aggregated(StateName), is_aggregated(RegionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_total
  
  
  ### measures for each region (regions)
  fc %>%
    filter(is_aggregated(StateName), !is_aggregated(RegionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_regions
  
  ### measures for each region (states) - bottom level
  fc %>%
    filter(!is_aggregated(StateName),!is_aggregated(RegionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_states
  
  
  
  total_vector <- convert_to_vector(results_total)
  region_vector <- convert_to_vector(results_regions)
  states_vector <- convert_to_vector(results_states)
  
  results <- as.data.frame(matrix(NA,1,20))
  colnames(results) <- rep(c("RMSE", "MASE","MAPE","CRPS"),times = c(5,5,5,5))
  results[1,]  <- rep(c("BASE", "BU","MinT","OLS","TD"),times = 4)
  results[2,] <- total_vector
  results[3,] <- region_vector
  results[4,] <- states_vector
  rownames(results) <- c("Methods","Country", "Regions", "States")
  
  results
  
}

