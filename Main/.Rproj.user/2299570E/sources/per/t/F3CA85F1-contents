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
library("forecast")



#### PARAMETERS
epochs <- 1
#data_load_path 
#data_save_path 


dane_usa <- read_delim("~/Library/CloudStorage/OneDrive-Personal/Projekty Bartosz/Praca magisterska/dane nowe/dane_usa.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


#time series with types of sectors
dane_sectors <- pivot_longer (dane_usa, cols = 8:12, names_to = "Sector", values_to = "MWh" )


#time series without types of sectors
dane <- dane_usa[-c(8, 9, 10, 11)]

colnames(dane)[8] <- "MWh"
colnames(dane)[6] <- "DivisionName"
colnames(dane)[5] <- "StateName"


#check for NAs
sum(is.na(dane))


#count(dane$MWh==0)

unique(dane$StateName)
unique(dane$DivisionName)


dane %>% filter(dane$MWh==0) -> check0


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

# grouping by quarter (monthly data)

dane_quarter %>% group_by(Quarter, StateName, DivisionName) %>% summarise(MWh = sum(MWh)) -> dane_quarter


#to tsibble
ts.dane_quarter <-as_tsibble(dane_quarter, index = "Quarter", key = c(StateName,DivisionName))

str(ts.dane_quarter)


#creating hierarchy structure, aggregation
dane_quarter_hts <- ts.dane_quarter %>%
  aggregate_key(DivisionName/StateName, MWh = sum(MWh))



str(dane_quarter_hts)


#checking for outliers

dane_quarter_hts %>%
  filter(is_aggregated(StateName), is_aggregated(DivisionName)) -> test_outliers

str(test_outliers[,c(1,4)])


ts_test <- ts(test_outliers[,c(4)])

ts_test[128] <- 100000000


tsoutliers(ts_test)
tsoutliers(gold)


states_ts <- list()
dane_quarter_hts %>%
  filter(!is_aggregated(StateName), !is_aggregated(DivisionName)) -> dane_quarter_hts_states

states <- NULL
states <- unlist(unique(dane_quarter_hts_states$StateName))[1:51]
states

convert_to_ts <- function (x){
  
  colnames(x) -> NamesOfColumns
  pos <- match("MWh", NamesOfColumns)
  result <- ts(x[,pos])
  
  result
}

for (i in 1:length(states)){
  
  print(states[i])
  
  dane_quarter_hts_states %>%
    filter(dane_quarter_hts_states$StateName == states[i]) -> temp_ts
  
  states_ts[[i]] <- tsoutliers(convert_to_ts(temp_ts))

}

lapply(states_ts, print)

# no outliers found


#exploratory analysis

dane_quarter_hts %>%
  filter(is_aggregated(StateName), is_aggregated(DivisionName)) -> ts_usa


ts_usa <- ts_usa[,c(1,4)]


#trend
ts_usa %>% model(STL(MWh)) %>% components() -> components
ts_usa %>% autoplot(MWh) + autolayer(components, trend, color="orange") + labs (title = "USA - Trend") + xlab("Kwartał [1Q]")


#seasonality
ts_usa %>% gg_season(MWh) + labs (title = "USA - Sezonowość") + xlab("Kwartał [1Q]") #sezonowość


#subseries
ts_usa %>% gg_subseries(MWh) + labs (title = "USA - Wykres podserii") + xlab("Kwartał [1Q]") #sezonowość


#autocorrelation
ts_usa %>% ACF(MWh, lag_max = 100) %>% autoplot() + labs (title = "USA - Wykres ACF") + xlab("Opóźnienie [1Q]")


#visualization
dane_quarter_hts %>%
  filter(is_aggregated(StateName)) %>%
  autoplot(MWh) +
  labs(y = "MWh",
       title = "USA energy consumption quarterly") +
  facet_wrap(vars(DivisionName), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

############

#data split on train and test data
dane_train <- dane_quarter_hts %>%
  filter(year(Quarter) <= 2017,is_aggregated(StateName), is_aggregated(DivisionName))

dane_test <- dane_quarter_hts %>%
  filter(year(Quarter) > 2017, is_aggregated(StateName), is_aggregated(DivisionName))


ratio <- nrow(dane_test)/(nrow(dane_test)+nrow(dane_train))
ratio


# #dane yearly 
# 
# dane %>% select(-c(Quarter,Month,State,Country)) -> dane_year
# 
# str(dane_year)
# 
# #grupowanie dla kazdego roku(bo dane ogolnie sa miesieczne i trzeba bylo zgrupowac kazde 12 miesiace dla roku)
# 
# dane_year %>% group_by(Year, StateName, DivisionName) %>% summarise(MWh = sum(MWh)) -> dane_year
# 
# 
# 
# #to tsibble
# ts.dane_year <-as_tsibble(dane_year, index = "Year", key = c(StateName,DivisionName))
# 
# 
# dane_year_hts <- ts.dane_year %>%
#   aggregate_key(DivisionName/StateName, MWh = sum(MWh))
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
#   facet_wrap(vars(DivisionName), scales = "free_y", ncol = 3) +
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
# ts.dane_month <-as_tsibble(dane_month, index = "Month", key = c(StateName,DivisionName))
# 
# 
# dane_month_hts <- ts.dane_month %>%
#   aggregate_key(DivisionName/StateName, MWh = sum(MWh))
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
#   facet_wrap(vars(DivisionName), scales = "free_y", ncol = 3) +
#   theme(legend.position = "none")



############ PROPHET, ETS, ARIMA - no reconciliation


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
    facet_wrap(vars(DivisionName), scales = "free_y")
  
  
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
    facet_wrap(vars(DivisionName), scales = "free_y")
  
  
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
    facet_wrap(vars(DivisionName), scales = "free_y")
  
  
  results_train <- get_results_train(fit)
  results_test <- get_results_test(fc)
  log.txt <- tic.log(format = TRUE)
  time_results <- as.data.frame(unlist(log.txt))
  tic.clearlog()
  
  results <- list(results_train, results_test, time_results)
  results
  
}



#naive
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


h_forecast_prophet <- function (period, order, type, Time){
  
  
  if (Time == TRUE){
    
    
    # Fun BU
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- "Hierarchical;BU;Prophet"
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = prophet(MWh ~ season(period = period, order = order,
                                          type = type))) %>%
        reconcile(
          bu = bottom_up(base)
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    # Fun OLS
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- "Hierarchical;OLS;Prophet"
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = prophet(MWh ~ season(period = period, order = order,
                                          type = type))) %>%
        reconcile(
          ols = min_trace(base, method = "ols")
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    # Fun MinT
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- "Hierarchical;MinT;Prophet"
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = prophet(MWh ~ season(period = period, order = order,
                                          type = type))) %>%
        reconcile(
          mint = min_trace(base, method = "mint_shrink")
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    # Fun TD
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- "Hierarchical;TD;Prophet"
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = prophet(MWh ~ season(period = period, order = order,
                                          type = type))) %>%
        reconcile(
          td = top_down(base, method = "forecast_proportions")
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    
    log.txt <- tic.log(format = TRUE)
    time_results <- as.data.frame(unlist(log.txt))
    tic.clearlog()
    
  } else {
    
    time_results <- "Time was not measured"
  }
  
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
    facet_wrap(vars(DivisionName), scales = "free_y")


#uwaga
  results_train <- "NA"
  results_test <- get_results_test_hierarchical(fc)
  
  results <- list(results_train, results_test, time_results)
  results
}
  


######################################
#function for hierarchical forecast
#ETS
#hierarchical_forecast(ETS)

#Arima
#hierarchical_forecast(ARIMA)


hierarchical_forecast <- function(FUN, Time, ...){
  
  
  if (Time == TRUE){
    
    
    # Fun BU
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- paste("Hierarchical;BU;", as.character(substitute(FUN)))
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = FUN(MWh)) %>%
        reconcile(
          bu = bottom_up(base)
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    # Fun OLS
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- paste("Hierarchical;OLS;", as.character(substitute(FUN)))
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = FUN(MWh)) %>%
        reconcile(
          ols = min_trace(base, method = "ols")
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    # Fun MinT
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- paste("Hierarchical;MinT;", as.character(substitute(FUN)))
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = FUN(MWh)) %>%
        reconcile(
          mint = min_trace(base, method = "mint_shrink")
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    # Fun TD
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- paste("Hierarchical;TD;", as.character(substitute(FUN)))
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = FUN(MWh)) %>%
        reconcile(
          td = top_down(base, method = "forecast_proportions")
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    
    log.txt <- tic.log(format = TRUE)
    time_results <- as.data.frame(unlist(log.txt))
    tic.clearlog()
    
  } else {
    
    time_results <- "Time was not measured"
  }
  
  
  fit <- dane_quarter_hts %>%
    filter(year(Quarter) <= 2017) %>%
    model(base = FUN(MWh)) %>%
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
    facet_wrap(vars(DivisionName), scales = "free_y")
  
  #uwaga
  results_train <- "NA"
  results_test <- get_results_test_hierarchical(fc)
  
  results <- list(results_train, results_test, time_results)
  results

}


hierarchical_forecast_drift <- function(Time, ...){
  
  
  if (Time == TRUE){
    
    
    # Fun BU
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- "Hierarchical;BU;drift"
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = RW(MWh ~ drift())) %>%
        reconcile(
          bu = bottom_up(base)
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    # Fun OLS
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- "Hierarchical;OLS;drift"
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = RW(MWh ~ drift())) %>%
        reconcile(
          ols = min_trace(base, method = "ols")
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    # Fun MinT
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- "Hierarchical;MinT;drift"
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = RW(MWh ~ drift())) %>%
        reconcile(
          mint = min_trace(base, method = "mint_shrink")
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    # Fun TD
    for (i in 1:epochs) {
      
      print(paste("Epoch: ", i))
      measure <- "Hierarchical;TD;drift"
      
      tic(measure)
      fit <- dane_quarter_hts %>%
        filter(year(Quarter) <= 2017) %>%
        model(base = RW(MWh ~ drift())) %>%
        reconcile(
          td = top_down(base, method = "forecast_proportions")
        )
      #bulding forecast
      fc <- fit %>% forecast(h = "4 years")
      toc(log = TRUE)
    }
    
    
    log.txt <- tic.log(format = TRUE)
    time_results <- as.data.frame(unlist(log.txt))
    tic.clearlog()
    
  } else {
    
    time_results <- "Time was not measured"
  }
  
  
  fit <- dane_quarter_hts %>%
    filter(year(Quarter) <= 2017) %>%
    model(base = RW(MWh ~ drift())) %>%
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
    facet_wrap(vars(DivisionName), scales = "free_y")
  
  #uwaga
  results_train <- "NA"
  results_test <- get_results_test_hierarchical(fc)
  
  results <- list(results_train, results_test, time_results)
  results
  
}



#second parameter: TRUE - measure time, FALSE - do not measure time

#naive
results_hierarchical_mean <- hierarchical_forecast(MEAN, TRUE)
results_hierarchical_naive <- hierarchical_forecast(NAIVE, TRUE)
results_hierarchical_snaive <- hierarchical_forecast(SNAIVE, TRUE)
results_hierarchical_drift <- hierarchical_forecast_drift(TRUE)


#results
results_hierarchical_arima <- hierarchical_forecast(ARIMA, TRUE)
results_hierarchical_ets <- hierarchical_forecast(ETS, TRUE)
results_hierarchical_prophet <- h_forecast_prophet(period = 4, order = 2, type = "multiplicative", TRUE)

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


#time measures

write_csv(results_hierarchical_arima[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_hierarchical_arima.csv")
write_csv(results_hierarchical_ets[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_hierarchical_ets.csv")
write_csv(results_hierarchical_snaive[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_hierarchical_snaive.csv")
write_csv(results_hierarchical_mean[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_hierarchical_mean.csv")
write_csv(results_hierarchical_naive[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_hierarchical_naive.csv")

write_csv(results_hierarchical_drift[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_hierarchical_drift.csv")
write_csv(results_hierarchical_prophet[[3]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_hierarchical_prophet.csv")



##### auxillary functions

convert_to_vector <- function (results){
  
  vector <- round(c(unlist(results[,2]), unlist(results[,3]), unlist(results[,4]), unlist(results[,5])),2)
  vector
}


get_results_train <- function (fit){
  
  fit %>%
    filter(is_aggregated(StateName), is_aggregated(DivisionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_total
  
  
  ### measures for each region (regions)
  fit %>%
    filter(is_aggregated(StateName), !is_aggregated(DivisionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_regions
  
  ### measures for each region (states) - bottom level
  fit %>%
    filter(!is_aggregated(StateName),!is_aggregated(DivisionName)) %>%
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
    filter(is_aggregated(StateName), is_aggregated(DivisionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_total
  
  
  ### measures for each region (regions)
  fc %>%
    filter(is_aggregated(StateName), !is_aggregated(DivisionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_regions
  
  ### measures for each region (states) - bottom level
  fc %>%
    filter(!is_aggregated(StateName),!is_aggregated(DivisionName)) %>%
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
    filter(is_aggregated(StateName), is_aggregated(DivisionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_total
  
  
  ### measures for each region (regions)
  fc %>%
    filter(is_aggregated(StateName), !is_aggregated(DivisionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_regions
  
  ### measures for each region (states) - bottom level
  fc %>%
    filter(!is_aggregated(StateName),!is_aggregated(DivisionName)) %>%
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
    filter(is_aggregated(StateName), is_aggregated(DivisionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_total
  
  
  ### measures for each region (regions)
  fc %>%
    filter(is_aggregated(StateName), !is_aggregated(DivisionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_regions
  
  ### measures for each region (states) - bottom level
  fc %>%
    filter(!is_aggregated(StateName),!is_aggregated(DivisionName)) %>%
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



