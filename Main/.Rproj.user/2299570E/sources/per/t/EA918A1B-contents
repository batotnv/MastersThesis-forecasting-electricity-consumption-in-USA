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



dane_usa <- read_delim("~/Library/CloudStorage/OneDrive-Personal/Projekty Bartosz/Praca magisterska/dane nowe/dane_usa.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


#time series with types of sectors

dane_sectors <- pivot_longer (dane_usa, cols = 8:12, names_to = "Sector", values_to = "MWh" )

?pivot_longer

#time series without types of sectors

dane <- dane_usa[-c(8, 9, 10, 11)]

colnames(dane)[8] <- "MWh"
colnames(dane)[6] <- "DivisionName"
colnames(dane)[5] <- "StateName"



#sprawdzanie czy wstepuja dane na
sum(is.na(dane))


#
count(dane$MWh==0)

unique(dane$StateName)
unique(dane$DivisionName)

#brak brakow w danych

#check

dane %>% filter(dane$MWh==0) -> check0

?filter
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

dane_quarter %>% group_by(Quarter, StateName, DivisionName) %>% summarise(MWh = sum(MWh)) -> dane_quarter



#to tsibble
ts.dane_quarter <-as_tsibble(dane_quarter, index = "Quarter", key = c(StateName,DivisionName))


dane_hierarchia_azure <- ts.dane_quarter %>% mutate(CountryName = "USA")

write_csv(dane_hierarchia_azure, "dane_hierarchia_azure.csv")


str(ts.dane_quarter)



#creating hierarchy structure, aggregation
dane_quarter_hts <- ts.dane_quarter %>%
  aggregate_key(DivisionName/StateName, MWh = sum(MWh))



#prognoza metodami benchmarkowymi
dane_train <- dane_quarter_hts %>%
  filter(year(Quarter) <= 2017,is_aggregated(StateName), is_aggregated(DivisionName))

#prognoza metodami benchmarkowymi
dane_test<- dane_quarter_hts %>%
  filter(year(Quarter) > 2017,is_aggregated(StateName), is_aggregated(DivisionName))


dane_fit <- dane_train %>%
    model(
      Mean = MEAN(MWh),
      Naive = NAIVE(MWh),
      Snaive = SNAIVE(MWh),
      Drift = RW(MWh ~ drift())
    )



dane_fc <- dane_fit %>%
  forecast(h = nrow(dane_test)) 

dane_fc %>% autoplot(level=NULL) + autolayer(dane_train)  + autolayer(dane_test) + labs (title = "Modele benchmarkowe - prognoza USA") + xlab("Kwartał [1Q]")



#### creating graphs

load_data <- function(name){
  
  data <- read.csv2(paste(sep="","~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wykresy/dane/",name,"_train.csv"))
  
  
  data <- pivot_longer (data, cols = 2:4, names_to = "measure", values_to = "value" )
  
  data$model <- name
  
  data
  
}


load_data_test <- function(name){
  
  data <- read.csv2(paste(sep="","~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wykresy/dane/",name,"_test.csv"))
  
  
  data <- pivot_longer (data, cols = 2:4, names_to = "measure", values_to = "value" )
  
  data$model <- name
  
  data
  
}

mean_train <- load_data("mean")
naive_train <- load_data("naive")
snaive_train <- load_data("snaive")
drift_train <- load_data("drift")

simple_methods_train <- rbind(mean_train, naive_train, snaive_train, drift_train)

colnames(simple_methods_train)[1]<- "level"

simple_methods_train %>%
  filter((measure == "RMSE")) -> simple_methods_train_rmse


simple_methods_train %>%
  filter((measure == "MAPE")) -> simple_methods_train_mape

simple_methods_train %>%
  filter((measure == "MASE")) -> simple_methods_train_mase


simple_methods_train %>% filter(!(measure == "MASE")) -> simple_methods_train

ggplot(simple_methods_train, aes(x=model, y=value, shape = level, color = level)) + 
  geom_point(size=3) + 
  facet_wrap(vars(measure), scales = "free") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Poziom hierarchii', shape = 'Poziom hierarchii', title = "Porównanie metod benchmarkowych - dane treningowe") 


## benchmarkowe test


mean_test <- load_data_test("mean")
naive_test <- load_data_test("naive")
snaive_test <- load_data_test("snaive")
drift_test <- load_data_test("drift")

simple_methods_test <- rbind(mean_test, naive_test, snaive_test, drift_test)

colnames(simple_methods_test)[1]<- "level"

simple_methods_test %>% filter(!(measure == "MASE")) -> simple_methods_test

ggplot(simple_methods_test, aes(x=model, y=value, shape = level, color = level)) + 
  geom_point(size=3) + 
  facet_wrap(vars(measure), scales = "free") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Poziom hierarchii', shape = 'Poziom hierarchii', title = "Porównanie metod benchmarkowych - dane testowe") 



####szereg z najmniejszym i najwiekszym bledem - SNAIVE

dane_train2 <- dane_quarter_hts %>%
  filter(year(Quarter) <= 2017)

dane_test2 <- dane_quarter_hts %>%
  filter(year(Quarter) > 2017)


dane_fit2 <- dane_train2 %>%
  model(
    Snaive = SNAIVE(MWh)
  )

dane_fc2 <- dane_fit2 %>%
  forecast(h = 16) 


dane_fit2 %>%
  filter(!is_aggregated(StateName),!is_aggregated(DivisionName)) %>%
  accuracy(
    measures = list(rmse = RMSE, mase = MASE, mape = MAPE)
  )  -> results_states_train

#train max
results_states_train %>% slice(which.max(mape)) -> results_states_train_max
# Montana

dane_test2 %>% filter(StateName == "Montana",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> montana_test
dane_fit2 %>% filter(StateName == "Montana",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> montana_fit
dane_train2 %>% filter(StateName == "Montana",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> montana_train
dane_fc2 %>% filter(StateName == "Montana",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> montana_fc

montana_train %>% autoplot(MWh, level= NULL) + autolayer(fitted(montana_fit))


#clrs <- c("blueviolet", "blue", "darkgoldenrod4")

montana_train %>%
  autoplot() +
  autolayer(fitted(montana_fit), color = "orange") + 
#  autolayer(montana_test, color = "blue") +
  guides(colour=guide_legend(title="Szeregi czasowe")) +
  xlab("Kwartał [1Q]") +
  ylab("Energia elektryczna [MWh]") +
  labs(title = "Montana") 

  # guides(colour=guide_legend(title="Data series"), 
  # fill=guide_legend(title="Prediction interval")) +
  # scale_color_manual(values=clrs)

# #  scale_color_manual(values=clrs)
# 
# 
# clrs <- c("blueviolet", "blue", "darkgoldenrod4", "red")
# 
# autoplot(montana_fc) +
#   autolayer(montana_fc$.mean, series="Forecast") +
#   autolayer(fitted(montana_fit), series='Fitted') + 
#   autolayer(montana_train, series = 'Train') +
#   autolayer(montana_test, series='Test') +
#   xlab("Observation [days]") +
#   ylab("Energy [Watts]") +
#   guides(colour=guide_legend(title="Data series"), 
#          fill=guide_legend(title="Prediction interval")) +
#   scale_color_manual(values=clrs)


# train min
results_states_train %>% slice(which.min(mape)) -> results_states_train_min
# Hawaii


dane_test2 %>% filter(StateName == "Hawaii",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Hawaii_test
dane_fit2 %>% filter(StateName == "Hawaii",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Hawaii_fit
dane_train2 %>% filter(StateName == "Hawaii",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Hawaii_train
dane_fc2 %>% filter(StateName == "Hawaii",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Hawaii_fc

Hawaii_train %>% autoplot(MWh, level= NULL) + autolayer(fitted(Hawaii_fit))


#clrs <- c("blueviolet", "blue", "darkgoldenrod4")

Hawaii_train %>%
  autoplot() +
  autolayer(fitted(Hawaii_fit), color = "orange") + 
#  autolayer(Hawaii_test, color = "blue") +
  guides(colour=guide_legend(title="Szeregi czasowe")) +
  xlab("Kwartał [1Q]") +
  ylab("Energia elektryczna [MWh]") +
  labs(title = "Hawaii") 



###### poprawic dane_fc2, dane_test2, dane_train2, gdzies tam jest blad


#### test data
dane_fc2 %>%
  filter(!is_aggregated(StateName),!is_aggregated(DivisionName)) %>%
  accuracy(
    data = dane_quarter_hts,
    measures = list(rmse = RMSE, mase = MASE, mape = MAPE)
  ) ->  results_states_test



#test max
results_states_test %>% slice(which.max(mape)) -> results_states_test_max
# New Mexico

dane_test2 %>% filter(StateName == "New Mexico",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> NewMexico_test
dane_fit2 %>% filter(StateName == "New Mexico",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> NewMexico_fit
dane_train2 %>% filter(StateName == "New Mexico",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> NewMexico_train
dane_fc2 %>% filter(StateName == "New Mexico",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> NewMexico_fc

NewMexico_train %>% autoplot(MWh, level= NULL) + autolayer(fitted(NewMexico_fit))


#clrs <- c("blueviolet", "blue", "darkgoldenrod4")

NewMexico_train %>%
  autoplot(level = NULL) +
#  autolayer(fitted(NewMexico_fit), color = "orange") + 
  autolayer(NewMexico_fc, color = "blue", level = NULL) +
  autolayer(NewMexico_test) +
  guides(colour=guide_legend(title="Szeregi czasowe")) +
  xlab("Kwartał [1Q]") +
  ylab("Energia elektryczna [MWh]") +
  labs(title = "New Mexico") 



# test min 
results_states_test %>% slice(which.min(mape)) -> results_states_test_min
# Vermont

dane_test2 %>% filter(StateName == "Vermont",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Vermont_test
dane_fit2 %>% filter(StateName == "Vermont",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Vermont_fit
dane_train2 %>% filter(StateName == "Vermont",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Vermont_train
dane_fc2 %>% filter(StateName == "Vermont",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Vermont_fc

Vermont_train %>% autoplot(MWh, level= NULL) + autolayer(fitted(Vermont_fit))


#clrs <- c("blueviolet", "blue", "darkgoldenrod4")

Vermont_train %>%
  autoplot(level = NULL) +
  #  autolayer(fitted(Vermont_fit), color = "orange") + 
  autolayer(Vermont_fc, color = "blue", level = NULL) +
  autolayer(Vermont_test) +
  guides(colour=guide_legend(title="Szeregi czasowe")) +
  xlab("Kwartał [1Q]") +
  ylab("Energia elektryczna [MWh]") +
  labs(title = "Vermont") 




#### PARAMETERS


epochs <- 1

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
  results_test <- "get_results_test(fc)"
  log.txt <- tic.log(format = TRUE)
  time_results <- as.data.frame(unlist(log.txt))
  tic.clearlog()
  
  
  
  results <- list(results_train, results_test, time_results)
  results
  
}

convert_to_vector <- function (results){
  
  vector <- round(c(unlist(results[,2]), unlist(results[,3]), unlist(results[,4]), unlist(results[,5])),2)
  vector
}

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
  results_test <- "na"
  log.txt <- tic.log(format = TRUE)
  time_results <- as.data.frame(unlist(log.txt))
  tic.clearlog()
  
  results <- list(results_train, results_test, time_results)
  results
  
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
  
  ### all series
  fit %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_all
  
  total_vector <- convert_to_vector(results_total)
  region_vector <- convert_to_vector(results_regions)
  states_vector <- convert_to_vector(results_states)
  all_vector <- convert_to_vector(results_all)
  
  results <- as.data.frame(matrix(NA,1,4))
  colnames(results) <- c("RMSE","MASE","MAPE", "CRPS")
  results[1,] <- total_vector
  results[2,] <- region_vector
  results[3,] <- states_vector
  results[4,] <- all_vector
  rownames(results) <- c("Country", "Regions", "States", "All series")
  
  results
  
}


results_non_hierarchical_arima <- non_hierarchical_forecast(ARIMA, epochs)
results_non_hierarchical_ets <- non_hierarchical_forecast(ETS, epochs)
results_non_hierarchical_snaive <- non_hierarchical_forecast(SNAIVE, epochs)
results_non_hierarchical_prophet <- non_hierarchical_prophet(period = 4, order = 2, type = "multiplicative")

#csv train measures
write_csv(results_non_hierarchical_arima[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_arima_train.csv")
write_csv(results_non_hierarchical_ets[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_ets_train.csv")
write_csv(results_non_hierarchical_snaive[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_snaive_train.csv")
write_csv(results_non_hierarchical_prophet[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_non_hierarchical_prophet_train.csv")



######## prognozowanie hierarchiczne






#results
results_hierarchical_arima <- hierarchical_forecast(ARIMA, FALSE)
results_hierarchical_ets <- hierarchical_forecast(ETS, FALSE)
results_hierarchical_prophet <- h_forecast_prophet(period = 4, order = 2, type = "multiplicative", FALSE)
results_hierarchical_snaive <- hierarchical_forecast(SNAIVE, FALSE)


results_hierarchical_snaive <- results_hierarchical_snaive[[5]]
results_hierarchical_snaive  %>% filter(is_aggregated(StateName),is_aggregated(DivisionName), .model =="base") -> results_hierarchical_snaive 

results_hierarchical_snaive$.model = "snaive"


results_hierarchical_snaive <-  as_tsibble(results_hierarchical_snaive)

str(results_hierarchical_snaive)


# #csv test measures
# write_csv(results_hierarchical_arima[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_arima.csv")
# write_csv(results_hierarchical_ets[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_ets.csv")
# write_csv(results_hierarchical_snaive[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_snaive.csv")
# write_csv(results_hierarchical_prophet[[2]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_prophet.csv")
# 
# 
# #csv train
# write_csv(results_hierarchical_arima[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_arima_train.csv")
# write_csv(results_hierarchical_ets[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_ets_train.csv")
# write_csv(results_hierarchical_snaive[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_snaive_train.csv")
# write_csv(results_hierarchical_prophet[[1]], "~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wyniki/results_hierarchical_prophet_train.csv")


#ets

results_hierarchical_ets[[5]] %>% filter(is_aggregated(StateName),is_aggregated(DivisionName)) %>% 
  autoplot(level=NULL) + autolayer(dane_train %>% filter(year(Quarter) >= 2011))  + autolayer(dane_test) +#autolayer(results_hierarchical_snaive, color = "blue") +
  labs (title = "Prognoza hierarchiczna dla USA - model bazowy ETS") + xlab("Kwartał [1Q]") + ylab("Energia elektryczna [MWh]")

#arima
results_hierarchical_arima[[5]] %>% filter(is_aggregated(StateName),is_aggregated(DivisionName)) %>% 
  autoplot(level=NULL) + autolayer(dane_train %>% filter(year(Quarter) >= 2011))  + autolayer(dane_test) + 
  labs (title = "Prognoza hierarchiczna dla USA - model bazowy ARIMA") + xlab("Kwartał [1Q]") + ylab("Energia elektryczna [MWh]")
#prophet
results_hierarchical_prophet[[5]] %>% filter(is_aggregated(StateName),is_aggregated(DivisionName)) %>% 
  autoplot(level=NULL) + autolayer(dane_train %>% filter(year(Quarter) >= 2011))  + autolayer(dane_test) + 
  labs (title = "Prognoza hierarchiczna dla USA - model bazowy Prophet") + xlab("Kwartał [1Q]") + ylab("Energia elektryczna [MWh]")

#snaive


hierarchical_forecast <- function(FUN, Time, ...){
  
  
  if (Time == TRUE){
    
    ##
    
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
  
  results <- list(results_train, results_test, time_results, fit, fc)
  results
  
}


h_forecast_prophet <- function (period, order, type, Time){
  
  
  if (Time == TRUE){
    
    
 ###
    
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
  
  results <- list(results_train, results_test, time_results, fit, fc)
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
  
  ### measures for all series
  fc %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase), mape = mean(mape), sspc = mean(ss)*100) -> results_all
  
  total_vector <- convert_to_vector(results_total)
  region_vector <- convert_to_vector(results_regions)
  states_vector <- convert_to_vector(results_states)
  all_vector <- convert_to_vector(results_all)
  
  results <- as.data.frame(matrix(NA,1,20))
  colnames(results) <- rep(c("RMSE", "MASE","MAPE","CRPS"),times = c(5,5,5,5))
  results[1,]  <- rep(c("BASE", "BU","MinT","OLS","TD"),times = 4)
  results[2,] <- total_vector
  results[3,] <- region_vector
  results[4,] <- states_vector
  results[5,] <- all_vector
  rownames(results) <- c("Methods","Country", "Regions", "States", "All series")
  
  results
  
}

convert_to_vector <- function (results){
  
  vector <- round(c(unlist(results[,2]), unlist(results[,3]), unlist(results[,4]), unlist(results[,5])),2)
  vector
}



#porownanie prognoz base dla kazdej metody



# ARIMA, ETS, Prophet, Snaive - 1 wersja
# ARIMA, ETS, Prophet - jako skill score do Snaive/Prognoz bazowych? - zrobic

# base train, base, bu, mint, ols, td



#####

load_data_advanced <- function(name){
  
  data <- read.csv2(paste(sep="","~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/wykresy/dane/hierarchical_",name,".csv"), header = F)
  data <- as.data.frame(t(data))
  colnames(data) <- data[1,]
  data <- data[-1,]
  data %>% arrange(Method) -> data
  data %>% split(data$Method) -> list_of_df
  final_df <- as.data.frame(matrix(NA,1,4))
  colnames(final_df) <- c("level", "measure", "value", "method")
  
  
  for (df in list_of_df){
    print(df)
    method <- df[1,2]
    df <- df[ , -which(names(df) %in% c("Method"))]
    df <- as.data.frame(t(df))
    df <- cbind(rownames(df),df)
    colnames(df) <- df[1,]
    df <- df[-1,]
    df<- pivot_longer (df, cols = 2:ncol(df), names_to = "measure", values_to = "value" )
    colnames(df)[1] <- "level"
    df$method <- method
    print(df)
    final_df <- rbind(final_df, df)
  }
  
  final_df <- final_df[-1,]
  final_df$model <- name
  
  final_df
}


df_ets <- load_data_advanced("ets")
df_arima <- load_data_advanced("arima")
df_prophet <- load_data_advanced("prophet")

df_snaive <- load_data_advanced("snaive")

advanced_methods <- rbind(df_ets, df_arima, df_prophet, df_snaive)
str(advanced_methods)

advanced_methods$value <- gsub(",",".", advanced_methods$value)
advanced_methods$value <- as.numeric(advanced_methods$value)

advanced_methods_usa <- advanced_methods %>% filter(level == "USA")

ggplot(advanced_methods_usa, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne  - porównanie metod rekoncyliacji i modeli bazowych - poziom USA ")


advanced_methods_Division <- advanced_methods %>% filter(level == "Division")

ggplot(advanced_methods_Division, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne - porównanie metod rekoncyliacji i modeli bazowych - poziom Division")


advanced_methods_State <- advanced_methods %>% filter(level == "State")

ggplot(advanced_methods_State, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne - porównanie metod rekoncyliacji i modeli bazowych - poziom State")


advanced_methods_All <- advanced_methods %>% filter(level == "All series")

ggplot(advanced_methods_All, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne - porównanie metod rekoncyliacji i modeli bazowych - wszystkie szeregi")


#skill score comparison


#calculating skill sscore


advanced_methods %>% filter(model=="snaive") -> methods_snaive

advanced_methods %>% filter(!model=="snaive") -> advanced_methods_ss


methods_snaive %>% mutate(key = paste(sep = "", level, measure, method)) -> methods_snaive

advanced_methods_ss %>% mutate(key = paste(sep = "", level, measure, method)) -> advanced_methods_ss


advanced_methods_ss %>% left_join(methods_snaive, by = "key") %>% select(-c("key", "level.y", "method.y", "model.y", "measure.y")) %>%
  mutate(measure = (value.y - value.x)/value.y) -> advanced_methods_ss


advanced_methods_ss %>% filter(measure.x == "CRPS") -> advanced_methods_ss_crps

advanced_methods_ss %>% filter(!measure.x == "CRPS") -> advanced_methods_ss_rest


advanced_methods_ss_crps <- advanced_methods_ss_crps[,-c(6,7)]


advanced_methods_ss_rest <- advanced_methods_ss_rest[,-c(3,6)]


colnames(advanced_methods_ss_rest) <- c("level",   "measure", "method",  "model",   "value"  )
colnames(advanced_methods_ss_crps) <- c("level",   "measure",   "value", "method",  "model"  )


advanced_methods_ss <- rbind(advanced_methods_ss_crps, advanced_methods_ss_rest)


## prognoza punktowa
advanced_methods_ss_usa <- advanced_methods_ss_rest %>% filter(level == "USA")

#eksperyment 1
advanced_methods_ss_usa <- advanced_methods_ss_rest %>% filter(level == "USA", method %in% c("BASE Train", "BASE Test"), !measure =="MASE")

ggplot(advanced_methods_ss_usa, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne  - porównanie metod rekoncyliacji i modeli bazowych - USA ", subtitle = "Skill score względem SNAIVE") +
  geom_hline(yintercept=0, linetype="dashed",
             color = "black", size=0.5)

#eksperyment 1
ggplot(advanced_methods_ss_usa, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognoza bazowa na poziomie USA za pomocą ARIMA, ETS i Prophet", subtitle = "Skill score względem SNAIVE") +
  geom_hline(yintercept=0, linetype="dashed",
             color = "black", size=0.5)


advanced_methods_ss_Division <- advanced_methods_ss_rest %>% filter(level == "Division")

ggplot(advanced_methods_ss_Division, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne - porównanie metod rekoncyliacji i modeli bazowych - Division", subtitle = "Skill score względem SNAIVE")+
  geom_hline(yintercept=0, linetype="dashed",
             color = "black", size=0.5)



advanced_methods_ss_State <- advanced_methods_ss_rest %>% filter(level == "State")
advanced_methods_ss_State <- advanced_methods_ss_rest %>% filter(level == "All series")

#eksperyment 2
advanced_methods_ss_State <- advanced_methods_ss_rest %>% filter(level == "State", method %in% c("BASE Train", "BASE Test"), !measure =="MASE")
#advanced_methods_ss_State <- advanced_methods_ss_rest %>% filter(level == "All series", method %in% c("BASE Train", "BASE Test"))

ggplot(advanced_methods_ss_State, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne - porównanie metod rekoncyliacji i modeli bazowych - State", subtitle = "Skill score względem SNAIVE")+
  geom_hline(yintercept=0, linetype="dashed",
             color = "black", size=0.5)

#eksperyment 2
ggplot(advanced_methods_ss_State, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognoza bazowa na poziomie State za pomocą ARIMA, ETS i Prophet", subtitle = "Skill score względem SNAIVE")+
  geom_hline(yintercept=0, linetype="dashed",
             color = "black", size=0.5)

advanced_methods_ss_All <- advanced_methods_ss_rest %>% filter(level == "All series")

ggplot(advanced_methods_ss_All, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne - porównanie metod rekoncyliacji i modeli bazowych - wszystkie szeregi", subtitle = "Skill score względem SNAIVE")+
  geom_hline(yintercept=0, linetype="dashed",
             color = "black", size=0.5)

###prognoza przedzialowa

advanced_methods_ss_usa <- advanced_methods_ss_crps %>% filter(level == "USA")

ggplot(advanced_methods_ss_usa, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne  - porównanie metod rekoncyliacji i modeli bazowych - poziom USA ", subtitle = "Skill score względem SNAIVE") +
  geom_hline(yintercept=0, linetype="dashed",
             color = "black", size=0.5)


advanced_methods_ss_Division <- advanced_methods_ss_crps %>% filter(level == "Division")

ggplot(advanced_methods_ss_Division, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne - porównanie metod rekoncyliacji i modeli bazowych - poziom Division", subtitle = "Skill score względem SNAIVE")+
  geom_hline(yintercept=0, linetype="dashed",
             color = "black", size=0.5)



advanced_methods_ss_State <- advanced_methods_ss_crps %>% filter(level == "State")

ggplot(advanced_methods_ss_State, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne - porównanie metod rekoncyliacji i modeli bazowych - poziom State", subtitle = "Skill score względem SNAIVE")+
  geom_hline(yintercept=0, linetype="dashed",
             color = "black", size=0.5)


advanced_methods_ss_All <- advanced_methods_ss_crps %>% filter(level == "All series", !method == "BASE Train")

ggplot(advanced_methods_ss_All, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne - porównanie metod rekoncyliacji i modeli bazowych - wszystkie szeregi", subtitle = "Skill score względem SNAIVE")+
  geom_hline(yintercept=0, linetype="dashed",
             color = "black", size=0.5)


advanced_methods_ss_crps$level <- factor(advanced_methods_ss_crps$level, levels=c('USA','Division','State'))

advanced_methods_ss_crps %>% filter(!(method == "BASE Train")) -> advanced_methods_ss_crps

ggplot(advanced_methods_ss_crps, aes(x=model, y=value, shape = method, color = method)) + 
  geom_point(size=3) + 
  facet_wrap(~level, scales = "free_y") +
  xlab("Model") + ylab ("Wartość miary błędu") +
  labs(color='Metody', shape = 'Metody', title = "Prognozowanie hierarchiczne  - porównanie metod rekoncyliacji i modeli bazowych", subtitle = "Skill score CRPS względem SNAIVE") +
  geom_hline(yintercept=0, linetype="dashed",
             color = "black", size=0.5)


####### time avg and std dev comparison


load_time_hierarchical <- function(name){
  
  time_data <- read_delim(paste(sep="","~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_hierarchical_",name,".csv"),delim = ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
  time_data %>% separate(X3, c("model", "seconds"), " ") -> time_data
  time_data$model <- gsub(":","", time_data$model)
  time_data <- time_data[,-1]
  colnames(time_data)[1] <- "method"
  time_data$seconds <- as.numeric(time_data$seconds)
  
  time_data
}


#loading files
time_hierarchical_arima <- load_time_hierarchical("arima")
time_hierarchical_ets <- load_time_hierarchical("ets")


#prophet
name <- "prophet"
time_hierarchical_prophet <- read_delim(paste(sep="","~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_hierarchical_",name,".csv"),delim = ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
time_hierarchical_prophet %>% separate(X1, c("type","method","model"), ";") -> time_hierarchical_prophet
time_hierarchical_prophet %>% separate(model, c("model","seconds","remainder"), " ") -> time_hierarchical_prophet
time_hierarchical_prophet$model <- gsub(":","", time_hierarchical_prophet$model)
time_hierarchical_prophet <- time_hierarchical_prophet[,-c(1,5)]




load_time_nonhierarchical <- function(name){
  
  time_data <- read_delim(paste(sep="","~/Library/CloudStorage/OneDrive-Personal/Praca magisterska/czas trwania wyniki/time_non_hierarchical_",name,".csv"), delim = ":", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
  time_data %>% separate(X3, c("seconds"), " ") -> time_data
  time_data <- time_data[,-1]
  colnames(time_data)[1] <-"model"
  time_data$method <- "BASE"
  
  time_data
}

time_arima <- load_time_nonhierarchical("arima")
time_ets <- load_time_nonhierarchical("ets")
time_snaive <- load_time_nonhierarchical("snaive")
time_prophet <- load_time_nonhierarchical("prophet")
time_prophet$model <- "Prophet"

time_all <- rbind(time_hierarchical_arima, time_hierarchical_ets, time_hierarchical_prophet, time_arima, time_ets, time_snaive, time_prophet)
time_all$seconds <- as.numeric(time_all$seconds)
str(time_all)



library(ggthemes)
g <- ggplot(time_all, aes(model, seconds))
g + geom_boxplot(aes(fill=factor(method))) +
  labs(title="Porównanie czasów wykonania metod rekoncyliacji oraz modeli bazowych", 
       x="Model",
       y="Czas [s]") + guides(fill=guide_legend(title="Metody"))



time_all %>% group_by(model, method) %>% summarise(mean = mean(seconds), sd = sd(seconds) ) -> time_all_agg

write_csv(time_all_agg, "time.csv")

ggplot(time_all_agg, aes(x=model, y=mean, shape = method, color = method)) + 
  geom_point(size=8) + 
#  facet_wrap(~measure, scales = "free_y") +
  xlab("Model") + ylab ("Czas [s]") +
  labs(color='Metody', shape = 'Metody', title = "Porównanie czasów wykonania metod rekoncyliacji oraz modeli bazowych")



###### biggest error measure


epochs <- 1

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
  results_test <- "get_results_test(fc)"
  log.txt <- tic.log(format = TRUE)
  time_results <- as.data.frame(unlist(log.txt))
  tic.clearlog()
  
  
  
  results <- list(fit, fc, results_train, time_results)
  results
  
}



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
  results_test <- "na"
  log.txt <- tic.log(format = TRUE)
  time_results <- as.data.frame(unlist(log.txt))
  tic.clearlog()
  
  results <- list(fit, fc, results_train, time_results)
  results
  
}

get_results_train <- function (fit){
  
  fit %>%
    filter(is_aggregated(StateName), is_aggregated(DivisionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    )  -> results_total
  
  
  ### measures for each region (regions)
  fit %>%
    filter(is_aggregated(StateName), !is_aggregated(DivisionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    )  -> results_regions
  
  ### measures for each region (states) - bottom level
  fit %>%
    filter(!is_aggregated(StateName),!is_aggregated(DivisionName)) %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    )  -> results_states
  
  ### all series
  fit %>%
    accuracy(
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) -> results_all
  
  results <- list(results_total,results_regions, results_states, results_all )
  
  results
  
}

hierarchical_forecast <- function(FUN, Time, ...){
  
  
  if (Time == TRUE){
    
    ##
    
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
  
  results <- list(fit, fc, results_test, time_results)
  results
  
}


h_forecast_prophet <- function (period, order, type, Time){
  
  
  if (Time == TRUE){
    
    
    ###
    
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
  
  results <- list(fit, fc, results_test, time_results)
  results
}




get_results_test_hierarchical <- function (fc){
  ### measures for whole country (total)
  
  fc %>%
    filter(is_aggregated(StateName), is_aggregated(DivisionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) -> results_total
  
  
  ### measures for each region (regions)
  fc %>%
    filter(is_aggregated(StateName), !is_aggregated(DivisionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    )  -> results_regions
  
  ### measures for each region (states) - bottom level
  fc %>%
    filter(!is_aggregated(StateName),!is_aggregated(DivisionName)) %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    )  -> results_states
  
  ### measures for all series
  fc %>%
    accuracy(
      data = dane_quarter_hts,
      measures = list(rmse = RMSE, mase = MASE, mape = MAPE, ss = skill_score(CRPS))
    ) -> results_all
  
  results <- list(results_total,results_regions, results_states, results_all )
  
  results
  
}


?skillsc





####szereg z najmniejszym i najwiekszym bledem - SNAIVE
#  results <- list(fit, fc, results_test, time_results)

single_state_train <- function (train_results, measure){
  dane_train2 <- dane_quarter_hts %>%
    filter(year(Quarter) <= 2017)
  
  dane_test2 <- dane_quarter_hts %>%
    filter(year(Quarter) > 2017)
  
  
  dane_fit2 <- train_results [[1]]
  
  dane_fc2 <- train_results [[2]]
  
  
  train_results[[3]][[4]]  -> results_states_train
  
#  fmeasure <- paste(sep = "","which.", measure)
  
  #train max
  results_states_train %>% slice(measure(mape)) -> results_states_train_max
  
  state <- as.character(results_states_train_max$StateName[1])
  error <- as.character(results_states_train_max$mape[1])
  print(paste("MAPE: ", error))
  
  dane_test2 %>% filter(StateName == state,!is_aggregated(StateName),!is_aggregated(DivisionName)) -> montana_test
  dane_fit2 %>% filter(StateName == state,!is_aggregated(StateName),!is_aggregated(DivisionName)) -> montana_fit
  dane_train2 %>% filter(StateName == state,!is_aggregated(StateName),!is_aggregated(DivisionName)) -> montana_train
  dane_fc2 %>% filter(StateName == state,!is_aggregated(StateName),!is_aggregated(DivisionName)) -> montana_fc
  
  montana_train %>% autoplot(MWh, level= NULL) + autolayer(fitted(montana_fit))
  
  
  #clrs <- c("blueviolet", "blue", "darkgoldenrod4")
  
  montana_train %>%
    autoplot() +
    autolayer(fitted(montana_fit), color = "orange") + 
    #  autolayer(montana_test, color = "blue") +
    guides(colour=guide_legend(title="Szeregi czasowe")) +
    xlab("Kwartał [1Q]") +
    ylab("Energia elektryczna [MWh]") +
    labs(title = state) 
  
  
  
}


single_state_test <- function (test_results, measure){
  dane_train2 <- dane_quarter_hts %>%
    filter(year(Quarter) <= 2017)
  
  dane_test2 <- dane_quarter_hts %>%
    filter(year(Quarter) > 2017)
  
  
  dane_fit2 <- test_results [[1]]
  
  dane_fc2 <- test_results [[2]]
  
  
  test_results[[3]][[4]]  -> results_states_train
  #  fmeasure <- paste(sep = "","which.", measure)
  
  #train max
  results_states_train %>% slice(measure(mape)) -> results_states_train_max
  
  state <- as.character(results_states_train_max$StateName[1])
  error <- as.character(results_states_train_max$mape[1])
  print(paste("MAPE: ", error)) 
  
  dane_test2 %>% filter(StateName == state,!is_aggregated(StateName),!is_aggregated(DivisionName)) -> montana_test
  dane_fit2 %>% filter(StateName == state,!is_aggregated(StateName),!is_aggregated(DivisionName)) -> montana_fit
  dane_train2 %>% filter(StateName == state,!is_aggregated(StateName),!is_aggregated(DivisionName)) -> montana_train
  dane_fc2 %>% filter(StateName == state,!is_aggregated(StateName),!is_aggregated(DivisionName), .model == "base") -> montana_fc
  
  montana_train %>% autoplot(MWh, level= NULL) + autolayer(fitted(montana_fit))
  
  
  #clrs <- c("blueviolet", "blue", "darkgoldenrod4")
  
  montana_train %>%
    autoplot(level = NULL) +
    autolayer(fitted(montana_fit), color = "orange") + 
    autolayer(montana_fc, color = "blue", level = NULL) +
    autolayer(montana_test) +
    guides(colour=guide_legend(title="Szeregi czasowe")) +
    xlab("Kwartał [1Q]") +
    ylab("Energia elektryczna [MWh]") +
    labs(title = state) 
 
  
}


#train

results_non_hierarchical_arima <- non_hierarchical_forecast(ARIMA, epochs)
results_non_hierarchical_ets <- non_hierarchical_forecast(ETS, epochs)
results_non_hierarchical_snaive <- non_hierarchical_forecast(SNAIVE, epochs)
results_non_hierarchical_prophet <- non_hierarchical_prophet(period = 4, order = 2, type = "multiplicative")


results_non_hierarchical_prophet[[3]][[4]] %>% filter(StateName == "Kentucky")
results_hierarchical_prophet[[3]][[4]] %>% filter(StateName == "Kentucky")

#test 
results_hierarchical_arima <- hierarchical_forecast(ARIMA, FALSE)
results_hierarchical_ets <- hierarchical_forecast(ETS, FALSE)
results_hierarchical_prophet <- h_forecast_prophet(period = 4, order = 2, type = "multiplicative", FALSE)
results_hierarchical_snaive <- hierarchical_forecast(SNAIVE, FALSE)


# dane treningowe najwiekszy i najmniejszy blad

single_state_train(results_non_hierarchical_prophet, which.max)

#train_results <- results_non_hierarchical_arima
#measure <- which.max

# dane testowe najwiekszy i najmniejszy blad

single_state_test(results_hierarchical_arima, which.min)
single_state_test(results_hierarchical_ets, which.max)
single_state_test(results_hierarchical_prophet, which.max)

#test_results <- results_hierarchical_ets
#measure <- which.max





###### poprawic dane_fc2, dane_test2, dane_train2, gdzies tam jest blad


#### test data
dane_fc2 %>%
  filter(!is_aggregated(StateName),!is_aggregated(DivisionName)) %>%
  accuracy(
    data = dane_quarter_hts,
    measures = list(rmse = RMSE, mase = MASE, mape = MAPE)
  ) ->  results_states_test



#test max
results_states_test %>% slice(which.max(mape)) -> results_states_test_max
# New Mexico

dane_test2 %>% filter(StateName == "New Mexico",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> NewMexico_test
dane_fit2 %>% filter(StateName == "New Mexico",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> NewMexico_fit
dane_train2 %>% filter(StateName == "New Mexico",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> NewMexico_train
dane_fc2 %>% filter(StateName == "New Mexico",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> NewMexico_fc

NewMexico_train %>% autoplot(MWh, level= NULL) + autolayer(fitted(NewMexico_fit))


#clrs <- c("blueviolet", "blue", "darkgoldenrod4")

NewMexico_train %>%
  autoplot(level = NULL) +
  #  autolayer(fitted(NewMexico_fit), color = "orange") + 
  autolayer(NewMexico_fc, color = "blue", level = NULL) +
  autolayer(NewMexico_test) +
  guides(colour=guide_legend(title="Szeregi czasowe")) +
  xlab("Kwartał [1Q]") +
  ylab("Energia elektryczna [MWh]") +
  labs(title = "New Mexico") 



# test min 
results_states_test %>% slice(which.min(mape)) -> results_states_test_min
# Vermont

dane_test2 %>% filter(StateName == "Vermont",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Vermont_test
dane_fit2 %>% filter(StateName == "Vermont",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Vermont_fit
dane_train2 %>% filter(StateName == "Vermont",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Vermont_train
dane_fc2 %>% filter(StateName == "Vermont",!is_aggregated(StateName),!is_aggregated(DivisionName)) -> Vermont_fc

Vermont_train %>% autoplot(MWh, level= NULL) + autolayer(fitted(Vermont_fit))


#clrs <- c("blueviolet", "blue", "darkgoldenrod4")

Vermont_train %>%
  autoplot(level = NULL) +
  #  autolayer(fitted(Vermont_fit), color = "orange") + 
  autolayer(Vermont_fc, color = "blue", level = NULL) +
  autolayer(Vermont_test) +
  guides(colour=guide_legend(title="Szeregi czasowe")) +
  xlab("Kwartał [1Q]") +
  ylab("Energia elektryczna [MWh]") +
  labs(title = "Vermont") 



