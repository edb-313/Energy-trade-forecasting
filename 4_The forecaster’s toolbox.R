#4_The forecaster’s toolbox

########################### Crude Oil Exports ###################################

oecd_crude_oil_agg %>% 
  autoplot(`Amount of Crude Oil (Thousand Barrels)`)

#fiting the models

crude_oil_fit <- oecd_crude_oil_agg %>% 
  model(
    Seasonal_naive = SNAIVE(`Amount of Crude Oil (Thousand Barrels)`),
    Naive = NAIVE(`Amount of Crude Oil (Thousand Barrels)`),
    Drift = RW(`Amount of Crude Oil (Thousand Barrels)` ~ drift()),
    Mean = MEAN(`Amount of Crude Oil (Thousand Barrels)`)
  )

crude_oil_fit

#forecasting for 12 months 

crude_oil_fc <- crude_oil_fit %>% 
  forecast(h = 12)

crude_oil_fc %>% 
  print(n = 36)

#plotting all forecasting models against historical data

crude_oil_fc %>% 
  autoplot(oecd_crude_oil_agg, level = NULL) +
  ggtitle("Forecasts for U.S. Crude Oil exports to OECD Europe") +
  xlab("Year") + ylab("Thousand Barrels") +
  guides(colour = guide_legend(title = "Forecast"))
  

########################### Oil Porducts Exports ################################


#fiting the models

oil_prod_fit <- oecd_oil_prod_agg %>% 
  model(
    Seasonal_naive = SNAIVE(`Amount of total Petroleum Porducts (Thousand Barrels)`),
    Naive = NAIVE(`Amount of total Petroleum Porducts (Thousand Barrels)`),
    Drift = RW(`Amount of total Petroleum Porducts (Thousand Barrels)` ~ drift()),
    Mean = MEAN(`Amount of total Petroleum Porducts (Thousand Barrels)`)
  )

crude_oil_fit

#forecasting for 12 months 

oil_prod_fc <- oil_prod_fit %>% 
  forecast(h = 12)

crude_oil_fc

#plotting all forecasting models against historical data

oil_prod_fc %>% 
  autoplot(oecd_oil_prod_agg, level = NULL) +
  ggtitle("Forecasts for U.S. Petroleum products exports to OECD Europe") +
  xlab("Year") + ylab("Thousand Barrels") +
  guides(colour = guide_legend(title = "Forecast"))


########################### Oil Porducts Exports ################################


#fiting the models

nat_gas_fit <- oecd_nat_gas_agg %>% 
  model(
    Seasonal_naive = SNAIVE(`Amount of Natural gas (MMcf)`),
    Naive = NAIVE(`Amount of Natural gas (MMcf)`),
    Drift = RW(`Amount of Natural gas (MMcf)` ~ drift()),
    Mean = MEAN(`Amount of Natural gas (MMcf)`)
  )

nat_gas_fit

#forecasting for 12 months 

nat_gas_fc <- nat_gas_fit %>% 
  forecast(h = 12)

nat_gas_fc

#plotting all forecasting models against historical data

nat_gas_fc %>% 
  autoplot(oecd_nat_gas_agg, level = NULL) +
  ggtitle("Forecasts for U.S. Natural Gas exports to OECD Europe") +
  xlab("Year") + ylab("Amount of Natural gas (MMcf") +
  guides(colour = guide_legend(title = "Forecast"))

###############################################################################
#Unnecessary part

fit_crude_oil <- oecd_crude_oil_agg %>% 
  model(SNAIVE(`Amount of Crude Oil (Thousand Barrels)`))

fit_crude_oil %>% augment() %>% tail()

augment(fit_crude_oil) %>% 
  ggplot(aes(x = oecd_crude_oil_agg$Date)) +
  geom_line(aes(y = oecd_crude_oil_agg$`Amount of Crude Oil (Thousand Barrels)`, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) #possible forecast inaccuracy due to Covid-19


#Evaluating point forecast accuracy

oecd_crude_oil_agg

#creating a training data set

crude_oil_train <- oecd_crude_oil_agg %>% 
  filter(Date >= as.Date("2015-06-01"), Date <= as.Date("2021-06-01"))

#fiting all four models

crude_oil_fit <- crude_oil_train %>% 
  model(
    Mean = MEAN(`Amount of Crude Oil (Thousand Barrels)`),
    Naive = NAIVE(`Amount of Crude Oil (Thousand Barrels)`),
    SNaive = SNAIVE(`Amount of Crude Oil (Thousand Barrels)`),
    Drift = RW(`Amount of Crude Oil (Thousand Barrels)` ~ drift())
  )

crude_oil_fit

crude_oil_fc <- crude_oil_fit %>% 
  forecast(h = 36)

#Time series cross validation

accuracy(crude_oil_fit) %>% 
  arrange(.model) %>% 
  select(.model, .type, RMSE, MAE, MAPE, MASE, RMSSE)

#plotting all for models 

crude_oil_fc %>% 
  autoplot(
    oecd_crude_oil_agg,
    level = NULL
  ) +
  labs(
    y = "Thousand Barrels",
    title = "Forecasts for U.S. Crude Oil exports to OECD Europe"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

#Inspecting residuals

naive_crude_oil <- crude_oil_train %>% 
  model(NAIVE(`Amount of Crude Oil (Thousand Barrels)`))

augment(naive_crude_oil) %>% 
  ggplot(aes(x = crude_oil_train$Date))+
  geom_line(aes(y = crude_oil_train$`Amount of Crude Oil (Thousand Barrels)`, colour = 'Data')) +
  geom_abline(aes(y = .ftted, colour = 'Fitted'))

#All residual diagnostic graphs
gg_tsresiduals(naive_crude_oil)

#plotting all residuals
augment(naive_crude_oil) %>% 
  autoplot(.resid) +
  labs(y = 'Thousand Barrels',
       title = 'Residuals from the Naive model')
#Histogram
augment(naive_crude_oil) %>% 
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 50) +
  labs(title = 'Histogram of naive model residuals')

#ACF
augment(naive_crude_oil) %>% 
  ACF(.resid) %>% 
  autoplot() + labs(title = 'ACF of residuals')


#Portmanteau test

augment(naive_crude_oil) %>% 
  features(.resid, ljung_box, lag = 50)
#do not reject null hypothesis of no autocorrelation (H0 - no autocorrelation in the residuals)
#model seems to adequately capture the patterns in the data, at least with respect to autocorrelation



################################################################################


#Evaluating point forecast accuracy

oecd_oil_prod_agg

#creating a training data set

oil_prod_train <- oecd_oil_prod_agg %>% 
  filter(Date <= as.Date("2020-01-01"))
oil_prod_train

#fiting all four models

oil_prod_fit <- oil_prod_train %>% 
  model(
    Mean = MEAN(`Amount of total Petroleum Porducts (Thousand Barrels)`),
    Naive = NAIVE(`Amount of total Petroleum Porducts (Thousand Barrels)`),
    SNaive = SNAIVE(`Amount of total Petroleum Porducts (Thousand Barrels)`),
    Drift = RW(`Amount of total Petroleum Porducts (Thousand Barrels)` ~ drift())
  )

oil_prod_fit

oil_prod_fc <- oil_prod_fit %>% 
  forecast(h = 36)

#evaluating the accuracy

accuracy(oil_prod_fit) %>% 
  arrange(.model) %>% 
  select(.model, .type, RMSE, MAE, MAPE, MASE, RMSSE)

#plotting all for models 

oil_prod_fc %>% 
  autoplot(
    oecd_oil_prod_agg,
    level = NULL
  ) +
  labs(
    y = "Thousand Barrels",
    title = "Forecasts for U.S. Petroleum Products exports to OECD Europe"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

#Inspecting residuals

mean_oil_prod <- oil_prod_train %>% 
  model(MEAN(`Amount of total Petroleum Porducts (Thousand Barrels)`))

#not working 
augment(mean_oil_prod) %>% 
  ggplot(aes(x = oil_prod_train$Date))+
  geom_line(aes(y = oil_prod_train$`Amount of total Petroleum Porducts (Thousand Barrels)`, colour = 'Data')) +
  geom_abline(aes(y = .ftted, colour = 'Fitted'))

#All residual diagnostic graphs
gg_tsresiduals(mean_oil_prod)

#plotting all residuals
augment(mean_oil_prod) %>% 
  autoplot(.resid) +
  labs(y = 'Thousand Barrels',
       title = 'Residuals from the Naive model')
#Histogram
augment(mean_oil_prod) %>% 
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 50) +
  labs(title = 'Histogram of naive model residuals')

#ACF
augment(mean_oil_prod) %>% 
  ACF(.resid) %>% 
  autoplot() + labs(title = 'ACF of residuals')


#Portmanteau test

augment(mean_oil_prod) %>% 
  features(.resid, ljung_box, lag = 10)
#do not reject null hypothesis of no autocorrelation (H0 - no autocorrelation in the residuals)
#model seems to adequately capture the patterns in the data, at least with respect to autocorrelation
