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
