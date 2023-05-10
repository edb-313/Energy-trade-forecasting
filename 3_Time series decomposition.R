#Time series decomposition 

########### Crude Oil Exports ##################################################################

#ploting the data

oecd_crude_oil_agg <- oecd_crude_oil %>%
  index_by(Date) %>%
  summarize_at(vars(`Amount of Crude Oil (Thousand Barrels)`), sum) %>%
  ungroup()

oecd_crude_oil_agg

oecd_crude_oil_agg <- oecd_crude_oil_agg %>%
  mutate(Date = yearmonth(Date))
         

oecd_crude_oil_agg %>% 
  autoplot(`Amount of Crude Oil (Thousand Barrels)`)


#finding lambda for box cox transformation

oecd_crude_oil_agg %>% features(`Amount of Crude Oil (Thousand Barrels)`, features = guerrero)

lambda <- oecd_crude_oil_agg %>% 
  features(`Amount of Crude Oil (Thousand Barrels)`, features = guerrero) %>% 
  pull(lambda_guerrero)


#box cox transformation

oecd_crude_oil_agg <- oecd_crude_oil_agg %>% 
  mutate(`Amount of Crude Oil (Thousand Barrels)` = box_cox(`Amount of Crude Oil (Thousand Barrels)`, lambda))

oecd_crude_oil_agg

#decomposition

dcmp_oecd_crude_oil <- oecd_crude_oil_agg %>%
  model(stl = STL(`Amount of Crude Oil (Thousand Barrels)`))


components(dcmp_oecd_crude_oil)

#ploting the components

components(dcmp_oecd_crude_oil) %>% autoplot()

#trend component against all components

oecd_crude_oil_agg %>%
  autoplot(`Amount of Crude Oil (Thousand Barrels)`, color='gray') +
  autolayer(components(dcmp_oecd_crude_oil), trend, color='red') +
  xlab("Year") + ylab("Barels thousands") +
  ggtitle("U.S. Exports of Crude Oil to OECD Europe (Thousand Barrels)")

#seasonal component

components(dcmp_oecd_crude_oil) %>% gg_subseries(season_year)


#######################################################################################
install.packages("seasonal")
install.packages("XQuartz")
library(seasonal)
library(Xqua)

x11_dcmp <- oecd_crude_oil_agg %>% 
  model(x11 = X_13ARIMA_SEATS(oecd_crude_oil(`Amount of Crude Oil (Thousand Barrels)` ~ x11()))) %>% 
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of US Crude oil exports to OECD Europe using X-11.")


###########  Oil Products Exports #######################################################

#ploting the data

oecd_oil_prod_agg <- oecd_oil_prod %>%
  index_by(Date) %>%
  summarize_at(vars(`Amount of total Petroleum Porducts (Thousand Barrels)`), sum) %>%
  ungroup()

oecd_oil_prod_agg

oecd_oil_prod_agg <- oecd_oil_prod_agg %>%
  mutate(Date = yearmonth(Date))


oecd_oil_prod_agg %>% 
  autoplot(`Amount of total Petroleum Porducts (Thousand Barrels)`)


#finding lambda for box cox transformation

oecd_oil_prod_agg %>% features(`Amount of total Petroleum Porducts (Thousand Barrels)`, features = guerrero)

#box cox transformation

oecd_oil_prod_agg %>% autoplot(box_cox(`Amount of total Petroleum Porducts (Thousand Barrels)`, 0.73)) +
  labs(y = "Box-Cox transformed exportamounts")

#decomposition

dcmp_oecd_oil_prod <- oecd_oil_prod_agg %>%
  model(stl = STL(`Amount of total Petroleum Porducts (Thousand Barrels)`))


components(dcmp_oecd_oil_prod)

#ploting the components

components(dcmp_oecd_oil_prod) %>% autoplot()

#trend component against all components

oecd_oil_prod_agg %>%
  autoplot(`Amount of total Petroleum Porducts (Thousand Barrels)`, color='gray') +
  autolayer(components(dcmp_oecd_oil_prod), trend, color='red') +
  xlab("Year") + ylab("Barels thousands") +
  ggtitle("U.S. Exports of Petroleum Porducts to OECD Europe (Thousand Barrels)")

#seasonal oomponent

components(dcmp_oecd_oil_prod) %>% gg_subseries(season_year)


########### Natural Gas Exports #######################################################


#ploting the data

oecd_nat_gas_agg <- oecd_nat_gas %>%
  index_by(Date) %>%
  summarize_at(vars(`Amount of Natural gas (MMcf)`), sum) %>%
  ungroup()

oecd_nat_gas_agg

oecd_nat_gas_agg <- oecd_nat_gas_agg %>%
  mutate(Date = yearmonth(Date))


oecd_nat_gas_agg %>% 
  autoplot(`Amount of Natural gas (MMcf)`)


#finding lambda for box cox transformation

oecd_nat_gas_agg %>% features(`Amount of Natural gas (MMcf)`, features = guerrero)

#box cox transformation

oecd_nat_gas_agg %>% autoplot(box_cox(`Amount of Natural gas (MMcf)`, 0.390)) +
  labs(y = "Box-Cox transformed exportamounts")

#decomposition

dcmp_oecd_nat_gas <- oecd_nat_gas_agg %>%
  model(stl = STL(`Amount of Natural gas (MMcf)`))


components(dcmp_oecd_nat_gas)

#ploting the components

components(dcmp_oecd_nat_gas) %>% autoplot()

#trend component against all components

oecd_nat_gas_agg %>%
  autoplot(`Amount of Natural gas (MMcf)`, color='gray') +
  autolayer(components(dcmp_oecd_nat_gas), trend, color='red') +
  xlab("Year") + ylab("Amount of Natural Gas (MMcf") +
  ggtitle("U.S. Exports of Natural Gas to OECD Europe (MMcf)")

#seasonal oomponent

components(dcmp_oecd_nat_gas) %>% gg_subseries(season_year)
