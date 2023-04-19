library(fpp3)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)


#Total Crude Oil Exports by Destination



raw_crude_oil <- read_csv("https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/Total%20Crude%20Oil%20Exports%20by%20Destination_EIA.csv?", skip = 2)

names(raw_crude_oil)[-2] <- str_replace_all(names(raw_crude_oil)[-2],
                                            c("U.S. Exports to " = "",
                                              " of Crude Oil \\(Thousand Barrels\\)" = ""))




crude_oil <- raw_crude_oil %>% 
  pivot_longer(cols = -Date,
               names_to = 'Destination country',
               values_to = 'Amount of Crude Oil (Thousand Barrels)')

crude_oil$Date <- as.Date(crude_oil$Date, format = "%m/%d/%Y")
crude_oil$`Amount of Crude Oil (Thousand Barrels)` <- replace_na(crude_oil$`Amount of Crude Oil (Thousand Barrels)`, 0)

ts_crude_oil <- crude_oil %>% 
  mutate(Date = yearmonth(Date)) %>% 
  as_tsibble(
    index = Date, 
    key = `Destination country`
  )

duplicates()

#Total Oil Products Exports by Destination

raw_oil_products <- read_csv("https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/Total%20Oil%20Products%20Exports%20by%20Destination_EIA.csv?",  skip = 2)

names(raw_oil_products)[-2] <- str_replace_all(names(raw_oil_products)[-2],
                                           c("U.S. Exports to " = "",
                                             " of Total Petroleum Products \\(Thousand Barrels\\)" = ""))
oil_products <- raw_oil_products %>% 
  pivot_longer(cols = -Date,
               names_to = 'Destination country',
               values_to = 'Amount of total Petroleum Porducts (Thousand Barrels)')

oil_products$Date <- as.Date(oil_products$Date, format = "%m/%d/%Y")
oil_products$`Amount of total Petroleum Porducts (Thousand Barrels)` <- replace_na(oil_products$`Amount of total Petroleum Porducts (Thousand Barrels)`, 0)

ts_oil_products <- oil_products %>% 
  as_tsibble(
    index = Date, 
    key = `Destination country`
  )


#Total Natural Gas Exports by Destination


