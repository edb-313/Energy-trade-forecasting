library(fpp3)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

################################################################################


#Total Crude Oil Exports by Destination



raw_crude_oil <- read_csv("https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/Total%20Crude%20Oil%20Exports%20by%20Destination_EIA.csv?", skip = 2,
                          col_types = cols(Date = col_date(format = "%d/%m/%Y")))

#Leaving only country names in column names

names(raw_crude_oil)[-2] <- str_replace_all(names(raw_crude_oil)[-2],
                                            c("U.S. Exports to " = "",
                                              " of Crude Oil \\(Thousand Barrels\\)" = ""))

#Pivoting (switching) columns starting from 2 column to rows

crude_oil <- raw_crude_oil %>% 
  pivot_longer(cols = -Date,
               names_to = 'Destination country',
               values_to = 'Amount of Crude Oil (Thousand Barrels)')


#Filtering out values where Date is 'NA'

crude_oil <- crude_oil %>%
  drop_na(Date) %>%
  mutate(Date = as.Date(Date))

#Fixing date format, replacing 'na' values with 0

crude_oil$Date <- as.Date(crude_oil$Date)
crude_oil$`Amount of Crude Oil (Thousand Barrels)` <- replace_na(crude_oil$`Amount of Crude Oil (Thousand Barrels)`, 0)

#Setting up the tsibble

ts_crude_oil <- crude_oil %>% 
  as_tsibble(
    index = Date, 
    key = `Destination country`
  )

################################################################################

#Total Oil Products Exports by Destination

raw_oil_products <- read_csv("https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/Total%20Oil%20Products%20Exports%20by%20Destination_EIA.csv?",  skip = 2,
                             col_types = cols(Date = col_date(format = "%d/%m/%Y")))

names(raw_oil_products)[-2] <- str_replace_all(names(raw_oil_products)[-2],
                                           c("U.S. Exports to " = "",
                                             " of Total Petroleum Products \\(Thousand Barrels\\)" = ""))
oil_products <- raw_oil_products %>% 
  pivot_longer(cols = -Date,
               names_to = 'Destination country',
               values_to = 'Amount of total Petroleum Porducts (Thousand Barrels)')


oil_products$`Amount of total Petroleum Porducts (Thousand Barrels)` <- replace_na(oil_products$`Amount of total Petroleum Porducts (Thousand Barrels)`, 0)

oil_products <- oil_products %>% 
  drop_na(Date) %>% 
  mutate(Date = as.Date(Date))

ts_oil_products <- oil_products %>% 
  as_tsibble(
    index = Date, 
    key = `Destination country`
  )

################################################################################
#Reliance on Russian gas

raw_rel_rusoil <- read_csv("https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/Monthly_Reliance_on_Russian_Oil.csv?", skip = 1)

colnames(raw_rel_rusoil)

rel_rusoil <- raw_rel_rusoil %>% 
  pivot_longer(cols = -...1,
               names_to = 'Date',
               values_to = 'Total Oil Imports from Russia / Total Oil Imports (%)')

rel_rusoil <- rel_rusoil %>% 
  rename(Country = ...1) %>% 
  mutate(Date = lubridate::dmy(Date)) %>% 
  select("Date","Country","Total Oil Imports from Russia / Total Oil Imports (%)")


ts_rel_rusoil <- rel_rusoil %>% 
  as_tsibble(
    index = Date,
    key = Country
  )

ts_rel_rusoil %>% 
  filter(Country == "Lithuania") %>% 
  head(24) %>% 
  print(n =24)
