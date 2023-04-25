library(fpp3)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(OECD)
library(tsibble)


################################################################################


#Total Crude Oil Exports by Destination



raw_crude_oil <- read_csv("https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/csv/Total%20Crude%20Oil%20Exports%20by%20Destination_EIA.csv?", skip = 2,
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

raw_oil_products <- read_csv("https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/csv/Total%20Oil%20Products%20Exports%20by%20Destination_EIA.csv",  skip = 2,
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

raw_rel_rusoil <- read_csv("https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/csv/Monthly_Reliance_on_Russian_Oil.csv", skip = 1)

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
    key = Country)

################################################################################
#Total Natural Gas Exports by Destination

Nat_gas <- read.csv('https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/csv/Natural%20Gas%20Exports%20by%20Country_EIA.csv', header=TRUE , check.names=FALSE,skip = 2)

# drop columns 2-3, 6-8, 54 and 57-72
Nat_gas <- subset(Nat_gas, select = -c(2:3, 6:8,54, 57:72))
Nat_gas[is.na(Nat_gas)] <- 0
#cimbining exports to the same countries
Nat_gas <- Nat_gas %>%
  mutate(Mexico = `U.S. Natural Gas Pipeline Exports to Mexico (MMcf)` + `Liquefied U.S. Natural Gas Exports by Truck to Mexico (Million Cubic Feet)`,
         Canada = `U.S. Natural Gas Pipeline Exports to Canada (MMcf)` + `Liquefied U.S. Natural Gas Exports by Truck to Canada (Million Cubic Feet)`)
#removing duplicate columns
Nat_gas <- Nat_gas[, !(names(Nat_gas) %in% c('U.S. Natural Gas Pipeline Exports to Mexico (MMcf)',
                                             'Liquefied U.S. Natural Gas Exports by Truck to Mexico (Million Cubic Feet)',
                                             'U.S. Natural Gas Pipeline Exports to Canada (MMcf)',
                                             'Liquefied U.S. Natural Gas Exports by Truck to Canada (Million Cubic Feet)'))]

#cleaning column names
colnames(Nat_gas) <- gsub(pattern = ".*(to\\s)", "", colnames(Nat_gas))
colnames(Nat_gas) <- gsub("\\(.*?\\)", "", colnames(Nat_gas))

#pivoting data
Gastemp <- Nat_gas %>% 
  pivot_longer(cols = -Date,
               names_to = 'Destination country',
               values_to = 'Amount of Natural gas (MMcf)')

Gastemp$Date <- as.Date(Gastemp$Date, format = "%d/%m/%Y")

Gastemp <- Gastemp[complete.cases(Gastemp$Date), ]

#converting to tsibbles
ts_natgas <- Gastemp %>% 
  as_tsibble(
    index = Date, 
    key = `Destination country`
  )

