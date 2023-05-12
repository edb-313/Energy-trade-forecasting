library(fpp3)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
################################################################################

#Total Crude Oil Exports by Destination


raw_crude_oil <- read_csv("https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/csv/Total%20Crude%20Oil%20Exports%20by%20Destination_EIA.csv", skip = 2,)


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
  mutate(Date = yearmonth(Date)) %>% 
  as_tsibble(
    index = Date, 
    key = `Destination country`
  )



#OECD Europe Countries vector

oecd_europe <- c("Austria", "Belgium", "Czech Republic", "Denmark", "Estonia",
                 "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
                 "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                 "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic",
                 "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
                 "United Kingdom")

#filtering out only relevant values

oecd_crude_oil <- ts_crude_oil %>%
  filter(`Destination country` %in% oecd_europe) %>% 
  filter(Date >= as.Date("2010-01-15"), Date <= as.Date("2023-01-15")) 

oecd_crude_oil
  
oecd_crude_oil_agg <- oecd_crude_oil %>%
  index_by(Date) %>%
  summarize_at(vars(`Amount of Crude Oil (Thousand Barrels)`), sum) %>%
  ungroup()


################################################################################

#Total Oil Products Exports by Destination

raw_oil_prod <- read_csv("https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/csv/Total%20Oil%20Products%20Exports%20by%20Destination_EIA.csv",  skip = 2,)

names(raw_oil_prod)[-2] <- str_replace_all(names(raw_oil_prod)[-2],
                                           c("U.S. Exports to " = "",
                                             " of Total Petroleum Products \\(Thousand Barrels\\)" = ""))
oil_prod <- raw_oil_prod %>% 
  pivot_longer(cols = -Date,
               names_to = 'Destination country',
               values_to = 'Amount of total Petroleum Porducts (Thousand Barrels)')


oil_prod$`Amount of total Petroleum Porducts (Thousand Barrels)` <- replace_na(oil_prod$`Amount of total Petroleum Porducts (Thousand Barrels)`, 0)

oil_prod <- oil_prod %>% 
  drop_na(Date) %>% 
  mutate(Date = as.Date(Date))

ts_oil_prod <- oil_prod %>%
  mutate(Date = yearmonth(Date)) %>% 
  as_tsibble(
    index = Date, 
    key = `Destination country`
  )

oecd_oil_prod <- ts_oil_prod %>% 
  filter(`Destination country` %in% oecd_europe) %>% 
  filter(Date >= as.Date("2010-01-15"), Date <= as.Date("2023-01-15"))

oecd_oil_prod

oecd_oil_prod_agg <- oecd_oil_prod %>%
  index_by(Date) %>%
  summarize_at(vars(`Amount of total Petroleum Porducts (Thousand Barrels)`), sum) %>%
  ungroup()

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

oecd_relrus <- ts_rel_rusoil %>%
  filter(Country %in% oecd_europe)

oecd_relrus %>% slice_sample(n=10)

################################################################################
#Total Natural Gas Exports by Destination

raw_nat_gas <- read.csv('https://raw.githubusercontent.com/edb-313/Energy-trade-forecasting/main/Data/csv/Natural%20Gas%20Exports%20by%20Country_EIA.csv',header=TRUE , check.names=FALSE, skip = 2)

# drop columns 2-3, 6-8, 54 and 57-72
raw_nat_gas <- subset(raw_nat_gas, select = -c(2:3, 6:8,54, 57:72))
raw_nat_gas[is.na(raw_nat_gas)] <- 0

#cimbining exports to the same countries

nat_gas <- raw_nat_gas %>%
  mutate(Mexico = `U.S. Natural Gas Pipeline Exports to Mexico (MMcf)` + `Liquefied U.S. Natural Gas Exports by Truck to Mexico (Million Cubic Feet)`,
         Canada = `U.S. Natural Gas Pipeline Exports to Canada (MMcf)` + `Liquefied U.S. Natural Gas Exports by Truck to Canada (Million Cubic Feet)`)

#removing duplicate columns

nat_gas <- nat_gas[, !(names(nat_gas) %in% c('U.S. Natural Gas Pipeline Exports to Mexico (MMcf)',
                                             'Liquefied U.S. Natural Gas Exports by Truck to Mexico (Million Cubic Feet)',
                                             'U.S. Natural Gas Pipeline Exports to Canada (MMcf)',
                                             'Liquefied U.S. Natural Gas Exports by Truck to Canada (Million Cubic Feet)'))]

#cleaning column names

colnames(nat_gas) <- gsub(pattern = ".*(to\\s)", "", colnames(nat_gas))
colnames(nat_gas) <- gsub("\\(.*?\\)", "", colnames(nat_gas))

#pivoting data
gastemp <- nat_gas %>% 
  pivot_longer(cols = -Date,
               names_to = 'Destination country',
               values_to = 'Amount of Natural gas (MMcf)')

#converting to tsibbles

gastemp

#removing n/as in date column, fixing date formatting, fixing values (romoving space in the end)
library(stringr) #for triming the strings

gastemp <- gastemp %>% 
  drop_na(Date) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(`Destination country` = str_trim(`Destination country`))


#summing duplicates 

gastemp <- gastemp %>%
  group_by(Date, `Destination country`) %>%
  summarize_at(vars(`Amount of Natural gas (MMcf)`), sum) %>%
  ungroup() %>% 
  drop_na(Date)

#creating a tsibble

ts_nat_gas <- gastemp %>%
  mutate(Date = yearmonth(Date)) %>% 
  as_tsibble(
    index = Date,
    key = `Destination country`
  )

#filtering only relevant data

oecd_nat_gas <- ts_nat_gas %>%
  filter(`Destination country` %in% oecd_europe) %>% 
  filter(Date >= as.Date("2010-01-15"), Date <= as.Date("2023-01-15"))



oecd_nat_gas_agg <- oecd_nat_gas %>%
  index_by(Date) %>%
  summarize_at(vars(`Amount of Natural gas (MMcf)`), sum) %>%
  ungroup()

oecd_nat_gas_agg <- oecd_nat_gas_agg %>% 
  mutate(`Amount of Natural gas (BOE)` = `Amount of Natural gas (MMcf)` * 1000 / 6000 )

oecd_nat_gas_agg <- oecd_nat_gas_agg %>% 
  select(Date, `Amount of Natural gas (BOE)`)

oecd_nat_gas_agg

################################################################################

oecd_merged <- left_join(oecd_nat_gas_agg, oecd_oil_prod_agg, by = "Date") %>%
  left_join(oecd_crude_oil_agg, by = "Date")

total_exp_oecd <- oecd_merged %>%
  pivot_longer(cols = -Date, names_to = "Export Type", values_to = "Amount")

total_exp_oecd

total_exp_oecd %>% ggplot(
 aes(Date,Amount,color = `Export Type`)) +
   geom_line()+
   labs(x = "Date", y = "Amount")+
  theme(legend.position = "bottom")

total_exp_oecd

lambda1 <- total_exp_oecd %>% 
  filter(`Export Type` == "Amount of Natural gas (BOE)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(Amount)) %>% 
  features(Total_Exports, features = guerrero) %>% 
  pull(lambda_guerrero)

lambda1

nat_gas_bx <-total_exp_oecd %>% 
  filter(`Export Type` == "Amount of Natural gas (BOE)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(Amount)) %>% 
  mutate(Total_Exports = box_cox(Total_Exports + 1, lambda1))

crude_oil_bx

