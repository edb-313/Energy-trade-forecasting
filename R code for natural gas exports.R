library(fpp3)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(dplyr)



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
library(ggplot2)

# Aggregate the data by year and sum the amount of natural gas exports
yearly_data <- ts_natgas %>% 
  group_by(year = format(Date, "%Y")) %>% 
  summarise(total_exports = sum(`Amount of Natural gas (MMcf)`))

yearly_data %>% autoplot(total_exports)+ labs(y="Total yearly exports",title= "Total Exports")
# Plot the time series using ggplot2
ggplot(yearly_data, aes(x = Date, y = total_exports)) +
  geom_line() +
  xlab("Year") +
  ylab("Total Exports (MMcf)") +
  ggtitle("Natural Gas Exports Over the Years")+scale_y_continuous(labels = scales::comma_format())
