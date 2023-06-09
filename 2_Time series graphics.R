library(fpp3)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(dplyr)
# load the scales package
library(scales)


#Annual time series overview Natural Gas
total_exp_oecd %>% filter(`Export Type` == "Amount of Natural gas (BOE)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  autoplot(Total_Exports) +
  labs(y = "Total exports of Natural gas",
       title = "Total exports")


#Annual time series overview Crude Oil
total_exp_oecd %>% filter(`Export Type` == "Amount of Crude Oil (Thousand Barrels)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  autoplot(Total_Exports) +
  labs(y = "Total exports of Crude Oil",
       title = "Total exports")


#Annual time series overview Petroleum
total_exp_oecd %>% filter(`Export Type` == "Amount of total Petroleum Porducts (Thousand Barrels)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  autoplot(Total_Exports) +
  labs(y = "Total exports of Petroleum",
       title = "Total exports")

#quarterly tsibble

total_exports_OECD_quarter <- total_exp_oecd %>% 
  group_by(`Export Type`, year = format(Date, "%Y")) %>% 
  filter(year >= "2013") %>% 
  filter(year < "2023") %>% 
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%  
  group_by(`Export Type`, Quarter) %>% 
  summarise(total_exports = sum(`Amount`))%>%
  as_tsibble(index = Quarter, key = `Export Type`)


# create seasonal plot of total exports monthly of Natural gas
total_exp_oecd %>% filter(`Export Type` == "Amount of Natural gas (BOE)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  gg_season(Total_Exports) +
  ylab("Total exports  of Natural Gas") +
  ggtitle("Seasonal plot: US exports of natural gas to OECD") +
  scale_y_continuous(labels = comma_format())

# create seasonal plot of total exports monthly of Crude oil
total_exp_oecd %>% filter(`Export Type` == "Amount of Crude Oil (Thousand Barrels)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  gg_season(Total_Exports) +
  ylab("Total exports  of Crude oil") +
  ggtitle("Seasonal plot: US exports of Crude Oil to OECD") +
  scale_y_continuous(labels = comma_format())

# create seasonal plot of total exports monthly of Petroleum products
total_exp_oecd %>% filter(`Export Type` == "Amount of total Petroleum Porducts (Thousand Barrels)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  gg_season(Total_Exports) +
  ylab("Total exports  of Petroleum Products") +
  ggtitle("Seasonal plot: US exports of Petroleum to OECD") +
  scale_y_continuous(labels = comma_format())

#seasonal plot quarterly of natural gas
total_exports_OECD_quarter %>% filter(`Export Type`=="Amount of Natural gas (BOE)") %>% 
  index_by(Quarter) %>% 
  summarise(Total_Exports = sum(`total_exports`)) %>% 
  gg_season(Total_Exports) +
  ylab("Total exports of Natural Gas") +
  ggtitle("Seasonal plot: US exports of natural gas") +
  scale_y_continuous(labels = comma_format())

#seasonal plot quarterly of Crude Oil
total_exports_OECD_quarter %>% filter(`Export Type`=="Amount of Crude Oil (Thousand Barrels)") %>% 
  index_by(Quarter) %>% 
  summarise(Total_Exports = sum(`total_exports`)) %>% 
  gg_season(Total_Exports) +
  ylab("Amount of Crude Oil (Thousand Barrels)") +
  ggtitle("Seasonal plot: US exports of Crude Oil") +
  scale_y_continuous(labels = comma_format())

#seasonal plot quarterly of Petroleum
total_exports_OECD_quarter %>% filter(`Export Type`=="Amount of total Petroleum Porducts (Thousand Barrels)") %>% 
  index_by(Quarter) %>% 
  summarise(Total_Exports = sum(`total_exports`)) %>% 
  gg_season(Total_Exports) +
  ylab("Amount of total Petroleum Porducts (Thousand Barrels)") +
  ggtitle("Seasonal plot: US exports of Petroleum") +
  scale_y_continuous(labels = comma_format())



#Subseries plots Natural gas
total_exp_oecd %>% filter(`Export Type` == "Amount of Natural gas (BOE)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  gg_subseries(Total_Exports) + 
  ylab("Total exports") + 
  ggtitle("Subseries plot: US exports of natural gas")+
  scale_y_continuous(labels = comma_format())

#Subseries plots Crude oil
total_exp_oecd %>% filter(`Export Type` == "Amount of Crude Oil (Thousand Barrels)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  gg_subseries(Total_Exports) + 
  ylab("Total exports") + 
  ggtitle("Subseries plot: US exports of Crude Oil")+
  scale_y_continuous(labels = comma_format())

#Subseries plots Petroleum
total_exp_oecd %>% filter(`Export Type` == "Amount of total Petroleum Porducts (Thousand Barrels)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  gg_subseries(Total_Exports) + 
  ylab("Total exports") + 
  ggtitle("Subseries plot: US exports of Petroleum")+
  scale_y_continuous(labels = comma_format())

#subseries quarterly Natural gas
total_exports_OECD_quarter %>% filter(`Export Type`=="Amount of Natural gas (BOE)") %>% 
  gg_subseries(total_exports) + 
  ylab("Total exports") + 
  ggtitle("Subseries plot: US exports of natural gas")+
  scale_y_continuous(labels = comma_format())


#subseries quarterly Crude Oil
total_exports_OECD_quarter %>% filter(`Export Type`=="Amount of Crude Oil (Thousand Barrels)") %>% 
  gg_subseries(total_exports) + 
  ylab("Total exports") + 
  ggtitle("Subseries plot: US exports of Crude Oil")+
  scale_y_continuous(labels = comma_format())


#subseries quarterly Petroleum
total_exports_OECD_quarter %>% filter(`Export Type`=="Amount of total Petroleum Porducts (Thousand Barrels)") %>% 
  gg_subseries(total_exports) + 
  ylab("Total exports") + 
  ggtitle("Subseries plot: US exports of Petroleum")+
  scale_y_continuous(labels = comma_format())





#ACF plot without lag Natural Gas

total_exp_oecd %>% filter(`Export Type` == "Amount of Natural gas (BOE)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  ACF() %>%  autoplot()


#ACF plot without lag Crude Oil

total_exp_oecd %>% filter(`Export Type` == "Amount of Crude Oil (Thousand Barrels)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  ACF() %>%  autoplot()

#ACF plot without lag Petroleum

total_exp_oecd %>% filter(`Export Type` == "Amount of total Petroleum Porducts (Thousand Barrels)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  ACF() %>%  autoplot()
total_exp_oecd %>% filter(`Export Type` == "Amount of total Petroleum Porducts (Thousand Barrels)") %>% 
  index_by(Date) %>% 
  summarise(Total_Exports = sum(`Amount`)) %>% 
  acf()
