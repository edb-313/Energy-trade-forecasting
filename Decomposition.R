#filtering out only needed data

total_quarterly <- ts_crude_oil %>%
  mutate(Quarter = yearquarter(Date)) %>%
  filter(`Destination country` == "U.S. Exports of Crude Oil (Thousand Barrels)") %>%
  filter(Quarter >= yearquarter("2000 Q1") & Quarter < yearquarter("2023 Q1")) %>%
  group_by(Quarter) %>%
  summarise(`Amount of Crude Oil` = sum(`Amount of Crude Oil (Thousand Barrels)`))

#creating a tsibble

ts_total_quarterly <-total_quarterly %>%
  as_tsibble(
  index = Quarter
)

#ploting the data

ts_total_quarterly %>% autoplot(`Amount of Crude Oil`)

#finding lambda for box cox transformation

ts_total_quarterly %>% features(`Amount of Crude Oil`, features = guerrero)

#box cox transformation

ts_total_quarterly %>% autoplot(box_cox(`Amount of Crude Oil`, 0.316)) +
  labs(y = "Box-Cox transformed exportamounts")

#decomposition

dcmp_total_quarterly <- ts_total_quarterly %>%
  model(stl = STL(`Amount of Crude Oil`))
components(dcmp_total_quarterly)

#ploting the components

components(dcmp_total_quarterly) %>% autoplot()

#trend component against all components

ts_total_quarterly %>%
  autoplot(`Amount of Crude Oil`, color='gray') +
  autolayer(components(dcmp_total_quarterly), trend, color='red') +
  xlab("Year") + ylab("Barels thousands") +
  ggtitle("U.S. Exports of Crude Oil (Thousand Barrels)")

#

components(dcmp_total_quarterly) %>% gg_subseries(season_)