library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)

# Create sub-tables for STP and SOP
ch_joined <-
  fread(
    'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/joined_sales_orders.csv'
  )
SOP_J <- ch_joined %>%
  filter(Type == "SOP")
SOP_j_net_value <- data.table(SOP_J[, 'Customer_ID'],
                              SOP_J[, 'Net_Value'])
SOP_j_net_value <- SOP_j_net_value %>%
  group_by(Customer_ID) %>%
  summarise(Total_netvalue = sum(Net_Value))
names(SOP_j_net_value)[-1] <-
  gsub("^", "SOP_", names(SOP_j_net_value)[-1])
setDT(SOP_j_net_value)
for (col in names(SOP_j_net_value)) {
  SOP_j_net_value[, (col) := replace(get(col), is.na(get(col)) |
                                             is.infinite(get(col)), 0)]
}
write.csv(x = SOP_j_net_value, file = 'path/SOP_NV_Variance.csv')











