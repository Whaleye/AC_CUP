# install.packages("data.table")
# install.packages("magrittr")
# install.packages("tidyr")
# install.packages("dplyr")

library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)

# load csv
classification <-
  fread(
    'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/classification.csv'
  )
customers <-
  fread(
    'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/customers.csv'
  )
# sales_orders <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/sales_orders.csv')
# sales_orders_header <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/sales_orders_header.csv')
# business_units <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/business_units.csv')
# service_map <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/service_map.csv')
# service_customers <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/service_customers.csv')
data.table(classification)

# Calculated according to the quantity of each customer's STP and SOP
j_STOP_num <-
  data.table(customers[, 'Customer_ID'],
             customers[, 'Type'])
j_STOP_num <-
  j_STOP_num %>%
  group_by(Customer_ID,
           Type) %>%
  summarise(count = n())
setDT(j_STOP_num)
j_STOP_num <-
  dcast(j_STOP_num,
        ... ~ Type,
        value.var = "count",
        na.rm = TRUE)
j_STOP_num <-
  j_STOP_num %>%
  replace_na(list('SOP' = 0, 'STP' = 0))
names(j_STOP_num)[-1] <-
  gsub("^",
       "Num_",
       names(j_STOP_num)[-1])



# Create sub-tables for STP and SOP
ch_joined <-
  fread(
    'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/joined_sales_orders.csv'
  )
STP_J <-
  ch_joined %>%
  filter(Type == "STP")
SOP_J <-
  ch_joined %>%
  filter(Type == "SOP")

# Take feature according to the type and quantity of sales_organization
STP_j_sales_organization <- data.table(STP_J[, 'Customer_ID'],
                                       STP_J[, 'Sales_Organization'])
STP_j_sales_organization <- STP_j_sales_organization %>%
  group_by(Customer_ID, Sales_Organization) %>%
  summarise(Quantity = n()) %>%
  spread(Sales_Organization, Quantity)
colnames(STP_j_sales_organization) <-
  c('Customer_ID',
    'NA',
    'sales_organization_A',
    'sales_organization_B')
STP_j_sales_organization <- STP_j_sales_organization %>%
  replace_na(list(
    'NA' = 0,
    'sales_organization_A' = 0,
    'sales_organization_B' = 0
  ))
colnames(STP_j_sales_organization)[colnames(STP_j_sales_organization) == "NA"] <-
  "sales_organization_NA"
names(STP_j_sales_organization)[-1] <-
  gsub("^", "STP_", names(STP_j_sales_organization)[-1])

SOP_j_sales_organization <- data.table(SOP_J[, 'Customer_ID'],
                                       SOP_J[, 'Sales_Organization'])
SOP_j_sales_organization <- SOP_j_sales_organization %>%
  group_by(Customer_ID, Sales_Organization) %>%
  summarise(Quantity = n()) %>%
  spread(Sales_Organization, Quantity)
colnames(SOP_j_sales_organization) <-
  c('Customer_ID',
    'NA',
    'sales_organization_A',
    'sales_organization_B')
SOP_j_sales_organization <- SOP_j_sales_organization %>%
  replace_na(list(
    'NA' = 0,
    'sales_organization_A' = 0,
    'sales_organization_B' = 0
  ))
colnames(SOP_j_sales_organization)[colnames(SOP_j_sales_organization) == "NA"] <-
  "sales_organization_NA"
names(SOP_j_sales_organization)[-1] <-
  gsub("^", "SOP_", names(SOP_j_sales_organization)[-1])

S_sales_organization <- data.table(ch_joined[, 'Customer_ID'],
                                   ch_joined[, 'Sales_Organization'])
S_sales_organization <- S_sales_organization %>%
  group_by(Customer_ID, Sales_Organization) %>%
  summarise(Quantity = n()) %>%
  spread(Sales_Organization, Quantity)
colnames(S_sales_organization) <-
  c('Customer_ID',
    'NA',
    'sales_organization_A',
    'sales_organization_B')
S_sales_organization <- S_sales_organization %>%
  replace_na(list(
    'NA' = 0,
    'sales_organization_A' = 0,
    'sales_organization_B' = 0
  ))
colnames(S_sales_organization)[colnames(S_sales_organization) == "NA"] <-
  "sales_organization_NA"
names(S_sales_organization)[-1] <-
  gsub("^", "S_", names(S_sales_organization)[-1])

#Extract features according to the number of orders
STP_j_sales_order_num <- data.table(STP_J[, 'Customer_ID'],
                                    STP_J[, 'Sales_Order'])
STP_j_sales_order_num <-
  STP_j_sales_order_num[, .N, by = 'Customer_ID']
colnames(STP_j_sales_order_num) <- c('Customer_ID', 'Order_Num')
names(STP_j_sales_order_num)[-1] <-
  gsub("^", "STP_", names(STP_j_sales_order_num)[-1])

SOP_j_sales_order_num <- data.table(SOP_J[, 'Customer_ID'],
                                    SOP_J[, 'Sales_Order'])
SOP_j_sales_order_num <-
  SOP_j_sales_order_num[, .N, by = 'Customer_ID']
colnames(SOP_j_sales_order_num) <- c('Customer_ID', 'Order_Num')
names(SOP_j_sales_order_num)[-1] <-
  gsub("^", "SOP_", names(SOP_j_sales_order_num)[-1])

S_sales_order_num <- data.table(ch_joined[, 'Customer_ID'],
                                ch_joined[, 'Sales_Order'])
S_sales_order_num <- S_sales_order_num[, .N, by = 'Customer_ID']
colnames(S_sales_order_num) <- c('Customer_ID', 'Order_Num')
names(S_sales_order_num)[-1] <-
  gsub("^", "S_", names(S_sales_order_num)[-1])

#Extract feature according to Net value
STP_j_net_value <- data.table(STP_J[, 'Customer_ID'],
                              STP_J[, 'Net_Value'])
STP_NV_Variance <- STP_j_net_value %>%
  group_by(Customer_ID) %>%
  summarise(Variance = format(var(Net_Value), digits = 2))
STP_NV_Variance$Variance <-
  gsub("NA",-1, STP_NV_Variance$Variance)
names(STP_NV_Variance)[-1] <-
  gsub("^", "STP_NV_", names(STP_NV_Variance)[-1])
STP_j_net_value <- STP_j_net_value %>%
  group_by(Customer_ID) %>%
  summarise(Total_netvalue = sum(Net_Value))
names(STP_j_net_value)[-1] <-
  gsub("^", "STP_", names(STP_j_net_value)[-1])

SOP_j_net_value <- data.table(SOP_J[, 'Customer_ID'],
                              SOP_J[, 'Net_Value'])
SOP_NV_Variance <- SOP_j_net_value %>%
  group_by(Customer_ID) %>%
  summarise(Variance = format(var(Net_Value), digits = 2))
SOP_NV_Variance$Variance <-
  gsub("NA",-1, SOP_NV_Variance$Variance)
names(SOP_NV_Variance)[-1] <-
  gsub("^", "SOP_NV_", names(SOP_NV_Variance)[-1])
SOP_j_net_value <- SOP_j_net_value %>%
  group_by(Customer_ID) %>%
  summarise(Total_netvalue = sum(Net_Value))
names(SOP_j_net_value)[-1] <-
  gsub("^", "SOP_", names(SOP_j_net_value)[-1])

S_net_value <- data.table(ch_joined[, 'Customer_ID'],
                          ch_joined[, 'Net_Value'])
S_NV_Variance <- S_net_value %>%
  group_by(Customer_ID) %>%
  summarise(Variance = format(var(Net_Value), digits = 2))
S_NV_Variance$Variance <- gsub("NA",-1, S_NV_Variance$Variance)
names(S_NV_Variance)[-1] <-
  gsub("^", "S_NV_", names(S_NV_Variance)[-1])
S_net_value <- S_net_value %>%
  group_by(Customer_ID) %>%
  summarise(Total_netvalue = sum(Net_Value))
names(S_net_value)[-1] <- gsub("^", "S_", names(S_net_value)[-1])

#Extract feature according to NUM ITEMS
STP_j_Num_Items <- data.table(STP_J[, 'Customer_ID'],
                              STP_J[, 'Num_Items'])
STP_NI_Variance <- STP_j_Num_Items %>%
  group_by(Customer_ID) %>%
  summarise(Variance = format(var(Num_Items), digits = 2))
STP_NI_Variance$Variance <-
  gsub("NA",-1, STP_NI_Variance$Variance)
names(STP_NI_Variance)[-1] <-
  gsub("^", "STP_NI_", names(STP_NI_Variance)[-1])
STP_j_Num_Items <- STP_j_Num_Items %>%
  group_by(Customer_ID) %>%
  summarise(Total_Num_Items = sum(Num_Items))
names(STP_j_Num_Items)[-1] <-
  gsub("^", "STP_", names(STP_j_Num_Items)[-1])

SOP_j_Num_Items <- data.table(SOP_J[, 'Customer_ID'],
                              SOP_J[, 'Num_Items'])
SOP_NI_Variance <- SOP_j_Num_Items %>%
  group_by(Customer_ID) %>%
  summarise(Variance = format(var(Num_Items), digits = 2))
SOP_NI_Variance$Variance <-
  gsub("NA",-1, SOP_NI_Variance$Variance)
names(SOP_NI_Variance)[-1] <-
  gsub("^", "STP_NI_", names(SOP_NI_Variance)[-1])
SOP_j_Num_Items <- SOP_j_Num_Items %>%
  group_by(Customer_ID) %>%
  summarise(Total_Num_Items = sum(Num_Items))
names(SOP_j_Num_Items)[-1] <-
  gsub("^", "STP_", names(SOP_j_Num_Items)[-1])

S_Num_Items <- data.table(ch_joined[, 'Customer_ID'],
                          ch_joined[, 'Num_Items'])
S_NI_Variance <- S_Num_Items %>%
  group_by(Customer_ID) %>%
  summarise(Variance = format(var(Num_Items), digits = 2))
S_NI_Variance$Variance <-
  gsub("NA",-1, S_NI_Variance$Variance)
names(S_NI_Variance)[-1] <-
  gsub("^", "S_NI_", names(S_NI_Variance)[-1])
S_Num_Items <- S_Num_Items %>%
  group_by(Customer_ID) %>%
  summarise(Total_Num_Items = sum(Num_Items))
names(S_Num_Items)[-1] <- gsub("^", "S_", names(S_Num_Items)[-1])

#Classify according to the sum of items of each material class of each customer
STP_j_c_m_num_items <-
  data.table(STP_J[, 'Customer_ID'],
             STP_J[, 'Material_Class'],
             STP_J[, 'Num_Items'])
STP_j_c_m_num_items <-
  STP_j_c_m_num_items %>%
  group_by(Customer_ID, Material_Class) %>%
  summarise(Sum_Num_Items = sum(Num_Items))
STP_j_c_m_num_items <-
  STP_j_c_m_num_items %>%
  pivot_wider(names_from = Material_Class,
              values_from = Sum_Num_Items)
STP_j_c_m_num_items[is.na(STP_j_c_m_num_items)] <- 0
names(STP_j_c_m_num_items)[-1] <-
  gsub("^", "STP_c_m_num_items_", names(STP_j_c_m_num_items)[-1])


SOP_j_c_m_num_items <- data.table(SOP_J[, 'Customer_ID'],
                                  SOP_J[, 'Material_Class'],
                                  SOP_J[, 'Num_Items'])
SOP_j_c_m_num_items <- SOP_j_c_m_num_items %>%
  group_by(Customer_ID,
           Material_Class) %>%
  summarise(Sum_Num_Items = sum(Num_Items))
SOP_j_c_m_num_items <-
  SOP_j_c_m_num_items %>%
  pivot_wider(names_from = Material_Class,
              values_from = Sum_Num_Items)
SOP_j_c_m_num_items[is.na(SOP_j_c_m_num_items)] <- 0
names(SOP_j_c_m_num_items)[-1] <-
  gsub("^", "SOP_c_m_num_items_", names(SOP_j_c_m_num_items)[-1])
SOP_j_c_m_num_items <- data.table(SOP_j_c_m_num_items)
for (col in names(SOP_j_c_m_num_items)) {
  SOP_j_c_m_num_items[, (col) := replace(get(col), is.na(get(col)) |
                                           is.infinite(get(col)), 0)]
}
SOP_j_c_m_num_items <-
  merge(SOP_j_c_m_num_items, classification, all.x = TRUE)
SOP_j_c_m_num_items <-
  SOP_j_c_m_num_items[match(classification$Customer_ID, SOP_j_c_m_num_items$Customer_ID),]
SOP_j_c_m_num_items <-
  SOP_j_c_m_num_items[!is.na(SOP_j_c_m_num_items$Reseller),]
for (col in names(SOP_j_c_m_num_items)) {
  SOP_j_c_m_num_items[, (col) := replace(get(col), is.na(get(col)) |
                                           is.infinite(get(col)), 0)]
}
write.csv(x = SOP_j_c_m_num_items, file = 'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/OUTPUT/SOP_j_c_m_num_items.csv')

S_c_m_num_items <-
  data.table(ch_joined[, 'Customer_ID'],
             ch_joined[, 'Material_Class'],
             ch_joined[, 'Num_Items'])
S_c_m_num_items <-
  S_c_m_num_items %>%
  group_by(Customer_ID,
           Material_Class) %>%
  summarise(Sum_num_items = sum(Num_Items))
S_c_m_num_items <-
  S_c_m_num_items %>%
  pivot_wider(names_from = Material_Class,
              values_from = Sum_num_items)
S_c_m_num_items[is.na(S_c_m_num_items)] <- 0
names(S_c_m_num_items)[-1] <-
  gsub("^", "S_c_m_num_items_", names(S_c_m_num_items)[-1])

#Classify according to the sum of the value of each material class of each customer
STP_j_c_m_Net_Value <- data.table(STP_J[, 'Customer_ID'],
                                  STP_J[, 'Material_Class'],
                                  STP_J[, 'Net_Value'])
STP_j_c_m_Net_Value <- STP_j_c_m_Net_Value %>%
  group_by(Customer_ID, Material_Class) %>%
  summarise(Sum_Net_Value = sum(Net_Value))
STP_j_c_m_Net_Value <- STP_j_c_m_Net_Value %>%
  pivot_wider(names_from = Material_Class, values_from = Sum_Net_Value)
STP_j_c_m_Net_Value[is.na(STP_j_c_m_Net_Value)] <- 0
names(STP_j_c_m_Net_Value)[-1] <-
  gsub("^", "STP_c_m_Net_Value_", names(STP_j_c_m_Net_Value)[-1])

SOP_j_c_m_Net_Value <- data.table(SOP_J[, 'Customer_ID'],
                                  SOP_J[, 'Material_Class'],
                                  SOP_J[, 'Net_Value'])
SOP_j_c_m_Net_Value <- SOP_j_c_m_Net_Value %>%
  group_by(Customer_ID, Material_Class) %>%
  summarise(Sum_Net_Value = sum(Net_Value))
SOP_j_c_m_Net_Value <- SOP_j_c_m_Net_Value %>%
  pivot_wider(names_from = Material_Class, values_from = Sum_Net_Value)
SOP_j_c_m_Net_Value[is.na(SOP_j_c_m_Net_Value)] <- 0
names(SOP_j_c_m_Net_Value)[-1] <-
  gsub("^", "SOP_c_m_Net_Value_", names(SOP_j_c_m_Net_Value)[-1])
SOP_j_c_m_Net_Value <- data.table(SOP_j_c_m_Net_Value)
for (col in names(SOP_j_c_m_Net_Value)) {
  SOP_j_c_m_Net_Value[, (col) := replace(get(col), is.na(get(col)) |
                                           is.infinite(get(col)), 0)]
}
SOP_j_c_m_Net_Value <-
  merge(SOP_j_c_m_Net_Value, classification, all.x = TRUE)
SOP_j_c_m_Net_Value <-
  SOP_j_c_m_Net_Value[match(classification$Customer_ID, SOP_j_c_m_Net_Value$Customer_ID),]
SOP_j_c_m_Net_Value <-
  SOP_j_c_m_Net_Value[!is.na(SOP_j_c_m_Net_Value$Reseller),]
for (col in names(SOP_j_c_m_Net_Value)) {
  SOP_j_c_m_Net_Value[, (col) := replace(get(col), is.na(get(col)) |
                                           is.infinite(get(col)), 0)]
}
write.csv(x = SOP_j_c_m_Net_Value, file = 'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/OUTPUT/SOP_j_c_m_Net_Value.csv')

S_c_m_Net_Value <- data.table(ch_joined[, 'Customer_ID'],
                              ch_joined[, 'Material_Class'],
                              ch_joined[, 'Net_Value'])
S_c_m_Net_Value <- S_c_m_Net_Value %>%
  group_by(Customer_ID, Material_Class) %>%
  summarise(Sum_Net_Value = sum(Net_Value))
S_c_m_Net_Value <- S_c_m_Net_Value %>%
  pivot_wider(names_from = Material_Class, values_from = Sum_Net_Value)
S_c_m_Net_Value[is.na(S_c_m_Net_Value)] <- 0
names(S_c_m_Net_Value)[-1] <-
  gsub("^", "S_c_m_Net_Value", names(S_c_m_Net_Value)[-1])

#Classify according to the sum of the BU_A of each material class of each customer
# STP_j_c_m_Net_Value <- data.table(STP_J[, 'Customer_ID'],
#                                   STP_J[, 'Material_Class'],
#                                   STP_J[, 'Net_Value']
# )
# STP_j_c_m_Net_Value <- STP_j_c_m_Net_Value %>%
#   group_by(Customer_ID, Material_Class) %>%
#   summarise(Sum_Net_Value = sum(Net_Value))
# STP_j_c_m_Net_Value <- STP_j_c_m_Net_Value %>%
#   pivot_wider(names_from = Material_Class, values_from = Sum_Net_Value)
# STP_j_c_m_Net_Value[is.na(STP_j_c_m_Net_Value)] <- 0
# names(STP_j_c_m_Net_Value)[-1] <-
#   gsub("^", "STP_c_m_Net_Value_", names(STP_j_c_m_Net_Value)[-1])
#
# SOP_j_c_m_Net_Value <- data.table(SOP_J[, 'Customer_ID'],
#                                   SOP_J[, 'Material_Class'],
#                                   SOP_J[, 'Net_Value']
# )
# SOP_j_c_m_Net_Value <- SOP_j_c_m_Net_Value %>%
#   group_by(Customer_ID, Material_Class) %>%
#   summarise(Sum_Net_Value = sum(Net_Value))
# SOP_j_c_m_Net_Value <- SOP_j_c_m_Net_Value %>%
#   pivot_wider(names_from = Material_Class, values_from = Sum_Net_Value)
# SOP_j_c_m_Net_Value[is.na(SOP_j_c_m_Net_Value)] <- 0
# names(SOP_j_c_m_Net_Value)[-1] <-
#   gsub("^", "SOP_c_m_Net_Value_", names(SOP_j_c_m_Net_Value)[-1])
SOP_j_c_m_BU_A <- data.table(SOP_J[, 'Customer_ID'],
                             SOP_J[, 'Material_Class'],
                             SOP_J[, 'Business_Unit'])
SOP_j_c_m_BU_A <- SOP_j_c_m_BU_A %>%
  group_by(Customer_ID, Material_Class) %>%
  summarize(NUM.BU_A = sum(Business_Unit == "BU_A"))
SOP_j_c_m_BU_A <- SOP_j_c_m_BU_A %>%
  pivot_wider(names_from = Material_Class, values_from = NUM.BU_A)
SOP_j_c_m_BU_A[is.na(SOP_j_c_m_BU_A)] <- 0
SOP_j_c_m_BU_A <- data.table(SOP_j_c_m_BU_A)
for (col in names(SOP_j_c_m_BU_A)) {
  SOP_j_c_m_BU_A[, (col) := replace(get(col), is.na(get(col)) |
                                      is.infinite(get(col)), 0)]
}
names(SOP_j_c_m_BU_A)[-1] <-
  gsub("^", "S_c_m_BU_A_", names(SOP_j_c_m_BU_A)[-1])
SOP_j_c_m_BU_A <-
  merge(SOP_j_c_m_BU_A, classification, all.x = TRUE)
SOP_j_c_m_BU_A <-
  SOP_j_c_m_BU_A[match(classification$Customer_ID, SOP_j_c_m_BU_A$Customer_ID),]
SOP_j_c_m_BU_A <- SOP_j_c_m_BU_A[!is.na(SOP_j_c_m_BU_A$Reseller),]
for (col in names(SOP_j_c_m_BU_A)) {
  SOP_j_c_m_BU_A[, (col) := replace(get(col), is.na(get(col)) |
                                      is.infinite(get(col)), 0)]
}
write.csv(x = SOP_j_c_m_BU_A, file = 'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/OUTPUT/SOP_j_c_m_BU_A.csv')

S_c_m_BU_A <- data.table(ch_joined[, 'Customer_ID'],
                         ch_joined[, 'Material_Class'],
                         ch_joined[, 'Business_Unit'])
S_c_m_BU_A <- S_c_m_BU_A %>%
  group_by(Customer_ID, Material_Class) %>%
  summarize(NUM.BU_A = sum(Business_Unit == "BU_A"))
S_c_m_BU_A <- S_c_m_BU_A %>%
  pivot_wider(names_from = Material_Class, values_from = NUM.BU_A)
S_c_m_BU_A[is.na(S_c_m_BU_A)] <- 0
S_c_m_BU_A <- data.table(S_c_m_BU_A)
for (col in names(S_c_m_BU_A)) {
  S_c_m_BU_A[, (col) := replace(get(col), is.na(get(col)) |
                                  is.infinite(get(col)), 0)]
}
names(S_c_m_BU_A)[-1] <-
  gsub("^", "S_c_m_BU_A_", names(S_c_m_BU_A)[-1])


#Classify according to the sum of the BU_B of each material class of each customer
# STP_j_c_m_Net_Value <- data.table(STP_J[, 'Customer_ID'],
#                                   STP_J[, 'Material_Class'],
#                                   STP_J[, 'Net_Value']
# )
# STP_j_c_m_Net_Value <- STP_j_c_m_Net_Value %>%
#   group_by(Customer_ID, Material_Class) %>%
#   summarise(Sum_Net_Value = sum(Net_Value))
# STP_j_c_m_Net_Value <- STP_j_c_m_Net_Value %>%
#   pivot_wider(names_from = Material_Class, values_from = Sum_Net_Value)
# STP_j_c_m_Net_Value[is.na(STP_j_c_m_Net_Value)] <- 0
# names(STP_j_c_m_Net_Value)[-1] <-
#   gsub("^", "STP_c_m_Net_Value_", names(STP_j_c_m_Net_Value)[-1])
#
SOP_j_c_m_BU_B <- data.table(SOP_J[, 'Customer_ID'],
                             SOP_J[, 'Material_Class'],
                             SOP_J[, 'Business_Unit'])
SOP_j_c_m_BU_B <- SOP_j_c_m_BU_B %>%
  group_by(Customer_ID, Material_Class) %>%
  summarize(NUM.BU_B = sum(Business_Unit == "BU_B"))
SOP_j_c_m_BU_B <- SOP_j_c_m_BU_B %>%
  pivot_wider(names_from = Material_Class, values_from = NUM.BU_B)
SOP_j_c_m_BU_B[is.na(SOP_j_c_m_BU_B)] <- 0
SOP_j_c_m_BU_B <- data.table(SOP_j_c_m_BU_B)
for (col in names(SOP_j_c_m_BU_B)) {
  SOP_j_c_m_BU_B[, (col) := replace(get(col), is.na(get(col)) |
                                      is.infinite(get(col)), 0)]
}
names(SOP_j_c_m_BU_B)[-1] <-
  gsub("^", "S_c_m_BU_B_", names(SOP_j_c_m_BU_B)[-1])
SOP_j_c_m_BU_B <-
  merge(SOP_j_c_m_BU_B, classification, all.x = TRUE)
SOP_j_c_m_BU_B <-
  SOP_j_c_m_BU_B[match(classification$Customer_ID, SOP_j_c_m_BU_B$Customer_ID),]
SOP_j_c_m_BU_B <- SOP_j_c_m_BU_B[!is.na(SOP_j_c_m_BU_B$Reseller),]
for (col in names(SOP_j_c_m_BU_B)) {
  SOP_j_c_m_BU_B[, (col) := replace(get(col), is.na(get(col)) |
                                      is.infinite(get(col)), 0)]
}
write.csv(x = SOP_j_c_m_BU_B, file = 'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/OUTPUT/SOP_j_c_m_BU_B.csv')

S_c_m_BU_B <- data.table(ch_joined[, 'Customer_ID'],
                         ch_joined[, 'Material_Class'],
                         ch_joined[, 'Business_Unit'])
S_c_m_BU_B <- S_c_m_BU_B %>%
  group_by(Customer_ID, Material_Class) %>%
  summarize(NUM.BU_B = sum(Business_Unit == "BU_B"))
S_c_m_BU_B <- S_c_m_BU_B %>%
  pivot_wider(names_from = Material_Class, values_from = NUM.BU_B)
S_c_m_BU_B[is.na(S_c_m_BU_B)] <- 0
S_c_m_BU_B <- data.table(S_c_m_BU_B)
for (col in names(S_c_m_BU_B)) {
  S_c_m_BU_B[, (col) := replace(get(col), is.na(get(col)) |
                                  is.infinite(get(col)), 0)]
}
names(S_c_m_BU_B)[-1] <-
  gsub("^", "S_c_m_BU_B_", names(S_c_m_BU_B)[-1])

#Classify according to the sum of the BU_C of each material class of each customer
# STP_j_c_m_Net_Value <- data.table(STP_J[, 'Customer_ID'],
#                                   STP_J[, 'Material_Class'],
#                                   STP_J[, 'Net_Value']
# )
# STP_j_c_m_Net_Value <- STP_j_c_m_Net_Value %>%
#   group_by(Customer_ID, Material_Class) %>%
#   summarise(Sum_Net_Value = sum(Net_Value))
# STP_j_c_m_Net_Value <- STP_j_c_m_Net_Value %>%
#   pivot_wider(names_from = Material_Class, values_from = Sum_Net_Value)
# STP_j_c_m_Net_Value[is.na(STP_j_c_m_Net_Value)] <- 0
# names(STP_j_c_m_Net_Value)[-1] <-
#   gsub("^", "STP_c_m_Net_Value_", names(STP_j_c_m_Net_Value)[-1])
SOP_j_c_m_BU_C <- data.table(SOP_J[, 'Customer_ID'],
                             SOP_J[, 'Material_Class'],
                             SOP_J[, 'Business_Unit'])
SOP_j_c_m_BU_C <- SOP_j_c_m_BU_C %>%
  group_by(Customer_ID, Material_Class) %>%
  summarize(NUM.BU_C = sum(Business_Unit == "BU_C"))
SOP_j_c_m_BU_C <- SOP_j_c_m_BU_C %>%
  pivot_wider(names_from = Material_Class, values_from = NUM.BU_C)
SOP_j_c_m_BU_C[is.na(SOP_j_c_m_BU_C)] <- 0
SOP_j_c_m_BU_C <- data.table(SOP_j_c_m_BU_C)
for (col in names(SOP_j_c_m_BU_C)) {
  SOP_j_c_m_BU_C[, (col) := replace(get(col), is.na(get(col)) |
                                      is.infinite(get(col)), 0)]
}
names(SOP_j_c_m_BU_C)[-1] <-
  gsub("^", "S_c_m_BU_C_", names(SOP_j_c_m_BU_C)[-1])
SOP_j_c_m_BU_C <-
  merge(SOP_j_c_m_BU_C, classification, all.x = TRUE)
SOP_j_c_m_BU_C <-
  SOP_j_c_m_BU_C[match(classification$Customer_ID, SOP_j_c_m_BU_C$Customer_ID),]
SOP_j_c_m_BU_C <- SOP_j_c_m_BU_C[!is.na(SOP_j_c_m_BU_C$Reseller),]
for (col in names(SOP_j_c_m_BU_C)) {
  SOP_j_c_m_BU_C[, (col) := replace(get(col), is.na(get(col)) |
                                      is.infinite(get(col)), 0)]
}
write.csv(x = SOP_j_c_m_BU_C, file = 'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/OUTPUT/SOP_j_c_m_BU_C.csv')

S_c_m_BU_C <- data.table(ch_joined[, 'Customer_ID'],
                         ch_joined[, 'Material_Class'],
                         ch_joined[, 'Business_Unit'])
S_c_m_BU_C <- S_c_m_BU_C %>%
  group_by(Customer_ID, Material_Class) %>%
  summarize(NUM.BU_C = sum(Business_Unit == "BU_C"))
S_c_m_BU_C <- S_c_m_BU_C %>%
  pivot_wider(names_from = Material_Class, values_from = NUM.BU_C)
S_c_m_BU_C[is.na(S_c_m_BU_C)] <- 0
S_c_m_BU_C <- data.table(S_c_m_BU_C)
for (col in names(S_c_m_BU_C)) {
  S_c_m_BU_C[, (col) := replace(get(col), is.na(get(col)) |
                                  is.infinite(get(col)), 0)]
}
names(S_c_m_BU_C)[-1] <-
  gsub("^", "S_c_m_BU_C_", names(S_c_m_BU_C)[-1])

#Calculate the mean of the net value
STP_j_c_m_Net_Value_A <- cbind(STP_j_c_m_Net_Value[, 1],
                               STP_j_c_m_Net_Value[, -1] / STP_j_c_m_num_items[, -1])
STP_j_c_m_Net_Value_A[is.na(STP_j_c_m_Net_Value_A)] <- 0
names(STP_j_c_m_Net_Value_A)[-1] <-
  gsub("^", "STP_c_m_Net_Value_A_", names(STP_j_c_m_Net_Value_A)[-1])

SOP_j_c_m_Net_Value_A <- cbind(SOP_j_c_m_Net_Value[, 1],
                               SOP_j_c_m_Net_Value[, -1] / SOP_j_c_m_num_items[, -1])
SOP_j_c_m_Net_Value_A[is.na(SOP_j_c_m_Net_Value_A)] <- 0
names(SOP_j_c_m_Net_Value_A)[-1] <-
  gsub("^", "SOP_c_m_Net_Value_A_", names(SOP_j_c_m_Net_Value_A)[-1])

S_c_m_Net_Value_A <- cbind(S_c_m_Net_Value[, 1],
                           S_c_m_Net_Value[, -1] / S_c_m_num_items[, -1])
S_c_m_Net_Value_A[is.na(S_c_m_Net_Value_A)] <- 0
names(S_c_m_Net_Value_A)[-1] <-
  gsub("^", "S_c_m_Net_Value_A_", names(S_c_m_Net_Value_A)[-1])

#Take feature according to the type and quantity of Document_Type
STP_j_Document_Type <- data.table(STP_J[, 'Customer_ID'],
                                  STP_J[, 'Document_Type'])
STP_j_Document_Type <- STP_j_Document_Type %>%
  group_by(Customer_ID, Document_Type) %>%
  summarise(Quantity = n()) %>%
  spread(Document_Type, Quantity)
STP_j_Document_Type[is.na(STP_j_Document_Type)] <- -1
colnames(STP_j_Document_Type)[colnames(STP_j_Document_Type) == "V1"] <-
  "Document_Type_NA"
names(STP_j_Document_Type)[-1] <-
  gsub("^", "STP_", names(STP_j_Document_Type)[-1])

SOP_j_Document_Type <- data.table(SOP_J[, 'Customer_ID'],
                                  SOP_J[, 'Document_Type'])
SOP_j_Document_Type <- SOP_j_Document_Type %>%
  group_by(Customer_ID, Document_Type) %>%
  summarise(Quantity = n()) %>%
  spread(Document_Type, Quantity)
SOP_j_Document_Type[is.na(SOP_j_Document_Type)] <- -1
colnames(SOP_j_Document_Type)[colnames(SOP_j_Document_Type) == "V1"] <-
  "Document_Type_NA"
names(SOP_j_Document_Type)[-1] <-
  gsub("^", "SOP_", names(SOP_j_Document_Type)[-1])

S_Document_Type <- data.table(ch_joined[, 'Customer_ID'],
                              ch_joined[, 'Document_Type'])
S_Document_Type <- S_Document_Type %>%
  group_by(Customer_ID, Document_Type) %>%
  summarise(Quantity = n()) %>%
  spread(Document_Type, Quantity)
S_Document_Type[is.na(S_Document_Type)] <- -1
colnames(S_Document_Type)[colnames(S_Document_Type) == "V1"] <-
  "Document_Type_NA"
names(S_Document_Type)[-1] <-
  gsub("^", "S_", names(S_Document_Type)[-1])


#Select features according to the type and quantity of Delivery
STP_j_Delivery <- data.table(STP_J[, 'Customer_ID'],
                             STP_J[, 'Delivery'])
STP_j_Delivery <- STP_j_Delivery %>%
  group_by(Customer_ID, Delivery) %>%
  summarise(Quantity = n()) %>%
  spread(Delivery, Quantity)
STP_j_Delivery[is.na(STP_j_Delivery)] <- -1
names(STP_j_Delivery)[-1] <-
  gsub("^", "STP_", names(STP_j_Delivery)[-1])

SOP_j_Delivery <- data.table(SOP_J[, 'Customer_ID'],
                             SOP_J[, 'Delivery'])
SOP_j_Delivery <- SOP_j_Delivery %>%
  group_by(Customer_ID, Delivery) %>%
  summarise(Quantity = n()) %>%
  spread(Delivery, Quantity)
SOP_j_Delivery[is.na(SOP_j_Delivery)] <- -1
names(SOP_j_Delivery)[-1] <-
  gsub("^", "SOP_", names(SOP_j_Delivery)[-1])

S_Delivery <- data.table(ch_joined[, 'Customer_ID'],
                         ch_joined[, 'Delivery'])
S_Delivery <- S_Delivery %>%
  group_by(Customer_ID, Delivery) %>%
  summarise(Quantity = n()) %>%
  spread(Delivery, Quantity)
S_Delivery[is.na(S_Delivery)] <- -1
names(S_Delivery)[-1] <-
  gsub("^", "S_", names(S_Delivery)[-1])

#Select feature according to the type and quantity of Business_Unit
STP_j_Business_Unit <- data.table(STP_J[, 'Customer_ID'],
                                  STP_J[, 'Business_Unit'])
STP_j_Business_Unit <- STP_j_Business_Unit %>%
  group_by(Customer_ID, Business_Unit) %>%
  summarise(Quantity = n()) %>%
  spread(Business_Unit, Quantity)
STP_j_Business_Unit[is.na(STP_j_Business_Unit)] <- -1
names(STP_j_Business_Unit)[-1] <-
  gsub("^", "STP_", names(STP_j_Business_Unit)[-1])

SOP_j_Business_Unit <- data.table(SOP_J[, 'Customer_ID'],
                                  SOP_J[, 'Business_Unit'])
SOP_j_Business_Unit <- SOP_j_Business_Unit %>%
  group_by(Customer_ID, Business_Unit) %>%
  summarise(Quantity = n()) %>%
  spread(Business_Unit, Quantity)
SOP_j_Business_Unit[is.na(SOP_j_Business_Unit)] <- -1
names(SOP_j_Business_Unit)[-1] <-
  gsub("^", "SOP_", names(SOP_j_Business_Unit)[-1])

S_Business_Unit <- data.table(ch_joined[, 'Customer_ID'],
                              ch_joined[, 'Business_Unit'])
S_Business_Unit <- S_Business_Unit %>%
  group_by(Customer_ID, Business_Unit) %>%
  summarise(Quantity = n()) %>%
  spread(Business_Unit, Quantity)
S_Business_Unit[is.na(S_Business_Unit)] <- -1
names(S_Business_Unit)[-1] <-
  gsub("^", "S_", names(S_Business_Unit)[-1])

#Select features according to the type and quantity of Material Class
STP_j_Material_Class <- data.table(STP_J[, 'Customer_ID'],
                                   STP_J[, 'Material_Class'])
STP_j_Material_Class <- STP_j_Material_Class %>%
  group_by(Customer_ID, Material_Class) %>%
  summarise(Quantity = n()) %>%
  spread(Material_Class, Quantity)
STP_j_Material_Class[is.na(STP_j_Material_Class)] <- -1
names(STP_j_Material_Class)[-1] <-
  gsub("^", "STP_", names(STP_j_Material_Class)[-1])

SOP_j_Material_Class <- data.table(SOP_J[, 'Customer_ID'],
                                   SOP_J[, 'Material_Class'])
SOP_j_Material_Class <- SOP_j_Material_Class %>%
  group_by(Customer_ID, Material_Class) %>%
  summarise(Quantity = n()) %>%
  spread(Material_Class, Quantity)
SOP_j_Material_Class[is.na(SOP_j_Material_Class)] <- -1
names(SOP_j_Material_Class)[-1] <-
  gsub("^", "SOP_", names(SOP_j_Material_Class)[-1])

S_Material_Class <- data.table(ch_joined[, 'Customer_ID'],
                               ch_joined[, 'Material_Class'])
S_Material_Class <- S_Material_Class %>%
  group_by(Customer_ID, Material_Class) %>%
  summarise(Quantity = n()) %>%
  spread(Material_Class, Quantity)
S_Material_Class[is.na(S_Material_Class)] <- -1
names(S_Material_Class)[-1] <-
  gsub("^", "S_", names(S_Material_Class)[-1])

#MERGE
# STP_j_MERGE <- merge(STP_j_Business_Unit, STP_j_c_m_Net_Value, by = 'Customer_ID')
# STP_j_MERGE <- merge(STP_j_MERGE, STP_j_c_m_Net_Value_A, by = 'Customer_ID')
# STP_j_MERGE <- merge(STP_j_MERGE, STP_j_c_m_num_items, by = 'Customer_ID')
# STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Delivery, by = 'Customer_ID')
# STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Document_Type, by = 'Customer_ID')
# STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Material_Class, by = 'Customer_ID')
# STP_j_MERGE <- merge(STP_j_MERGE, STP_j_net_value, by = 'Customer_ID')
# STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Num_Items, by = 'Customer_ID')
# STP_j_MERGE <- merge(STP_j_MERGE, STP_j_sales_order_num, by = 'Customer_ID')
# STP_j_MERGE <- merge(STP_j_MERGE, STP_j_sales_organization, by = 'Customer_ID')
# STP_j_MERGE <- merge(STP_j_MERGE, STP_NI_Variance, by = 'Customer_ID')
# STP_j_MERGE <- merge(STP_j_MERGE, STP_NV_Variance, by = 'Customer_ID')

SOP_j_MERGE <-
  merge(SOP_j_Business_Unit, SOP_j_c_m_Net_Value, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_j_c_m_Net_Value_A, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_j_c_m_num_items, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_j_Delivery, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_j_Document_Type, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_j_Material_Class, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_j_net_value, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_j_Num_Items, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_j_sales_order_num, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_j_sales_organization, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_NI_Variance, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_NV_Variance, by = 'Customer_ID')
SOP_j_MERGE <-
  merge(SOP_j_MERGE, SOP_NV_Variance, by = 'Customer_ID')




S_MERGE <- merge(j_STOP_num, S_Business_Unit, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_c_m_Net_Value, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_c_m_Net_Value_A, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_c_m_num_items, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_Delivery, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_Document_Type, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_Num_Items, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_Material_Class, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_net_value, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_NI_Variance, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_sales_order_num, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_NV_Variance, by = 'Customer_ID')
S_MERGE <- merge(S_MERGE, S_sales_organization, by = 'Customer_ID')

Feature <-
  merge(S_MERGE, SOP_j_MERGE, by = 'Customer_ID', all = TRUE)
#Feature <- merge(Feature, S_MERGE, by = "Customer_ID", all = TRUE)

Feature <-
  merge(Feature, classification, by = "Customer_ID", all = TRUE)
write.csv(x = Feature, file = 'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/OUTPUT/Feature.csv')
