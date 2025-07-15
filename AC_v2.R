install.packages("data.table")
install.packages("magrittr")
install.packages("tidyr")
install.packages("dplyr")

library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)

#导入csv
classification <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/classification.csv')
customers <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/customers.csv')
sales_orders <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/sales_orders.csv')
sales_orders_header <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/sales_orders_header.csv')
business_units <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/business_units.csv')
service_map <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/service_map.csv')
service_customers <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/service_customers.csv')

#按照每个客户STP和SOP的数量计算
j_STOP_num <- 
  data.table(
    customers[, 'Customer_ID'], 
    customers[, 'Type']
    )
j_STOP_num <- 
  j_STOP_num %>% 
  group_by(
    Customer_ID, 
    Type) %>% 
  summarise(count = n()) 
setDT(j_STOP_num)
j_STOP_num <- 
  dcast(
    j_STOP_num,
    ... ~Type, 
    value.var = "count", 
    na.rm = TRUE
    )
j_STOP_num <- 
  j_STOP_num %>%
  replace_na(
    list('SOP' = 0, 'STP' = 0)
    )
names(j_STOP_num)[-1] <- 
  gsub("^", 
       "Num_", 
       names(j_STOP_num)[-1]
       )

#建立STP和SOP的分表
ch_joined <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/joined_sales_orders.csv')
STP_J <- 
  ch_joined %>% 
  filter(Type == "STP")
SOP_J <- 
  ch_joined %>% 
  filter(Type == "SOP")

#按照sales_organization的种类和数量取feature
STP_j_sales_organization <- data.table(STP_J[, 'Customer_ID'],
                                       STP_J[, 'Sales_Organization']
)
STP_j_sales_organization <- STP_j_sales_organization %>%
  group_by(Customer_ID, Sales_Organization) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Sales_Organization, Quantity) # 使用spread()函数将产品数量分开
colnames(STP_j_sales_organization) <- c('Customer_ID', 'NA', 'sales_organization_A', 'sales_organization_B')
STP_j_sales_organization <- STP_j_sales_organization %>%
  replace_na(list('NA' = 0, 'sales_organization_A' = 0, 'sales_organization_B' = 0))
colnames(STP_j_sales_organization)[colnames(STP_j_sales_organization) == "NA"] <- "sales_organization_NA"
names(STP_j_sales_organization)[-1] <- gsub("^", "STP_", names(STP_j_sales_organization)[-1])

SOP_j_sales_organization <- data.table(SOP_J[, 'Customer_ID'],
                                       SOP_J[, 'Sales_Organization']
)
SOP_j_sales_organization <- SOP_j_sales_organization %>%
  group_by(Customer_ID, Sales_Organization) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Sales_Organization, Quantity) # 使用spread()函数将产品数量分开
colnames(SOP_j_sales_organization) <- c('Customer_ID', 'NA', 'sales_organization_A', 'sales_organization_B')
SOP_j_sales_organization <- SOP_j_sales_organization %>%
  replace_na(list('NA' = 0, 'sales_organization_A' = 0, 'sales_organization_B' = 0))
colnames(SOP_j_sales_organization)[colnames(SOP_j_sales_organization) == "NA"] <- "sales_organization_NA"
names(SOP_j_sales_organization)[-1] <- gsub("^", "SOP_", names(SOP_j_sales_organization)[-1])

S_sales_organization <- data.table(ch_joined[, 'Customer_ID'],
                                   ch_joined[, 'Sales_Organization']
)
S_sales_organization <- S_sales_organization %>%
  group_by(Customer_ID, Sales_Organization) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Sales_Organization, Quantity) # 使用spread()函数将产品数量分开
colnames(S_sales_organization) <- c('Customer_ID', 'NA', 'sales_organization_A', 'sales_organization_B')
S_sales_organization <- S_sales_organization %>%
  replace_na(list('NA' = 0, 'sales_organization_A' = 0, 'sales_organization_B' = 0))
colnames(S_sales_organization)[colnames(S_sales_organization) == "NA"] <- "sales_organization_NA"
names(S_sales_organization)[-1] <- gsub("^", "S_", names(S_sales_organization)[-1])

#按照订单数提取feature
STP_j_sales_order_num <- data.table(STP_J[, 'Customer_ID'],
                                    STP_J[, 'Sales_Order']
)
STP_j_sales_order_num <- STP_j_sales_order_num[, .N, by = 'Customer_ID']
colnames(STP_j_sales_order_num) <- c('Customer_ID', 'Order_Num')
names(STP_j_sales_order_num)[-1] <- gsub("^", "STP_", names(STP_j_sales_order_num)[-1])

SOP_j_sales_order_num <- data.table(SOP_J[, 'Customer_ID'],
                                    SOP_J[, 'Sales_Order']
)
SOP_j_sales_order_num <- SOP_j_sales_order_num[, .N, by = 'Customer_ID']
colnames(SOP_j_sales_order_num) <- c('Customer_ID', 'Order_Num')
names(SOP_j_sales_order_num)[-1] <- gsub("^", "SOP_", names(SOP_j_sales_order_num)[-1])

S_sales_order_num <- data.table(ch_joined[, 'Customer_ID'],
                                ch_joined[, 'Sales_Order']
)
S_sales_order_num <- S_sales_order_num[, .N, by = 'Customer_ID']
colnames(S_sales_order_num) <- c('Customer_ID', 'Order_Num')
names(S_sales_order_num)[-1] <- gsub("^", "S_", names(S_sales_order_num)[-1])

#按照Net value提取feature
STP_j_net_value <- data.table(STP_J[, 'Customer_ID'],
                              STP_J[, 'Net_Value']
)
STP_NV_Variance <- STP_j_net_value %>% 
  group_by(Customer_ID) %>% 
  summarise(Variance = format(var(Net_Value), digits = 2))
STP_NV_Variance$Variance <- gsub("NA", -1, STP_NV_Variance$Variance) #替换NA为-1
names(STP_NV_Variance)[-1] <- gsub("^", "STP_NV_", names(STP_NV_Variance)[-1])
STP_j_net_value <- STP_j_net_value %>%
  group_by(Customer_ID) %>%  # 以 customerID 为分组键
  summarise(Total_netvalue = sum(Net_Value)) # 计算每组的 netvalue 的和
names(STP_j_net_value)[-1] <- gsub("^", "STP_", names(STP_j_net_value)[-1])

SOP_j_net_value <- data.table(SOP_J[, 'Customer_ID'],
                              SOP_J[, 'Net_Value']
)
SOP_NV_Variance <- SOP_j_net_value %>% 
  group_by(Customer_ID) %>% 
  summarise(Variance = format(var(Net_Value), digits = 2))
SOP_NV_Variance$Variance <- gsub("NA", -1, SOP_NV_Variance$Variance) #替换NA为-1
names(SOP_NV_Variance)[-1] <- gsub("^", "SOP_NV_", names(SOP_NV_Variance)[-1])
SOP_j_net_value <- SOP_j_net_value %>%
  group_by(Customer_ID) %>%  # 以 customerID 为分组键
  summarise(Total_netvalue = sum(Net_Value)) # 计算每组的 netvalue 的和
names(SOP_j_net_value)[-1] <- gsub("^", "SOP_", names(SOP_j_net_value)[-1])

S_net_value <- data.table(ch_joined[, 'Customer_ID'],
                          ch_joined[, 'Net_Value']
)
S_NV_Variance <- S_net_value %>% 
  group_by(Customer_ID) %>% 
  summarise(Variance = format(var(Net_Value), digits = 2))
S_NV_Variance$Variance <- gsub("NA", -1, S_NV_Variance$Variance) #替换NA为-1
names(S_NV_Variance)[-1] <- gsub("^", "S_NV_", names(S_NV_Variance)[-1])
S_net_value <- S_net_value %>%
  group_by(Customer_ID) %>%  # 以 customerID 为分组键
  summarise(Total_netvalue = sum(Net_Value)) # 计算每组的 netvalue 的和
names(S_net_value)[-1] <- gsub("^", "S_", names(S_net_value)[-1])

#按照NUM ITEMS提取feature
STP_j_Num_Items <- data.table(STP_J[, 'Customer_ID'],
                              STP_J[, 'Num_Items']
)
STP_NI_Variance <- STP_j_Num_Items %>% 
  group_by(Customer_ID) %>% 
  summarise(Variance = format(var(Num_Items), digits = 2))
STP_NI_Variance$Variance <- gsub("NA", -1, STP_NI_Variance$Variance) #替换NA为-1
names(STP_NI_Variance)[-1] <- gsub("^", "STP_NI_", names(STP_NI_Variance)[-1])
STP_j_Num_Items <- STP_j_Num_Items %>%
  group_by(Customer_ID) %>%  # 以 customerID 为分组键
  summarise(Total_Num_Items = sum(Num_Items)) # 计算每组的 netvalue 的和
names(STP_j_Num_Items)[-1] <- gsub("^", "STP_", names(STP_j_Num_Items)[-1])

SOP_j_Num_Items <- data.table(SOP_J[, 'Customer_ID'],
                              SOP_J[, 'Num_Items']
)
SOP_NI_Variance <- SOP_j_Num_Items %>% 
  group_by(Customer_ID) %>% 
  summarise(Variance = format(var(Num_Items), digits = 2))
SOP_NI_Variance$Variance <- 
  gsub("NA", -1, SOP_NI_Variance$Variance) #替换NA为-1
names(SOP_NI_Variance)[-1] <- 
  gsub("^", "STP_NI_", names(SOP_NI_Variance)[-1])
SOP_j_Num_Items <- SOP_j_Num_Items %>%
  group_by(Customer_ID) %>%  # 以 customerID 为分组键
  summarise(Total_Num_Items = sum(Num_Items)) # 计算每组的 netvalue 的和
names(SOP_j_Num_Items)[-1] <- gsub("^", "STP_", names(SOP_j_Num_Items)[-1])

S_Num_Items <- data.table(ch_joined[, 'Customer_ID'],
                          ch_joined[, 'Num_Items']
)
S_NI_Variance <- S_Num_Items %>% 
  group_by(Customer_ID) %>% 
  summarise(Variance = format(var(Num_Items), digits = 2))
S_NI_Variance$Variance <- 
  gsub("NA", -1, S_NI_Variance$Variance) #替换NA为-1
names(S_NI_Variance)[-1] <- 
  gsub("^", "S_NI_", names(S_NI_Variance)[-1])
S_Num_Items <- S_Num_Items %>%
  group_by(Customer_ID) %>%  # 以 customerID 为分组键
  summarise(Total_Num_Items = sum(Num_Items)) # 计算每组的 netvalue 的和
names(S_Num_Items)[-1] <- gsub("^", "S_", names(S_Num_Items)[-1])

#按照每个customer的每个material class的items之和来分类
STP_j_c_m_num_items <- 
  data.table(STP_J[, 'Customer_ID'],
             STP_J[, 'Material_Class'],
             STP_J[, 'Num_Items']
)
STP_j_c_m_num_items <- 
  STP_j_c_m_num_items %>% 
  group_by(Customer_ID, Material_Class) %>% 
  summarise(Sum_Num_Items = sum(Num_Items))
STP_j_c_m_num_items <- 
  STP_j_c_m_num_items %>% 
  pivot_wider(names_from = Material_Class, 
              values_from = Sum_Num_Items
)
STP_j_c_m_num_items[is.na(STP_j_c_m_num_items)] <- 0
names(STP_j_c_m_num_items)[-1] <- 
  gsub("^", "STP_c_m_num_items_", names(STP_j_c_m_num_items)[-1])

SOP_j_c_m_num_items <- data.table(SOP_J[, 'Customer_ID'],
                                  SOP_J[, 'Material_Class'],
                                  SOP_J[, 'Num_Items']
)
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

S_c_m_num_items <- 
  data.table(
    ch_joined[, 'Customer_ID'],
    ch_joined[, 'Material_Class'],
    ch_joined[, 'Num_Items']
    )
S_c_m_num_items <- 
  S_c_m_num_items %>% 
  group_by(
    Customer_ID, 
    Material_Class) %>% 
  summarise(
    Sum_num_items = sum(Num_Items)
    )
S_c_m_num_items <- 
  S_c_m_num_items %>% 
  pivot_wider(
    names_from = Material_Class, 
    values_from = Sum_num_items
    )
S_c_m_num_items[is.na(S_c_m_num_items)] <- 0
names(S_c_m_num_items)[-1] <- 
  gsub("^", "S_c_m_num_items_", names(S_c_m_num_items)[-1])

#按照每个customer的每个material class的value之和来分类
STP_j_c_m_Net_Value <- data.table(STP_J[, 'Customer_ID'],
                                  STP_J[, 'Material_Class'],
                                  STP_J[, 'Net_Value']
)
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
                                  SOP_J[, 'Net_Value']
)
SOP_j_c_m_Net_Value <- SOP_j_c_m_Net_Value %>% 
  group_by(Customer_ID, Material_Class) %>% 
  summarise(Sum_Net_Value = sum(Net_Value))
SOP_j_c_m_Net_Value <- SOP_j_c_m_Net_Value %>% 
  pivot_wider(names_from = Material_Class, values_from = Sum_Net_Value)
SOP_j_c_m_Net_Value[is.na(SOP_j_c_m_Net_Value)] <- 0
names(SOP_j_c_m_Net_Value)[-1] <- 
  gsub("^", "SOP_c_m_Net_Value_", names(SOP_j_c_m_Net_Value)[-1])

S_c_m_Net_Value <- data.table(ch_joined[, 'Customer_ID'],
                              ch_joined[, 'Material_Class'],
                              ch_joined[, 'Net_Value']
)
S_c_m_Net_Value <- S_c_m_Net_Value %>% 
  group_by(Customer_ID, Material_Class) %>% 
  summarise(Sum_Net_Value = sum(Net_Value))
S_c_m_Net_Value <- S_c_m_Net_Value %>% 
  pivot_wider(names_from = Material_Class, values_from = Sum_Net_Value)
S_c_m_Net_Value[is.na(S_c_m_Net_Value)] <- 0
names(S_c_m_Net_Value)[-1] <- 
  gsub("^", "S_c_m_Net_Value", names(S_c_m_Net_Value)[-1])

#计算net value的均值
STP_j_c_m_Net_Value_A <- cbind(STP_j_c_m_Net_Value[,1], 
                               STP_j_c_m_Net_Value[,-1] / STP_j_c_m_num_items[,-1])
STP_j_c_m_Net_Value_A[is.na(STP_j_c_m_Net_Value_A)] <- 0
names(STP_j_c_m_Net_Value_A)[-1] <- 
  gsub("^", "STP_c_m_Net_Value_A_", names(STP_j_c_m_Net_Value_A)[-1])

SOP_j_c_m_Net_Value_A <- cbind(SOP_j_c_m_Net_Value[,1], 
                               SOP_j_c_m_Net_Value[,-1] / SOP_j_c_m_num_items[,-1])
SOP_j_c_m_Net_Value_A[is.na(SOP_j_c_m_Net_Value_A)] <- 0
names(SOP_j_c_m_Net_Value_A)[-1] <- 
  gsub("^", "SOP_c_m_Net_Value_A_", names(SOP_j_c_m_Net_Value_A)[-1])

S_c_m_Net_Value_A <- cbind(S_c_m_Net_Value[,1], 
                           S_c_m_Net_Value[,-1] / S_c_m_num_items[,-1])
S_c_m_Net_Value_A[is.na(S_c_m_Net_Value_A)] <- 0
names(S_c_m_Net_Value_A)[-1] <- 
  gsub("^", "S_c_m_Net_Value_A_", names(S_c_m_Net_Value_A)[-1])

#按照Document_Type的种类和数量取feature
STP_j_Document_Type <- data.table(STP_J[, 'Customer_ID'],
                                  STP_J[, 'Document_Type']
)
STP_j_Document_Type <- STP_j_Document_Type %>%
  group_by(Customer_ID, Document_Type) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Document_Type, Quantity) # 使用spread()函数将产品数量分开
STP_j_Document_Type[is.na(STP_j_Document_Type)] <- -1
colnames(STP_j_Document_Type)[colnames(STP_j_Document_Type) == "V1"] <- "Document_Type_NA"
names(STP_j_Document_Type)[-1] <- 
  gsub("^", "STP_", names(STP_j_Document_Type)[-1])

SOP_j_Document_Type <- data.table(SOP_J[, 'Customer_ID'],
                                  SOP_J[, 'Document_Type']
)
SOP_j_Document_Type <- SOP_j_Document_Type %>%
  group_by(Customer_ID, Document_Type) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Document_Type, Quantity) # 使用spread()函数将产品数量分开
SOP_j_Document_Type[is.na(SOP_j_Document_Type)] <- -1
colnames(SOP_j_Document_Type)[colnames(SOP_j_Document_Type) == "V1"] <- "Document_Type_NA"
names(SOP_j_Document_Type)[-1] <- 
  gsub("^", "SOP_", names(SOP_j_Document_Type)[-1])

S_Document_Type <- data.table(ch_joined[, 'Customer_ID'],
                              ch_joined[, 'Document_Type']
)
S_Document_Type <- S_Document_Type %>%
  group_by(Customer_ID, Document_Type) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Document_Type, Quantity) # 使用spread()函数将产品数量分开
S_Document_Type[is.na(S_Document_Type)] <- -1
colnames(S_Document_Type)[colnames(S_Document_Type) == "V1"] <- "Document_Type_NA"
names(S_Document_Type)[-1] <- 
  gsub("^", "S_", names(S_Document_Type)[-1])


#按照Delivery的种类和数量取feature
STP_j_Delivery <- data.table(STP_J[, 'Customer_ID'], 
                             STP_J[, 'Delivery']
)
STP_j_Delivery <- STP_j_Delivery %>%
  group_by(Customer_ID, Delivery) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Delivery, Quantity) # 使用spread()函数将产品数量分开
STP_j_Delivery[is.na(STP_j_Delivery)] <- -1
names(STP_j_Delivery)[-1] <- 
  gsub("^", "STP_", names(STP_j_Delivery)[-1])

SOP_j_Delivery <- data.table(SOP_J[, 'Customer_ID'], 
                             SOP_J[, 'Delivery']
)
SOP_j_Delivery <- SOP_j_Delivery %>%
  group_by(Customer_ID, Delivery) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Delivery, Quantity) # 使用spread()函数将产品数量分开
SOP_j_Delivery[is.na(SOP_j_Delivery)] <- -1
names(SOP_j_Delivery)[-1] <- 
  gsub("^", "SOP_", names(SOP_j_Delivery)[-1])

S_Delivery <- data.table(ch_joined[, 'Customer_ID'], 
                         ch_joined[, 'Delivery']
)
S_Delivery <- S_Delivery %>%
  group_by(Customer_ID, Delivery) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Delivery, Quantity) # 使用spread()函数将产品数量分开
S_Delivery[is.na(S_Delivery)] <- -1
names(S_Delivery)[-1] <- 
  gsub("^", "S_", names(S_Delivery)[-1])

#按照Business_Unit的种类和数量取feature
STP_j_Business_Unit <- data.table(STP_J[, 'Customer_ID'], 
                                  STP_J[, 'Business_Unit']
)
STP_j_Business_Unit <- STP_j_Business_Unit %>%
  group_by(Customer_ID, Business_Unit) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Business_Unit, Quantity) # 使用spread()函数将产品数量分开
STP_j_Business_Unit[is.na(STP_j_Business_Unit)] <- -1
names(STP_j_Business_Unit)[-1] <- 
  gsub("^", "STP_", names(STP_j_Business_Unit)[-1])

SOP_j_Business_Unit <- data.table(SOP_J[, 'Customer_ID'], 
                                  SOP_J[, 'Business_Unit']
)
SOP_j_Business_Unit <- SOP_j_Business_Unit %>%
  group_by(Customer_ID, Business_Unit) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Business_Unit, Quantity) # 使用spread()函数将产品数量分开
SOP_j_Business_Unit[is.na(SOP_j_Business_Unit)] <- -1
names(SOP_j_Business_Unit)[-1] <- 
  gsub("^", "SOP_", names(SOP_j_Business_Unit)[-1])

S_Business_Unit <- data.table(ch_joined[, 'Customer_ID'], 
                              ch_joined[, 'Business_Unit']
)
S_Business_Unit <- S_Business_Unit %>%
  group_by(Customer_ID, Business_Unit) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Business_Unit, Quantity) # 使用spread()函数将产品数量分开
S_Business_Unit[is.na(S_Business_Unit)] <- -1
names(S_Business_Unit)[-1] <- 
  gsub("^", "S_", names(S_Business_Unit)[-1])

#按照Material Class的种类和数量取feature
STP_j_Material_Class <- data.table(STP_J[, 'Customer_ID'], 
                                   STP_J[, 'Material_Class']
)
STP_j_Material_Class <- STP_j_Material_Class %>%
  group_by(Customer_ID, Material_Class) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Material_Class, Quantity) # 使用spread()函数将产品数量分开
STP_j_Material_Class[is.na(STP_j_Material_Class)] <- -1
names(STP_j_Material_Class)[-1] <- 
  gsub("^", "STP_", names(STP_j_Material_Class)[-1])

SOP_j_Material_Class <- data.table(SOP_J[, 'Customer_ID'], 
                                   SOP_J[, 'Material_Class']
)
SOP_j_Material_Class <- SOP_j_Material_Class %>%
  group_by(Customer_ID, Material_Class) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Material_Class, Quantity) # 使用spread()函数将产品数量分开
SOP_j_Material_Class[is.na(SOP_j_Material_Class)] <- -1
names(SOP_j_Material_Class)[-1] <- 
  gsub("^", "SOP_", names(SOP_j_Material_Class)[-1])

S_Material_Class <- data.table(ch_joined[, 'Customer_ID'], 
                               ch_joined[, 'Material_Class']
)
S_Material_Class <- S_Material_Class %>%
  group_by(Customer_ID, Material_Class) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Material_Class, Quantity) # 使用spread()函数将产品数量分开
S_Material_Class[is.na(S_Material_Class)] <- -1
names(S_Material_Class)[-1] <- 
  gsub("^", "S_", names(S_Material_Class)[-1])

#MERGE
STP_j_MERGE <- merge(STP_j_Business_Unit, STP_j_c_m_Net_Value, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_c_m_Net_Value_A, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_c_m_num_items, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Delivery, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Document_Type, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Material_Class, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_net_value, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Num_Items, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_sales_order_num, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_sales_organization, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_NI_Variance, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_NV_Variance, by = 'Customer_ID')

SOP_j_MERGE <- merge(SOP_j_Business_Unit, SOP_j_c_m_Net_Value, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_c_m_Net_Value_A, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_c_m_num_items, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_Delivery, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_Document_Type, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_Material_Class, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_net_value, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_Num_Items, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_sales_order_num, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_sales_organization, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_NI_Variance, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_NV_Variance, by = 'Customer_ID')

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

Feature <- merge(STP_j_MERGE, SOP_j_MERGE, by = 'Customer_ID', all = TRUE)
Feature <- merge(Feature, S_MERGE, by = "Customer_ID", all = TRUE)
Feature[is.na(Feature)] <- -1

Feature <- merge(Feature, classification, by = "Customer_ID", all = TRUE)
write.csv(x = Feature,file = 'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/OUTPUT/Feature.csv')





