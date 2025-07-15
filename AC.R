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

# #计算客户数量
# n_customers <- nrow(classification)
# n_customers
# 
# #计算有标签的客户数量
# n_customers_TRUE <- classification[Reseller != 'NA', .N]
# n_customers_TRUE
# 
# #计算reseller和non-reseller的数量
# n_subsum <- data.table(classification[, .N, by = 'Reseller'])
# n_nonreseller <- n_subsum[1, N]
# n_reseller <- n_subsum[3, N]
# n_unknow <- n_subsum[2, N]
# 
# #查看order的总数量
# n_orders <- nrow(customers)
# 
# #连接数据
# files <- list.files('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data')
# head(files)
# 
# #连接classification和customer
# m1 <- data.table(merge(classification, customers, by = "Customer_ID", all = TRUE))
# m1
# 
# #计算每个customer的order数量
# n_order <- m1[, .N, by = 'Customer_ID']
# n_order
# 
# #计算每个customer的两种不同类型的order数量
# n_order_STP <- m1[Type == "STP", .N, by = 'Customer_ID']
# n_order_STP
# 
# n_order_SOP <- m1[Type == "SOP", .N, by = 'Customer_ID']
# n_order_SOP
# 
# #计算每个customer中item order的数量
# n_order_item <- m1[Item_Position != 0, .N, by = 'Customer_ID']
# n_order_item
# 
# n_order_item <- m1[Item_Position != 1, .N, by = 'Customer_ID']
# n_order_item
# 
# 
# 
# #连接订单和cost center
# m2 <- data.table(merge(sales_orders, business_units, by = "Cost_Center", all = TRUE))
# m2
# 
# #计算Cost Center的数量
# 
# n_cost_center <- nrow(business_units)
# 
# #计算Material class的数量
# n_MATKL_class <- nrow(service_map)
# 
# #计算每种MATKL对应的订单数量
# n_order_MATKL <- data.table(sales_orders[, .N, by = 'Material_Class'])
# n_order_MATKL
# 
# #连接sales orders和sales orders header
# 
# nrow(sales_orders_header)
# 
# #ItemPositon不为0的sales order
# customers[Item_Position != 0]
# 
# #连接m1和sales order
# m3 <- data.table(merge(m1, sales_orders, by = "Sales_Order", all = TRUE))
# m3
# 
# #连接m3和sales order header
# m4 <- data.table(merge(m3, sales_orders_header, by = "Sales_Order", all = TRUE))
# m4
# 
# #计算每个Creator的order数量
# n_order_creator <- sales_orders_header[, .N, by = 'Creator']
# n_order_creator
# 
# #计算每个Document的order数量
# #n_order_doc <- sales_orders_header[, .N, by = 'Document_Type']
# #n_order_doc
# #dt_order_doc <- 
# 
# 
# #连接order和header
# left_order <- merge(sales_orders, sales_orders_header, by = 'Sales_Order', all.x = T)
# left_order
# #连接business units
# left_order2 <- merge(left_order, business_units, by = 'Cost_Center', all.x = T)
# left_order2
# #连接service map
# colnames(service_map) <- gsub("MATKL_service", "Material_Class", colnames(service_map))
# left_order3 <- merge(left_order2, service_map, by = 'Material_Class', all.x = T)
# left_order3
# #连接customer
# left_order4 <- merge(customers, classification, by = 'Customer_ID', all.x = T)
# left_order4
# #连接classification
# left_order5 <- merge(left_order3, left_order4, by = 'Sales_Order', all.x = T)
# left_order5

# #判断两个item值是否相等
# for(i in c(1:1000)){
#   if (identical(left_order5[i, Item_Position.x], left_order5[i, Item_Position.y]))
#   {
#     return()
#   }else{
#     left_order5$Item_Position.y[i] <- '0'
#   }
# }
# 
# left_order5
# 
# #检查item position
# 
# class(left_order5)
# sapply(left_order5, typeof)
# 
# classification[, Reseller == "NA"]

# #连接service map和sales order
# m3_6 <- merge(sales_orders, service_map, by = 'Material_Code', all.x = T)
# m3_6
# sales_orders[, unique(Material_Class)]
# #继续连接business units
# m3_6_5 <- merge(m3_6, business_units, by = 'Cost_Center', all.x = T)
# m3_6_5
# #继续连接header
# m_order <- merge(m3_6_5, sales_orders_header, by = 'Sales_Order', all.x = T)
# m_order
# 
# #连接customer和classification
# m_customer <- merge(customers, classification, by = "Customer_ID", all = TRUE)
# m_customer 
# 
# #连接customer和order，这个是最终版本
# m_end <- merge(m_order, m_customer, by = 'Sales_Order', all = TRUE)
# m_end
# colnames(m_end)
# colnames(m_order)
# m_end_1 <- m_end[, -c("Customer_ID", "Item_Position.y", "Type", "Reseller", "Test_set_id")]
# m_end_1
# colnames(m_end_1) <- gsub("Item_Position.x", "Item_Position", colnames(m_end_1))
# m_end_2 <- m_end_1[which(duplicated(m_end_1)), ]
# m_end_3 <- merge(m_end_2, m_customer, by = 'Sales_Order', all.x = TRUE)
# m_end_4 <- m_end[!duplicated(m_end_1), ]

#对m_end矩阵建立每一个Mcode的订购情况，显示netvaluey
# m_end_mcode <- dcast(m_end, ... ~Material_Code, value.var = "Net_Value.y")


#按照zoom的思路,Item_Position.x是原来order的Item_Position
# for(i in c(1:nrow(m_end))){
#   if(identical(m_end[i, Item_Position.x], m_end[i, Item_Position.y]))
#   {
#     next
#   }else{
#     if(identical(m_end[i, Item_Position.y], "0"))
#     {
#       next
#     }else{
#       m_end[i, Item_Position.y] <- '0'
#     }
#   }
# }

#多列合并
# m_m <- merge(m_customer, m_order, by=c("Sales_Order", "Item_Position"))
# 
# join <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/joined_sales_orders.csv')
# 

# #统计每个客户的订单数，基于m_end
# customer_order <- m_end[, .N, by = 'Customer_ID']
# customer_order <- merge(customer_order, classification, by = 'Customer_ID', all.x = T)
# 
# #统计每个用户的平均价格
# m_end$Net_Value.x<-as.numeric(m_end$Net_Value.x)
# customer_preis.x <- aggregate(m_end$Net_Value.x, by = list(m_end$Customer_ID), sum)
# customer_preis.y <- aggregate(m_end$Net_Value.y, by = list(m_end$Customer_ID), sum)
# customer_preis <- merge(customer_preis.x, customer_preis.y, by = 'Group.1', all.x = T)
# names(customer_preis) <- c("Customer_ID","Net_Value.x","Net_Value.y")
# customer_preis <- merge(customer_preis, customer_order, by = 'Customer_ID', all.x = T)

# #来自xinhang
# n_each_Material_Class <- sales_orders[, .N, by = 'Material_Class']
# n_Material_Class<- nrow(n_each_Material_Class)
# #n_sales_orders <- nrow(m)
# n_customers <- nrow(classification)
# 
# n_items <- matrix(0, nrow = n_customers, ncol = n_Material_Class)
# data.table(n_items)
# n_items <- merge(n_items, classification, all = TRUE)
# accumulated_price_items <- matrix(0, nrow = n_customers, ncol = n_Material_Class)
# n_orders <- matrix(0, nrow = n_customers, ncol = n_Material_Class)

# for(i in c(1:n_sale_orders)){
#   Customer <- m[i,'Customer ID']
#   Material_Class <- m[i,'Material_Class']
#   Customer_ID <- 
#   
#   N <- m[i,'Num_Items']
#   Price <- m[i,'Net_Value']
#   
#   n_items[Customer,Material_Class] <- n_items[Customer,Material_Class] + N
#   n_items[i, Customer_ID] <- $Customer_ID#从哪个表里调客户ID?
#   accumulated_price_items[Customer,Material_Class] <- accumulated_price_items[Customer,Material_Class] + Price
#   n_orders[Customer,Material_Class] <- n_orders[Customer,Material_Class] + 1 
#   
# }
# 
# 






#导入csv
classification <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/classification.csv')
customers <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/customers.csv')
sales_orders <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/sales_orders.csv')
sales_orders_header <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/sales_orders_header.csv')
business_units <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/business_units.csv')
service_map <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/service_map.csv')
service_customers <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/service_customers.csv')

#按照每个客户STP和SOP的数量计算
j_STOP_num <- data.table(customers[, 'Customer_ID'], customers[, 'Type'])
j_STOP_num <- j_STOP_num %>% group_by(Customer_ID, Type) %>% summarise(count = n()) 
setDT(j_STOP_num)
j_STOP_num <- dcast(j_STOP_num, ... ~Type, value.var = "count", na.rm = TRUE)
j_STOP_num <- j_STOP_num %>%
  replace_na(list('SOP' = 0, 'STP' = 0))

#建立STP和SOP的分表
ch_joined <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/joined_sales_orders.csv')
STP_J <- ch_joined %>% filter(Type == "STP")
SOP_J <- ch_joined %>% filter(Type == "SOP")

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

#按照订单数提取feature
STP_j_sales_order_num <- data.table(STP_J[, 'Customer_ID'],
                                    STP_J[, 'Sales_Order']
)
STP_j_sales_order_num <- STP_j_sales_order_num[, .N, by = 'Customer_ID']
colnames(STP_j_sales_order_num) <- c('Customer_ID', 'Order_Num')

SOP_j_sales_order_num <- data.table(SOP_J[, 'Customer_ID'],
                                    SOP_J[, 'Sales_Order']
)
SOP_j_sales_order_num <- SOP_j_sales_order_num[, .N, by = 'Customer_ID']
colnames(SOP_j_sales_order_num) <- c('Customer_ID', 'Order_Num')

#按照Net value提取feature
STP_j_net_value <- data.table(STP_J[, 'Customer_ID'],
                              STP_J[, 'Net_Value']
)
STP_NV_Variance <- STP_j_net_value %>% 
  group_by(Customer_ID) %>% 
  summarise(Variance = format(var(Net_Value), digits = 2))
STP_NV_Variance$Variance <- gsub("NA", -1, STP_NV_Variance$Variance) #替换NA为-1
STP_j_net_value <- STP_j_net_value %>%
  group_by(Customer_ID) %>%  # 以 customerID 为分组键
  summarise(Total_netvalue = sum(Net_Value)) # 计算每组的 netvalue 的和

SOP_j_net_value <- data.table(SOP_J[, 'Customer_ID'],
                              SOP_J[, 'Net_Value']
)
SOP_NV_Variance <- SOP_j_net_value %>% 
  group_by(Customer_ID) %>% 
  summarise(Variance = format(var(Net_Value), digits = 2))
SOP_NV_Variance$Variance <- gsub("NA", -1, SOP_NV_Variance$Variance) #替换NA为-1
SOP_j_net_value <- SOP_j_net_value %>%
  group_by(Customer_ID) %>%  # 以 customerID 为分组键
  summarise(Total_netvalue = sum(Net_Value)) # 计算每组的 netvalue 的和

#按照NUM ITEMS提取feature
STP_j_Num_Items <- data.table(STP_J[, 'Customer_ID'],
                              STP_J[, 'Num_Items']
)
STP_NI_Variance <- STP_j_Num_Items %>% 
  group_by(Customer_ID) %>% 
  summarise(Variance = format(var(Num_Items), digits = 2))
STP_NI_Variance$Variance <- gsub("NA", -1, STP_NI_Variance$Variance) #替换NA为-1
STP_j_Num_Items <- STP_j_Num_Items %>%
  group_by(Customer_ID) %>%  # 以 customerID 为分组键
  summarise(Total_netvalue = sum(Num_Items)) # 计算每组的 netvalue 的和

SOP_j_Num_Items <- data.table(SOP_J[, 'Customer_ID'],
                              SOP_J[, 'Num_Items']
)
SOP_NI_Variance <- SOP_j_Num_Items %>% 
  group_by(Customer_ID) %>% 
  summarise(Variance = format(var(Num_Items), digits = 2))
SOP_NI_Variance$Variance <- gsub("NA", -1, SOP_NI_Variance$Variance) #替换NA为-1
SOP_j_Num_Items <- SOP_j_Num_Items %>%
  group_by(Customer_ID) %>%  # 以 customerID 为分组键
  summarise(Total_netvalue = sum(Num_Items)) # 计算每组的 netvalue 的和

#按照Document_Type的种类和数量取feature
STP_j_Document_Type <- data.table(STP_J[, 'Customer_ID'],
                                  STP_J[, 'Document_Type']
)
STP_j_Document_Type <- STP_j_Document_Type %>%
  group_by(Customer_ID, Document_Type) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Document_Type, Quantity) # 使用spread()函数将产品数量分开
STP_j_Document_Type[is.na(STP_j_Document_Type)] <- -1

SOP_j_Document_Type <- data.table(SOP_J[, 'Customer_ID'],
                                  SOP_J[, 'Document_Type']
)
SOP_j_Document_Type <- SOP_j_Document_Type %>%
  group_by(Customer_ID, Document_Type) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Document_Type, Quantity) # 使用spread()函数将产品数量分开
SOP_j_Document_Type[is.na(SOP_j_Document_Type)] <- -1

#按照Delivery的种类和数量取feature
STP_j_Delivery <- data.table(STP_J[, 'Customer_ID'], STP_J[, 'Delivery'])
STP_j_Delivery <- STP_j_Delivery %>%
  group_by(Customer_ID, Delivery) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Delivery, Quantity) # 使用spread()函数将产品数量分开
STP_j_Delivery[is.na(STP_j_Delivery)] <- -1

SOP_j_Delivery <- data.table(SOP_J[, 'Customer_ID'], SOP_J[, 'Delivery'])
SOP_j_Delivery <- SOP_j_Delivery %>%
  group_by(Customer_ID, Delivery) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Delivery, Quantity) # 使用spread()函数将产品数量分开
SOP_j_Delivery[is.na(SOP_j_Delivery)] <- -1

#按照Business_Unit的种类和数量取feature
STP_j_Business_Unit <- data.table(STP_J[, 'Customer_ID'], STP_J[, 'Business_Unit'])
STP_j_Business_Unit <- STP_j_Business_Unit %>%
  group_by(Customer_ID, Business_Unit) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Business_Unit, Quantity) # 使用spread()函数将产品数量分开
STP_j_Business_Unit[is.na(STP_j_Business_Unit)] <- -1

SOP_j_Business_Unit <- data.table(SOP_J[, 'Customer_ID'], SOP_J[, 'Business_Unit'])
SOP_j_Business_Unit <- SOP_j_Business_Unit %>%
  group_by(Customer_ID, Business_Unit) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Business_Unit, Quantity) # 使用spread()函数将产品数量分开
SOP_j_Business_Unit[is.na(SOP_j_Business_Unit)] <- -1

#按照Material Class的种类和数量取feature
STP_j_Material_Class <- data.table(STP_J[, 'Customer_ID'], STP_J[, 'Material_Class'])
STP_j_Material_Class <- STP_j_Material_Class %>%
  group_by(Customer_ID, Material_Class) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Material_Class, Quantity) # 使用spread()函数将产品数量分开
STP_j_Material_Class[is.na(STP_j_Material_Class)] <- -1

SOP_j_Material_Class <- data.table(SOP_J[, 'Customer_ID'], SOP_J[, 'Material_Class'])
SOP_j_Material_Class <- SOP_j_Material_Class %>%
  group_by(Customer_ID, Material_Class) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Material_Class, Quantity) # 使用spread()函数将产品数量分开
SOP_j_Material_Class[is.na(SOP_j_Material_Class)] <- -1

S_Material_Class <- data.table(ch_joined[, 'Customer_ID'], ch_joined[, 'Material_Class'])
S_Material_Class <- S_Material_Class %>%
  group_by(Customer_ID, Material_Class) %>%  # 以客户和产品为分组键
  summarise(Quantity = n()) %>% # 计数函数 n()
  spread(Material_Class, Quantity) # 使用spread()函数将产品数量分开
S_Material_Class[is.na(S_Material_Class)] <- -1

#MERGE
STP_j_MERGE <- merge(STP_j_sales_organization, STP_j_sales_order_num, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_NV_Variance, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_net_value, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_NI_Variance, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Num_Items, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Document_Type, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Delivery, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Business_Unit, by = 'Customer_ID')
STP_j_MERGE <- merge(STP_j_MERGE, STP_j_Material_Class, by = 'Customer_ID')

SOP_j_MERGE <- merge(SOP_j_sales_organization, SOP_j_sales_order_num, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_NV_Variance, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_net_value, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_NI_Variance, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_Num_Items, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_Document_Type, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_Delivery, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_Business_Unit, by = 'Customer_ID')
SOP_j_MERGE <- merge(SOP_j_MERGE, SOP_j_Material_Class, by = 'Customer_ID')

Feature <- merge(SOP_j_MERGE, STP_j_MERGE, by = "Customer_ID", all = TRUE)
Feature <- merge(Feature, S_Material_Class, by = "Customer_ID", all = TRUE)
Feature[is.na(Feature)] <- -1

Feature <- merge(Feature, classification, by = "Customer_ID", all = TRUE)
write.csv(x = Feature,file = 'E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/Feature.csv')





