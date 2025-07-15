install.packages("data.table")
install.packages("magrittr")
install.packages("tidyr")
install.packages("dplyr")

library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)

#导入csv
# classification <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/classification.csv')
# customers <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/customers.csv')
# sales_orders <- fread('C:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/sales_orders.csv')
# sales_orders_header <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/sales_orders_header.csv')
# business_units <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/business_units.csv')
# service_map <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/service_map.csv')
# 
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
# 
# # #判断两个item值是否相等
# # for(i in c(1:1000)){
# #   if (identical(left_order5[i, Item_Position.x], left_order5[i, Item_Position.y]))
# #   {
# #     return()
# #   }else{
# #     left_order5$Item_Position.y[i] <- '0'
# #   }
# # }
# # 
# # left_order5
# # 
# # #检查item position
# # 
# # class(left_order5)
# # sapply(left_order5, typeof)
# # 
# # classification[, Reseller == "NA"]
# 
# #连接service map和sales order
# m3_6 <- merge(sales_orders, service_map, by = 'Material_Class', all.x = T)
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
# 
# #对m_end矩阵建立每一个Mcode的订购情况，显示netvaluey
# m_end_netvaluey <- dcast(m_end, ... ~Material_Class, value.var = "Net_Value.y")
# 
# 
# #按照zoom的思路,Item_Position.x是原来order的Item_Position
# # for(i in c(1:nrow(m_end))){
# #   if(identical(m_end[i, Item_Position.x], m_end[i, Item_Position.y]))
# #   {
# #     next
# #   }else{
# #     if(identical(m_end[i, Item_Position.y], "0"))
# #     {
# #       next
# #     }else{
# #       m_end[i, Item_Position.y] <- '0'
# #     }
# #   }
# # }
# 
# #多列合并
# # m_m <- merge(m_customer, m_order, by=c("Sales_Order", "Item_Position"))
# # 
# # join <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/joined_sales_orders.csv')
# # 
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


classification <- fread('C:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/classification.csv')
# customers <- fread('E:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/customers.csv')
sales_orders <- fread('C:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/sales_orders.csv')
joined_sales_orders <- fread('C:/TUM/22WS/Business Analytics and Machine Learning (IN2028)/AC_Project/training_data/joined_sales_orders.csv')



n_each_Material_Class <- sales_orders[, .N, by = 'Material_Class']
n_Material_Class<- nrow(n_each_Material_Class)
n_sales_orders <- nrow(joined_sales_orders)
n_customers <- nrow(classification)

n_items <- matrix(0, nrow = n_customers, ncol = n_Material_Class)
accumulated_price_items <- matrix(0, nrow = n_customers, ncol = n_Material_Class)
n_orders <- matrix(0, nrow = n_customers, ncol = n_Material_Class)

for(i in c(1:n_sales_orders)){
  Customer_ID <- joined_sales_orders[i,'Customer_ID']
  Material_Class <- joined_sales_orders[i,'Material_Class']
  
  Customer_Position <- which(classification$Customer_ID == Customer_ID)
  Material_Class_Position <- which(n_Material_Class$Material_Class == Material_Class)
  
  N <- joined_sales_orders[i,'Num_Items']
  Price <- joined_sales_orders[i,'Net_Value']
  
  n_items[Customer_Position,Material_Class_Position] <- n_items[Customer_Position,Material_Class_Position] + N
  accumulated_price_items[Customer_Position,Material_Class_Position] <- accumulated_price_items[Customer_Position,Material_Class_Position] + Price
  n_orders[Customer_Position,Material_Class_Position] <- n_orders[Customer_Position,Material_Class_Position] + 1 
  
 }







