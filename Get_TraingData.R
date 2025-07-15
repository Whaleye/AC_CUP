# install.packages("data.table")
# install.packages("magrittr")
# install.packages("tidyr")
# install.packages("dplyr")

library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)

path <- getwd()

classification <- fread(paste(path, "/classification.csv",sep=""))
customers <- fread(paste(path, "/customers.csv",sep=""))
sales_orders <- fread(paste(path, "/sales_orders.csv",sep=""))
sales_orders_header <- fread(paste(path, "/sales_orders_header.csv",sep=""))
business_units <- fread(paste(path, "/business_units.csv",sep=""))
service_map <- fread(paste(paste(path, "/service_map.csv",sep="")))
ch_joined <- fread(paste(paste(path, "/training_variables/joined_sales_orders.csv",sep="")))

Get_Value <- function(Final_Table, classification){
  customers <- classification[["Customer_ID"]]
  Material_Classes <- names(table(Final_Table[["Material_Class"]]))
  cust_length = length(customers)
  class_length = length(Material_Classes)
  Li = list()
  variables_BU <- array(0,dim=c(cust_length,class_length,3))
  variables_Type <- array(0,dim=c(cust_length,class_length,2))
  variables_SalesOrg <- array(0,dim=c(cust_length,class_length,2))
  Cycle = 1
  for(i in 1:length(customers)){
    # print(customers[i])
    print(Cycle)
    Cycle = Cycle+1
    pick_customer <- customers[i]
    place_customer <- which(Final_Table[["Customer_ID"]]==pick_customer)
    sub_customer <- Final_Table[place_customer, ]
    sub_MC <- names(table(sub_customer[["Material_Class"]]))
    for (j in 1:length(sub_MC)){
      pick_class <- sub_MC[j]
      h <- which(Material_Classes== pick_class)
      place_in_matrix <- which(Material_Classes==pick_class)
      place_class <- which(sub_customer[["Material_Class"]]==pick_class)
      
      code_BU <- sub_customer[place_class, ][["Business_Unit"]]
      code_Type <- sub_customer[place_class, ][["Type"]]
      code_SO <- sub_customer[place_class, ][["Sales_Organization"]]
      
      table_BU <- table(code_BU)
      num_BU = as.numeric(table_BU)
      name_table_BU = names(table_BU)
      if (length(table_BU)==3){
        variables_BU[i,h,1] = num_BU[1]
        variables_BU[i,h,2] = num_BU[2]
        variables_BU[i,h,3] = num_BU[3]
      }else if(length(table_BU)==2){
        if (name_table_BU[1]=="BU_A"&name_table_BU[2]=="BU_B"){
          variables_BU[i,h,1] = num_BU[1]
          variables_BU[i,h,2] = num_BU[2]
        }else if(name_table_BU[1]=="BU_A"&name_table_BU[2]=="BU_C"){
          variables_BU[i,h,1] = num_BU[1]
          variables_BU[i,h,3] = num_BU[2]
        }else{
          variables_BU[i,h,2] = num_BU[1]
          variables_BU[i,h,3] = num_BU[2]
        }
      }else{
        if(name_table_BU[1]=="BU_A"){
          variables_BU[i,h,1] = num_BU[1]
        }else if(name_table_BU[1]=="BU_B"){
          variables_BU[i,h,2] = num_BU[1]
        }else{
          
          variables_BU[i,h,3] = num_BU[1]
        }
      }
      table_Type <- table(code_Type)
      num_Type = as.numeric(table_Type)
      name_table_Type = names(table_Type)
      if (length(table_Type)==2){
        variables_Type[i,h,1] = table_Type[1]
        variables_Type[i,h,2] = table_Type[2]
      }else{
        if(name_table_Type[1]=="SOP"){
          variables_Type[i,h,1] = table_Type[1]
        }else{
          variables_Type[i,h,2] = table_Type[1]
        }
      }
      table_SO <- table(code_SO)
      num_SO = as.numeric(table_SO)
      name_table_SO = names(table_SO)
      if (length(table_SO)==2){
        variables_SalesOrg[i,h,1] = table_SO[1]
        variables_SalesOrg[i,h,2] = table_SO[2]
      }else{
        if(name_table_SO[1]=="A"){
          variables_SalesOrg[i,h,1] = table_SO[1]
        }else{
          variables_SalesOrg[i,h,2] = table_SO[1]
        }
      }
      
    }
  }
  Li$"Business_Unit" = variables_BU
  Li$"Type" = variables_Type
  Li$"Sales_Organization" = variables_SalesOrg
  return(Li)
}

Value = Get_Value(ch_joined, classification)
#BU_A,B,C
Business_Unit_data <- Value$Business_Unit
#SOP,STP
Type_data <- Value$Type
#A,B
Sales_Organization_data <- Value$Sales_Organization

D_BU_A <- Business_Unit_data[,,1]
D_BU_B <- Business_Unit_data[,,2]
D_BU_C <- Business_Unit_data[,,3]
D_SOP <- Type_data[,,1]
D_STP <- Type_data[,,2]
D_SOrg_A <- Sales_Organization_data[,,1]
D_SOrg_B <- Sales_Organization_data[,,2]

Reseller_V = classification[["Reseller"]]
D_BU_A <- as.data.frame(D_BU_A)
D_BU_A[,330] = Reseller_V
colnames(D_BU_A)[330] = "Reseller"
places_NA = which(is.na(classification[["Reseller"]]))
D_BU_A_Training = D_BU_A[-places_NA, ]
D_BU_A_Test = D_BU_A[places_NA, ]

D_BU_B <- as.data.frame(D_BU_B)
D_BU_B[,330] = Reseller_V
colnames(D_BU_B)[330] = "Reseller"
D_BU_B_Training = D_BU_B[-places_NA, ]
D_BU_B_Test = D_BU_B[places_NA, ]

D_BU_C <- as.data.frame(D_BU_C)
D_BU_C[,330] = Reseller_V
colnames(D_BU_C)[330] = "Reseller"
D_BU_C_Training = D_BU_C[-places_NA, ]
D_BU_C_Test = D_BU_C[places_NA, ]



write.csv(D_BU_A_Training, file = paste(path, "\\training_variables\\D_BU_A_Training.csv",sep=""),quote= FALSE,col.names=FALSE)
write.csv(D_BU_A_Test, file = paste(path, "\\training_variables\\D_BU_A_Test.csv",sep=""),quote= FALSE,col.names=FALSE)
write.csv(D_BU_B_Training, file = paste(path, "\\training_variables\\D_BU_B_Training.csv",sep=""),quote= FALSE,col.names=FALSE)
write.csv(D_BU_B_Test, file = paste(path, "\\training_variables\\D_BU_B_Test.csv",sep=""),quote= FALSE,col.names=FALSE)
write.csv(D_BU_C_Training, file = paste(path, "\\training_variables\\D_BU_C_Training.csv",sep=""),quote= FALSE,col.names=FALSE)
write.csv(D_BU_C_Test, file = paste(path, "\\training_variables\\D_BU_C_Test.csv",sep=""),quote= FALSE,col.names=FALSE)

