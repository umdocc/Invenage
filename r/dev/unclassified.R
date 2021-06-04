# this function get the latest actual_unit_price for all products
# it should be included in reports section

# get_actual_unit_price <- function(po_filter_str, method='latest_import', 
#                                   remove_null=T,multi_resolve='mean'){
#   # query the database using po_filter_str
#   query <- paste0("select * from import_log where po_name like '%",
#                   po_filter_str,"%'")
#   import_data <- db_read_query(query)
#   
#   # remove items with zero/NA price
#   if(remove_null){
#     import_data <- import_data %>% 
#       filter(actual_unit_cost!=0 & !is.na(actual_unit_cost))
#   }
#   
#   # filter for delivery_date
#   if(method=='latest_import'){
#     import_data <- import_data %>% group_by(prod_code) %>% 
#       mutate(latest_delivery_date=max(delivery_date))
#     import_data <- import_data %>% filter(delivery_date==latest_delivery_date)
#   }
#   
#   # resolve situation where the same product is imported multiple time
#   # in one single day
#   if(multi_resolve=='mean'){
#     import_data <- import_data %>% group_by(prod_code) %>% 
#       summarise(mean_unit_cost=mean(actual_unit_cost))
#   }
#   return(import_data)
# }

# latest_unit_price <- get_actual_unit_price(po_filter_str=config$po_file_include)
# latest_unit_price <- merge(latest_unit_price,product_info, by='prod_code')
# 
# write.xlsx(latest_unit_price,"~/Downloads/report_data.xlsx")
