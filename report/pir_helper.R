# ----------------------- UIs and render functions -----------------------------
pir_load_ui <- function(input,output,ui_list){
  if('pir_data' %in% ui_list){
    output$pir_data <- render_pir_data(input)
  }
  
  return(output)
}

# render display data
render_pir_data <- function(input){DT::renderDataTable({

  # we will need to access input here as these values 
  # has not been written to global data
  if(input$pir_report_type==uielem$value_report){
    inventory_report <- get_value_report(vendor_info$vendor_id[
      vendor_info$vendor==input$pir_vendor])
    output_tbl <- get_value_report_sum(inventory_report)
  }
  
  # # if po_report is selected
  # if(report_type==uielem$po_report){
  #   output_tbl <- get_po_report(current_vendor_id) #get data
  # }
  # 
  # if(report_type==uielem$separate_lot){
  #   output_tbl <- get_separate_lot_report(current_vendor_id)
  # }
  # 
  # if(report_type==uielem$expiry_first){
  #   output_tbl <- gen_empty_df()
  # }
  
  #translate and render
  output_tbl <- translate_tbl_column(output_tbl,uielem)
  DT::datatable(output_tbl, options = list(pageLength = 15),rownames=F)
})
}

pir_create_report <- function(input){
  
  # config variables
  pir_data$report_type <- input$pir_report_type
  pir_data$vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor==input$pir_vendor]
  gbl_write_var("pir_data",pir_data)
  
  # if value report is selected
  if(pir_data$report_type==uielem$value_report){
    inventory_report <- get_value_report(pir_data$vendor_id)
  }
  
  pir_print_report(pir_data, inventory_report)
}


pir_print_report <- function(pir_data, inventory_report){
  
  
  
  if(pir_data$report_type==uielem$value_report){
    
    inventory_report_sum <- get_value_report_sum(inventory_report)
    inventory_report_sum <- translate_tbl_column(inventory_report_sum, uielem)
  }
  
  # translate and write
  inventory_report <- inventory_report %>%
    select(vendor_id,comm_name,ref_smn,total_remain_qty,mean_unit_cost,
           total_value)
  inventory_report <- translate_tbl_column(inventory_report,uielem)
  
  if(pir_data$report_type==uielem$value_report){
    list_of_sheets <- list()
    list_of_sheets[[uielem$summary]] <- inventory_report_sum
    list_of_sheets[[uielem$detail]] <- inventory_report
  }else{
    list_of_sheets <- list()
    list_of_sheets[[uielem$summary]] <- inventory_report_sum
  }
  
  # write excel using list of sheet
  write.xlsx(list_of_sheets, file=pir_data$output_path,
             overwrite = T)
  open_location(pir_data$output_path)
  
  
}
#   
#   if(report_type==uielem$separate_lot){
#     output_data <- get_separate_lot_report(current_vendor_id)
#     output_data <- translate_tbl_column(output_data,uielem)
#     write.xlsx(output_data, file=output_path,overwrite = T)
#   }
#   
#   if(report_type==uielem$expiry_first){
#     print('reserver for report with expiry first')
#   }
#   
#   # if po_report is selected
#   if(report_type==uielem$po_report){
#     # generate data
#     output_data <- get_po_report(current_vendor_id)
#     
#     # translate and write
#     output_data <- translate_tbl_column(output_data,uielem)
#     write.xlsx(output_data, file=output_path,overwrite = T)
#   }
# 
#   system2('open',output_path,timeout = 2)
# }

# separate lot is just inventory without sum
get_separate_lot_report <- function(vendor_id){

  # collect other information
  inventory_report <- merge(inventory,
                            product_info %>% 
                              select(prod_code, comm_name,ref_smn, vendor_id),
                            by='prod_code',all.x=T)
  inventory_report <- merge(inventory_report,
                            vendor_info %>% 
                              select(vendor_id, vendor),
                            by='vendor_id',all.x=T)
  
  if(vendor_id!=0){
    inventory_report <- inventory_report[inventory_report$vendor_id==vendor_id,]
  }
  col_names <- split_semi(config$pir_separate_lot_report_col)
  inventory_report <- inventory_report %>% select(col_names)
  
  return(inventory_report)
}

# generate the inventory report as data_frame
get_value_report <- function(vendor_id=0){
  inventory_report <- inventory %>%
    group_by(prod_code) %>%
    summarise(total_remain_qty = sum(remaining_qty))
  
  # collect other information
  inventory_report <- merge(inventory_report,
                            product_info %>% 
                              select(prod_code, comm_name,ref_smn, vendor_id),
                            by='prod_code',all.x=T)
  inventory_report <- merge(inventory_report,
                            vendor_info %>% 
                              select(vendor_id, vendor),
                            by='vendor_id',all.x=T)
  
  actual_unit_price <- get_actual_unit_price(
    po_filter_str=config$po_file_include)
  inventory_report <- merge(inventory_report,actual_unit_price,
                            by='prod_code',all.x=T)
  inventory_report$total_value <- 
    inventory_report$total_remain_qty*inventory_report$mean_unit_cost
  
  return(inventory_report)

}

get_value_report_sum <- function(inventory_report){
  inventory_report_sum <- inventory_report %>% 
    group_by(vendor,vendor_id) %>%
    summarise(total_value_sum = sum(total_value,na.rm=T))
  all_sum <- sum(inventory_report_sum$total_value_sum)
  inventory_report_sum$total_value_sum[inventory_report_sum$vendor_id==0] <- 
    all_sum
  
  #remove vendor_id as it is no longer needed
  inventory_report_sum$vendor_id <- NULL
  
  inventory_report_sum <- inventory_report_sum %>% 
    rename(total_value=total_value_sum)
  
  return(inventory_report_sum)
}

# # # generate the po_report used for placing a po
# get_po_report <- function(vendor_id,round_num=2,trans_col=T){
# 
#   lookback_yr <- as.numeric(config$stats_lookback_yr)
# 
#   query <- paste0(
#     "select product_info.prod_code, product_info.comm_name, product_info.ref_smn
#     from product_info where vendor_id=", vendor_id," and active=1")
#   product_list <- db_read_query(query)
#   to_date <- Sys.Date()
#   from_date <- to_date - ceiling(365*lookback_yr)
#  
#   sales_report <- get_sales_report(vendor_id,from_date,to_date)
#   sales_report$ave_mth_sale <- sales_report$total_sale_pack/(12*lookback_yr)
# 
#   sl_report <- get_sl_report(vendor_id)
#   po_report <- gen_suggested_order(product_list, sales_report, sl_report)
# 
#   po_report$suggested_order <- round(po_report$suggested_order,digits = 0)
# 
#   #clean up and translate
#   po_report <- po_report %>% select(comm_name,ref_smn,total_remain_qty,
#                                     ave_mth_sale,median_sl_mth,
#                                     max_sl_mth,suggested_mth_stock,
#                                     suggested_order,note)
#   if(trans_col){
#     po_report <- translate_tbl_column(po_report,uielem)
#   }
#   
#   
#   po_report <- round_df(po_report,digits = round_num)
#   return(po_report)
#   }
# 
# gen_suggested_order <- function(product_list, sales_report, sl_report){
#   # merge all reports and generate suggested stock requirements
#   master_report <- merge(product_list,sales_report,by='prod_code',all.x=T)
#   master_report <- merge(master_report,sl_report,by='prod_code',all.x=T)
#   master_report$suggested_mth_stock <- config$mth_stock
#   master_report$suggested_mth_stock[master_report$max_sl_mth<18] <- 
#     config$mth_stock_less18
#   master_report$suggested_mth_stock[master_report$max_sl_mth<12] <- 
#     config$mth_stock_less12
#   master_report$note <- ifelse(
#     master_report$median_sl_mth/master_report$max_sl_mth<0.8, "check date",'')
#   
#   # merge with current inventory and generate suggested order
#   current_inv <- update_inventory(config_dict) %>% group_by(prod_code) %>%
#     summarise(total_remain_qty = sum(remaining_qty),.groups='drop')
#   master_report <- merge(master_report,current_inv,by='prod_code',all.x=T)
#   master_report$suggested_order <- 
#     as.numeric(master_report$suggested_mth_stock)*master_report$ave_mth_sale-
#     master_report$total_remain_qty
#   master_report$suggested_order[master_report$suggested_order<0] <- 0
#   return(master_report)
# } 
# 
# # generate the report on sales
# get_sales_report <- function(input_vendor_id, from_date, to_date,
#                              summary=T){
#   # extract the sales data
#   query <- paste0(
#     "select sale_log.prod_code, sale_log.unit, sale_log.qty,
#     sale_log.pxk_num from sale_log inner join product_info 
#     on sale_log.prod_code=product_info.prod_code
#     inner join pxk_info on sale_log.pxk_num=pxk_info.pxk_num
#     where product_info.vendor_id=",input_vendor_id
#     ," and pxk_info.sale_datetime between '",from_date,"' and '",to_date,"'")
#   sale_data <- db_read_query(query)
#   sale_data <- convert_to_pack(sale_data,packaging,'qty','pack_qty')
#   if(summary){
#     sale_data <- sale_data %>%  group_by(prod_code) %>% 
#       summarise(total_sale_pack=sum(pack_qty),.groups='drop')
#   }
#   return(sale_data)
# }
# 
# # generate the report on shelf life
# get_sl_report <- function(input_vendor_id){
#   query <- paste0(
#   "select import_log.prod_code, import_log.unit, import_log.qty,
#    import_log.exp_date, import_log.delivery_date, product_info.vendor_id 
#    from import_log inner join product_info 
#    on import_log.prod_code=product_info.prod_code
#    where import_log.po_name like '%",config$po_file_include,"%'
#    and product_info.vendor_id=", input_vendor_id)
#   
#   po_import <- db_read_query(query)
#   po_import$exp_date <- gsub(" .*$","",po_import$exp_date)
#   
#   po_import$exp_date_full <- parse_date_time(
#     po_import$exp_date, orders = c('y-m-d','d-m-y','y-m','m-y'))
#   
#   po_import <- po_import[!is.na(po_import$exp_date_full),]
#   po_import$useful_life <- difftime(po_import$exp_date_full,po_import$delivery_date,
#                                     units = "days")
#   
#   shelf_life_stats <- po_import %>% group_by(prod_code) %>% 
#     summarise(median_sl_mth = as.numeric(median(useful_life))/365*12,
#               min_sl_mth = as.numeric(min(useful_life))/365*12,
#               max_sl_mth = as.numeric(max(useful_life))/365*12,
#               .groups='drop')
#   
#   return(shelf_life_stats)
# }

# if method='latest_import' the program will calculate actual_unit_price from
# the latest import first, then use latest local import price for remainin items
get_actual_unit_price <- function(po_filter_str, method='latest_import', 
                                  remove_null=T,multi_resolve='mean'){
  # query the database using po_filter_str
  import_data <- import_log[grepl(config$po_id_string,import_log$po_name),]
  
  import_data <- get_unit_price_from_import_data(import_data, remove_null,
                                                 method,multi_resolve)
  
  # local import data
  local_data <- import_log[!grepl(config$po_id_string,import_log$po_name),]
  local_data <- get_unit_price_from_import_data(local_data, remove_null,
                                                 method,multi_resolve)
  # remove prod_code found in import_data
  local_data <- local_data[!(local_data$prod_code %in% import_data$prod_code),]
  
  import_data <- rbind(import_data,local_data)
  
  return(import_data)
}

get_unit_price_from_import_data <- function(import_data, remove_null,
                                            method, multi_resolve){
  # remove items with zero/NA price
  if(remove_null){
    import_data <- import_data %>% 
      filter(actual_unit_cost!=0 & !is.na(actual_unit_cost))
  }
  # filter for delivery_date
  if(method=='latest_import'){
    import_data <- import_data %>% group_by(prod_code) %>% 
      mutate(latest_delivery_date=max(delivery_date))
    import_data <- import_data %>% filter(delivery_date==latest_delivery_date)
  }
  
  # resolve situation where the same product is imported multiple time
  # in one single day
  if(multi_resolve=='mean'){
    import_data <- import_data %>% group_by(prod_code) %>% 
      summarise(mean_unit_cost=mean(actual_unit_cost))
  }
  return(import_data)
}