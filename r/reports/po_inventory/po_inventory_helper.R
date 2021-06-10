# create 

# generate the po_report used for placing a po
create_po_report <- function(input,open_file=T){

  input_vendor <- input$por_vendor
  lookback_yr <- as.numeric(config$stats_lookback_yr)
  input_vendor_id <- db_read_query(paste0(
    "select vendor_id from vendor_info where vendor='",input_vendor,"'"))$vendor_id
  query <- paste0(
    "select product_info.prod_code, product_info.comm_name, product_info.ref_smn 
    from product_info where vendor_id=", input_vendor_id," and active=1")
  product_list <- db_read_query(query)
  to_date <- Sys.Date()
  from_date <- to_date - ceiling(365*lookback_yr)

  sales_report <- get_sales_report(input_vendor_id,from_date,to_date)
  sales_report$ave_mth_sale <- sales_report$total_sale_pack/(12*lookback_yr)
  
  sl_report <- get_sl_report(input_vendor_id)
  po_report <- gen_suggested_order(product_list, sales_report, sl_report)
  
  po_report <- round_report_col(po_report, 'suggested_order', decimal = 0)
  
  #clean up and translate
  po_report <- po_report %>% select(comm_name,ref_smn,total_remain_qty,
                                    ave_mth_sale,median_sl_mth,
                                    max_sl_mth,suggested_mth_stock,
                                    suggested_order,note)
  po_report <- translate_tbl_column(po_report,ui_elem)
  
  dest_path <- file.path(
    config$report_out_path,paste0(config$report_name_default,".xlsx"))
  write.xlsx(po_report,dest_path)
  if(open_file){
    system2('open',dest_path,timeout = 2)
  }
}

gen_suggested_order <- function(product_list, sales_report, sl_report){
  # merge all reports and generate suggested stock requirements
  master_report <- merge(product_list,sales_report,by='prod_code',all.x=T)
  master_report <- merge(master_report,sl_report,by='prod_code',all.x=T)
  master_report$suggested_mth_stock <- config$mth_stock
  master_report$suggested_mth_stock[master_report$max_sl_mth<18] <- 
    config$mth_stock_less18
  master_report$suggested_mth_stock[master_report$max_sl_mth<12] <- 
    config$mth_stock_less12
  master_report$note <- ifelse(
    master_report$median_sl_mth/master_report$max_sl_mth<0.8, "check date",'')
  
  # merge with current inventory and generate suggested order
  current_inv <- update_inventory(config_dict) %>% group_by(prod_code) %>%
    summarise(total_remain_qty = sum(remaining_qty),.groups='drop')
  master_report <- merge(master_report,current_inv,by='prod_code',all.x=T)
  master_report$suggested_order <- 
    as.numeric(master_report$suggested_mth_stock)*master_report$ave_mth_sale-
    master_report$total_remain_qty
  master_report$suggested_order[master_report$suggested_order<0] <- 0
  return(master_report)
} 

# generate the report on sales
get_sales_report <- function(input_vendor_id, from_date, to_date,
                             summary=T){
  # extract the sales data
  query <- paste0(
    "select sale_log.prod_code, sale_log.unit, sale_log.qty,
    sale_log.pxk_num from sale_log inner join product_info 
    on sale_log.prod_code=product_info.prod_code
    inner join pxk_info on sale_log.pxk_num=pxk_info.pxk_num
    where product_info.vendor_id=",input_vendor_id
    ," and pxk_info.sale_datetime between '",from_date,"' and '",to_date,"'")
  sale_data <- db_read_query(query)
  sale_data <- convert_to_pack(sale_data,packaging,'qty','pack_qty')
  if(summary){
    sale_data <- sale_data %>%  group_by(prod_code) %>% 
      summarise(total_sale_pack=sum(pack_qty),.groups='drop')
  }
  return(sale_data)
}

# generate the report on shelf life
get_sl_report <- function(input_vendor_id){
  query <- paste0(
  "select import_log.prod_code, import_log.unit, import_log.qty,
   import_log.exp_date, import_log.delivery_date, product_info.vendor_id 
   from import_log inner join product_info 
   on import_log.prod_code=product_info.prod_code
   where import_log.po_name like '%",config$po_file_include,"%'
   and product_info.vendor_id=", input_vendor_id)
  
  po_import <- db_read_query(query)
  po_import$exp_date <- gsub(" .*$","",po_import$exp_date)
  
  po_import$exp_date_full <- parse_date_time(
    po_import$exp_date, orders = c('y-m-d','d-m-y','y-m','m-y'))
  
  po_import <- po_import[!is.na(po_import$exp_date_full),]
  po_import$useful_life <- difftime(po_import$exp_date_full,po_import$delivery_date,
                                    units = "days")
  
  shelf_life_stats <- po_import %>% group_by(prod_code) %>% 
    summarise(median_sl_mth = as.numeric(median(useful_life))/365*12,
              min_sl_mth = as.numeric(min(useful_life))/365*12,
              max_sl_mth = as.numeric(max(useful_life))/365*12,
              .groups='drop')
  
  return(shelf_life_stats)
}



# render_slr_product <- function(input){renderUI({
#   # get list of products currently sold to a customer
#   current_customer_name <- input$slr_customer
#   current_customer_id <- db_read_query(paste0(
#     "select customer_id from customer_info where customer_name='",
#     current_customer_name,"'"))
#   product_list <- db_read_query(paste0(
#     "select distinct prod_code from sale_log inner join pxk_info
#     on sale_log.pxk_num = pxk_info.pxk_num 
#     where pxk_info.customer_id =",current_customer_id))
#   
#   # render the UI
#   selectizeInput(
#     inputId = "slr_product", label=uielem$comm_name,
#     choices = product_list, selected = product_list[1])
# })}

print_inventory_report <- function(input){
  value_report <- input$cir_value_report
  current_vendor_id <- db_read_query(paste0(
    "select vendor_id from vendor_info
  where vendor='",input$cir_vendor,"'"))$vendor_id
  separate_lot <- input$cir_separate_lot
  expiry_first <- input$cir_expiry_first
  
  if(value_report){
    knitr_path <- file.path(config$knitr_path,"inventory_report.Rmd")
    output_path <- file.path(config$report_out_path,
                             paste0(config$report_name_default,".docx"))
    rmarkdown::render(knitr_path, output_format = "word_document",
                      output_file = output_path)
  }
  
  system2('open',output_path,timeout = 2)
}



#convert to value report
convert_to_value_report <- function(inventory_report){
  if(length(inventory_report$prod_code[
    duplicated(inventory_report$prod_code)])>0){
      stop('inventory report not summarised')
  }else{
    actual_unit_price <- get_actual_unit_price(
      po_filter_str=config$po_file_include)
    inventory_report <- merge(inventory_report,actual_unit_price,
                              by='prod_code',all.x=T)
    tmp <- db_read_query("select prod_code, vendor_id,comm_name, ref_smn
                           from product_info")
    inventory_report <- merge(inventory_report, tmp,by='prod_code',all.x=T)
    inventory_report$total_value <- 
      inventory_report$total_remain_qty*inventory_report$mean_unit_cost
    
    # create summary table
    tmp <- db_read_query("select vendor_id, vendor from vendor_info")
    inventory_report_sum <- inventory_report %>% 
      group_by(vendor_id) %>%
      summarise(total_value_sum = sum(total_value,na.rm=T))
    

    inventory_report_sum$comm_name <- uielem$total_inv_value
    
    #rename for consistency before rbind
    inventory_report_sum <- inventory_report_sum %>% 
      rename(total_value = total_value_sum)
    inventory_report_sum <- inventory_report_sum %>% 
      mutate( total_remain_qty = NA, mean_unit_cost = NA, prod_code = NA,
              ref_smn = NA)
    
    inventory_report <- merge(inventory_report,tmp,by='vendor_id')
    inventory_report_sum <- merge(inventory_report_sum,tmp,by='vendor_id')
    spacing_df <- create_spacing_df(inventory_report)
    
    inventory_report <- rbind(inventory_report_sum,spacing_df,
                              inventory_report)
    
    return(inventory_report)
  }
}

# if method='latest_import' the program will calculate actual_unit_price from
# the latest import first, then use latest local import price for remainin items
get_actual_unit_price <- function(po_filter_str, method='latest_import', 
                                  remove_null=T,multi_resolve='mean'){
  # query the database using po_filter_str
  import_data <- db_read_query(paste0("select * from import_log 
                                      where po_name like '%",
                                      po_filter_str,"%'"))
  
  import_data <- get_unit_price_from_import_data(import_data, remove_null,
                                                 method,multi_resolve)
  
  # local import data
  local_data <- db_read_query(paste0("select * from import_log 
                                     where po_name not like '%",
                                     po_filter_str,"%'"))
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

# reload_slr_ui <- function(input,output,ui_list){
#   if ('slr_product' %in% ui_list){
#     output$slr_product <- render_slr_product(input)
#   }
# }