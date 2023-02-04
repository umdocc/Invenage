# ----------------------- UIs and render functions -----------------------------
pir_load_ui <- function(input,output,ui_list){
  if('pir_data' %in% ui_list){
    output$pir_data <- render_pir_data(input)
  }
  
  return(output)
}

# render display data
render_pir_data <- function(input){DT::renderDataTable({
  
  # get current vendor_id
  current_vid <- vendor_info$vendor_id[
    vendor_info$vendor==input$pir_vendor]
  
  # generate tables based on report type
  if(input$pir_report_type==uielem$value_report){
    inventory_report <- pir_get_value_report(current_vid)
    output_tbl <- pir_get_value_report_sum(inventory_report)
  }
  if(input$pir_report_type==uielem$po_report){
    output_tbl <- pir_get_po_report(current_vid)
  }
  if(input$pir_report_type==uielem$separate_lot){
    
    output_tbl <- pir_get_separate_lot_report(
      vendor_id = current_vid)
  }
  if(input$pir_report_type==uielem$full_product){
    
    output_tbl <- pir_get_full_product_report()
  }
  
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
  
  # if full product report is selected
  if(pir_data$report_type==uielem$full_product){
    inventory_report <- pir_get_full_product_report()
  }
  
  # if value report is selected
  if(pir_data$report_type==uielem$value_report){
    inventory_report <- pir_get_value_report(pir_data$vendor_id)
  }
  
  # if report used for po placement is selected
  if(pir_data$report_type==uielem$po_report){
    inventory_report <- pir_get_po_report(vendor_id = pir_data$vendor_id)
  }
  
  # if report used for inventory control is used
  if(pir_data$report_type==uielem$separate_lot){
    inventory_report <- pir_get_separate_lot_report(
      vendor_id = pir_data$vendor_id)
  }
  
    
  pir_print_report(pir_data, inventory_report)
}


pir_print_report <- function(pir_data, inventory_report){
  
  
  # get custom data based on report types
  if(pir_data$report_type==uielem$value_report){
    required_cols <- c('vendor', 'comm_name', 'ref_smn', 'total_remain_qty',
                       'mean_unit_cost', 'total_value')
    inventory_report_sum <- pir_get_value_report_sum(inventory_report)
    inventory_report_sum <- translate_tbl_column(inventory_report_sum, uielem)
  }
  if(pir_data$report_type==uielem$po_report){
    required_cols <- c('comm_name', 'ref_smn', 'total_remain_qty',
                       'monthly_sale')
  }
  if(pir_data$report_type==uielem$separate_lot){
    required_cols <- split_semi(config$pir_separate_lot_report_col)
  }
  if(pir_data$report_type==uielem$full_product){
    required_cols <- split_semi(config$pir_full_product_report_col)
  }
  
  # filter the required columns
  inventory_report <- inventory_report[,required_cols]
  
  # translate the table
  inventory_report <- translate_tbl_column(inventory_report,uielem)
  
  # preparing sheet data
  if(pir_data$report_type==uielem$value_report){
    list_of_sheets <- list()
    list_of_sheets[[uielem$summary]] <- inventory_report_sum
    list_of_sheets[[uielem$detail]] <- inventory_report
  }else{
    list_of_sheets <- list()
    list_of_sheets[[uielem$detail]] <- inventory_report
  }
  
  # write excel using list of sheet
  write.xlsx(list_of_sheets, file=pir_data$output_path,
             overwrite = T)
  open_location(pir_data$output_path)
}

# separate lot is just inventory without sum
pir_get_separate_lot_report <- function(vendor_id){
  
  # collect other information
  inventory_report <- merge(
    inventory, product_info %>% 
      select(prod_code, comm_name,ref_smn, vendor_id),
    by='prod_code',all.x=T)
  inventory_report <- merge(
    inventory_report, vendor_info %>% select(vendor_id, vendor),
    by='vendor_id',all.x=T)
  
  if(vendor_id!=0){
    inventory_report <- inventory_report[inventory_report$vendor_id==vendor_id,]
  }
  if(!is.null(config$pir_separate_lot_report_col)){
    col_names <- split_semi(config$pir_separate_lot_report_col)
    inventory_report <- inventory_report %>% select(col_names)
  }
  return(inventory_report)
}

# generate the inventory report as data_frame
pir_get_value_report <- function(vendor_id=0){
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
  if(vendor_id!=0){
    inventory_report <- inventory_report[
      inventory_report$vendor_id == vendor_id,]
  }
  
  actual_unit_price <- get_actual_unit_price(
    po_filter_str=config$po_file_include)
  inventory_report <- merge(inventory_report,actual_unit_price,
                            by='prod_code',all.x=T)
  inventory_report$total_value <- 
    inventory_report$total_remain_qty*inventory_report$mean_unit_cost
  
  return(inventory_report)
  
}

pir_get_value_report_sum <- function(inventory_report){
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

pir_get_exp_first_report <- function(
    vendor_id = c(1,2,3,4,5,6,7), mode = "full", cut_off = 0, extra_info=T){
  
  # prepare data and mark expired items
  report_data <- get_separate_lot_report(vendor_id = 0)
  report_data$expired <- report_data$intexp_date <= (Sys.Date()+cut_off)
  
  # filter on mode
  if(mode=="exp_only"){
    report_data <- report_data[
      report_data$expired & !is.na(report_data$expired),]
  }
  
  # add po name and delivery date if extra_info = T
  if(extra_info){
    import_data <- import_log %>% group_by(prod_code, lot, po_name) %>% 
      summarise(delivery_date=max(delivery_date))
    import_data <- import_data[grepl(".PO.", import_data$po_name),]
    import_data <- import_data %>% group_by(prod_code, lot) %>% 
      mutate(max_delivery_date=max(delivery_date)) %>% 
      filter(delivery_date==max_delivery_date)
    report_data <- merge(report_data, import_data, all.x=T)
  }
  
  # filter vendor id
  if(vendor_id!=0){
    report_data <- report_data[report_data$vendor_id %in% vendor_id,]
  }
  
  return(report_data)
}

#simple function to write out all inventory
pir_get_full_product_report <- function(pos_item = F){
  
  if(!pos_item){
    inventory_report <- pir_get_value_report(vendor_id=0) %>% 
      select(prod_code, vendor_id, total_remain_qty)
    inventory_report <- merge(product_info, inventory_report, all.x = T)
    inventory_report <- merge(inventory_report, 
                              vendor_info %>% select(vendor_id, vendor), 
                              all.x = T)
  }
  
  return(inventory_report)
}

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

pir_get_po_report <- function(vendor_id, keep_prod_code=F){
  if(vendor_id==0){
    show_error("invalid_vendor",set_error = T)
    inventory_report <- inventory
  }else{
    inventory_report <- pir_get_value_report(vendor_id) %>% 
      select(prod_code, vendor_id, total_remain_qty)
    sale_report <- ssr_get_sale_average()
    sale_report <- merge(
      sale_report, 
      product_info %>% select(prod_code, vendor_id), all.x=T)
    sale_report <- sale_report[sale_report$vendor_id == vendor_id,]
    inventory_report <- merge(inventory_report, sale_report,all = T)

    inventory_report <- merge(
      inventory_report, product_info %>% select(prod_code, comm_name, ref_smn)
    )
    inventory_report <- inventory_report %>%
      select(prod_code, comm_name, ref_smn, total_remain_qty, monthly_sale)
  }
  
  if(!keep_prod_code){
    inventory_report$prod_code <- NULL
  }
  
  return(inventory_report)
}