# functions related to preparing reports
# create_report function
get_rp_filename <- function(report_type, config_dict){
  report_name <- ui_elem$actual[ui_elem$label==report_type]
  rp_filename <- file.path( # report file name
    config_dict$value[config_dict$name=='report_out_path'], paste0(
      config_dict$value[config_dict$name=='company_name'], '.',
      report_name,'.',
      format(Sys.Date(),config_dict$value[config_dict$name=='date_format']),
      '.xlsx') )
  return(rp_filename)
}

write_inv_value_rp <- function(){
  rp_data <- update_inventory(config_dict)
  # gather all informations
  rp_filename <- get_rp_filename('inv_value_report', config_dict)
  summary_sheet_name <- ui_elem$actual[ui_elem$label=='summary']
  missing_price_sheet_name <- ui_elem$actual[ui_elem$label=='missing_price']
  totalNSXcostName <- ui_elem$actual[ui_elem$label=='total_inv_value']
  val_by_vendor <- rp_data %>% group_by(vendor) %>% summarise(
    total_inv_value = sum(total_inv_value,na.rm=T)) %>% ungroup
  val_by_vendor <- val_by_vendor[!is.na(val_by_vendor$vendor),]
  # get vendor list here
  vendor_list <- gsub('-.*$','',val_by_vendor$vendor)
  vendor_list <- vendor_list[!duplicated(vendor_list)]
  # add total cost, and format the ouput
  tmp <- val_by_vendor[1:2,]
  tmp[1,] <- ''
  tmp$vendor[2] <- ui_elem$actual[ui_elem$label=='total_inv_value']
  tmp$total_inv_value[2] <- sum(val_by_vendor$total_inv_value,na.rm=T)
  val_by_vendor <- rbind(val_by_vendor,tmp)

  val_by_vendor$total_inv_value <- format(
    as.numeric(val_by_vendor$total_inv_value), big.mark=",")
  
  wb <- createWorkbook()
  addWorksheet(wb, summary_sheet_name)
  addWorksheet(wb, missing_price_sheet_name)
  # write missing_price
  missing_price <- rp_data[is.na(rp_data$ave_pack_import_cost),] %>%
    select(prod_code,name,ref_smn)
  missing_price <- translate_tbl_column(missing_price,ui_elem)
  writeData(wb, sheet=missing_price_sheet_name, missing_price)
  # write summary sheet
  output_rp <- translate_tbl_column(val_by_vendor,ui_elem)
  writeData(wb, sheet=summary_sheet_name, output_rp)

  for (i in 1:length(vendor_list)){
    addWorksheet(wb, vendor_list[i])
    tmp_df <- rp_data[grepl(vendor_list[i],rp_data$vendor),] %>%
      select(name,ref_smn,lot,exp_date,remaining_qty,ave_pack_import_cost,
             total_inv_value)
    tmp_df <- translate_tbl_column(tmp_df,ui_elem)
    writeData(wb, sheet=vendor_list[i], tmp_df)
  }
  saveWorkbook(wb,rp_filename,overwrite = T)
  return(rp_filename)
}

get_sales_report <- function(config_dict, from_date='2019-11-04',
                             to_date = '2019-11-10'){
  # getting variables ready
  from_date <- strptime(from_date,'%Y-%m-%d')
  # since strptime start from 00:00:00 we need to add a day in seconds 
  # to include the to_date
  to_date <- strptime(to_date,'%Y-%m-%d')+(24*60*60-1)
  # database read
  conn <- db_open(config_dict)
  tmp <- dbReadTable(conn,'sale_log')
  sale_log <- dbReadTable(conn,'sale_log')
  pxk_info <- dbReadTable(conn,'pxk_info')
  import_log <- dbReadTable(conn,'import_log')
  customer_info <- dbReadTable(conn,'customer_info')
  product_info <- dbReadTable(conn,'product_info')
  packaging <- dbReadTable(conn,'packaging')
  dbDisconnect(conn)
  
  # data manipulation
  tmp <- merge(tmp,packaging %>% 
                 select(unit,units_per_pack,prod_code),all.x=T)
  tmp <- merge(tmp,pxk_info %>% select(
    pxk_num,customer_id,sale_datetime),all.x = T)
  tmp$sale_datetime <- strptime(tmp$sale_datetime,"%Y-%m-%d %H:%M:%S")
  
  tmp <- tmp[((tmp$sale_datetime>=from_date) & (tmp$sale_datetime<= to_date)),]
  tmp <- tmp[!is.na(tmp$prod_code),]
  # check data integrity
  if (nrow(tmp[duplicated(tmp %>% select(pxk_num,prod_code,lot,qty,note)),])>0){
    print(tmp[duplicated(tmp %>% select(pxk_num,prod_code,lot)),])
    stop('Critical error! duplications found in sale_log')
  }
  
  ave_import_cost <- get_est_import_cost(
    import_log, algorithm='weighted_average')
  tmp <- merge(tmp,ave_import_cost,all.x = T)
  
  # sales for current week
  tmp <- merge(tmp,customer_info %>% select(customer_id,customer_name),all.x=T)
  tmp <- merge(tmp,product_info %>% select(prod_code,name,ref_smn),all.x=T)
  
  # calculating all prices data
  tmp$unit_import_cost <- tmp$ave_pack_import_cost/tmp$units_per_pack
  tmp$unit_profit <- tmp$unit_price-tmp$unit_import_cost
  tmp$total_profit <- tmp$unit_profit*tmp$qty
  tmp$profit_margin <- round(100*((tmp$unit_price/tmp$unit_import_cost)-1),
                             digits=1)
  
  # preparing output
  tmp <- tmp[order(tmp$customer_name),]
  tmp$sale_date <- strftime(tmp$sale_datetime,'%d-%m-%Y')
  tmp <- tmp %>% select(
    customer_name, name, pxk_num, sale_date, unit, qty, unit_price,
    unit_import_cost, unit_profit, total_profit, profit_margin)
  return(tmp)
}

# a report comprise of report_name, from_date, to_date and rp_data
write_report_data <- function(
  report_type,rp_data,from_date,to_date){
  
  # read report configuration from database
  conn <- db_open(config_dict)
  report_info <- dbReadTable(conn,"output_info")
  report_info <- report_info[report_info$type=='report_output',]
  dbDisconnect(conn)
  # get the input,output file, report name
  rp_filename <- get_rp_filename(report_type, config_dict)
  report_name <- ui_elem$actual[ui_elem$label==report_type]
  rp_form <- config_dict$value[config_dict$name=='report_form_path']
  
  wb <- loadWorkbook(rp_form)
  #writing data
  writeData( # report name
    wb,sheet=1,report_name, 
    startRow = report_info$value[report_info$name=='report_name_r'], 
    startCol = report_info$value[report_info$name=='report_name_c'])
  writeData( # from_date
    wb,sheet=1,from_date, 
    startRow = report_info$value[report_info$name=='from_date_r'], 
    startCol = report_info$value[report_info$name=='from_date_c'])
  writeData( # to_date
    wb,sheet=1,to_date, 
    startRow = report_info$value[report_info$name=='to_date_r'], 
    startCol = report_info$value[report_info$name=='to_date_c'])
  # actual data
  writeData(wb,sheet=1,rp_data, startRow = 5)
  saveWorkbook(wb,rp_filename,overwrite = T) #save the workbook
  return(rp_filename)
}

build_rp_data <- function(report_type, input){
  if (report_type == 'inv_value_report'){
    # refresh information
    rp_data <- update_inventory(config_dict)
  }
  if (report_type == 'inv_audit_report'){
    # read the inventory
    rp_data <- update_inventory(config_dict)
    # set all negative number to 0
    rp_data <- rp_data[rp_data$remaining_qty>0,]
    # add ordering unit
    ordering_unit <- get_ordering_unit(packaging)
    rp_data <- merge(rp_data,ordering_unit,all.x=T)
    rp_data <- rp_data %>% 
      select(vendor,name,ref_smn,lot,exp_date,remaining_qty,unit)
    rp_data <- round_report_col(rp_data,'remaining_qty')
  }
  if (report_type == 'inv_order_report'){
    # read the inventory
    rp_data <- update_inventory(config_dict)
    # set all negative number to 0
    rp_data <- rp_data[rp_data$remaining_qty>0,]
    rp_data <- rp_data %>% group_by(prod_code) %>%
      summarise(remaining_qty = sum(remaining_qty)) %>% ungroup
    # merge with prod_info so that we get zero items as well
    rp_data <- merge(rp_data,product_info %>%
                               select(prod_code,type),all.y=T)
    # add sales_summary
    sales_summary <- get_sales_summary(config_dict)
    rp_data <- merge(rp_data,sales_summary %>% select(
      prod_code,ave_mth_sale), all.x=T)
    #add other info
    rp_data <- add_inv_report_info(rp_data)
    # add importlic_exp data
    rp_data <- merge(rp_data,importlic_data,all.x=T)
    rp_data$mth_supply_left <- rp_data$remaining_qty /
      rp_data$ave_mth_sale
    rp_data <- round_report_col(
      rp_data, col_list = c('ave_mth_sale', 'mth_supply_left', 'remaining_qty'),
      decimal = 2)
    rp_data <- rp_data %>%
      select(vendor, name, ref_smn, warehouse, remaining_qty,
             unit, ave_mth_sale,mth_supply_left,importlic_exp)
  }
  # get the report data
  if (report_type == 'sale_profit_report'){
    from_date <- input$from_date # from_date and to_date depends on rp type
    to_date <- input$to_date
    rp_data <- get_sales_report(config_dict,from_date,to_date)
    rp_data <- clean_duplicates(
      rp_data,col_list = c("customer_name", "sale_date", "pxk_num"))
  }
  if (report_type == 'inv_exp_date_report'){
    rp_data <- update_inventory(config_dict)
    rp_data$remaining_days <- rp_data$intexp_date-Sys.time()
    rp_data$label[rp_data$remaining_days<180] <- 'less_than_6mth'
    rp_data$label[rp_data$remaining_days<90] <- 'less_than_3mth'
    rp_data <- rp_data[order(rp_data$intexp_date),]
    rp_data <- merge(rp_data,ui_elem,all.x=T)
    rp_data$note <- rp_data$actual
    rp_data <- rp_data %>% select(vendor,name,ref_smn,remaining_qty,
                                      exp_date,note)
  }
  rp_data <- translate_tbl_column(rp_data,ui_elem)
  return(rp_data)
}

add_inv_report_info <- function(inventoryReport){
  #recover human-readble info
  inventoryReport <- merge(
    inventoryReport, product_info %>% select(
      prod_code,name,vendor,ref_smn,warehouse_id))
  inventoryReport <- merge(
    inventoryReport,warehouse_info %>% select(warehouse_id,warehouse))

  # add ordering unit
  ordering_unit <- get_ordering_unit(packaging)
  inventoryReport <- merge(inventoryReport, ordering_unit, all.x=T)
  # select the appropriate column
  return(inventoryReport)
}

round_report_col <- function(rp_data,col_list,decimal = 2){
  for (i in col_list){
    rp_data[[i]] <- round(rp_data[[i]], digits= decimal)
  }
  return(rp_data)
}