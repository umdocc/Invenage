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

create_report <- function(report_type,rp_filename,config_dict,input){
  # report_name (in local language)
  # report_name <- ui_elem$actual[ui_elem$label==report_type]

    # sale_profit_report
  if (report_type == 'sale_profit_report'){
    from_date <- input$from_date # from_date and to_date depends on rp type
    to_date <- input$to_date
    rp_data <- create_excel_report(
      config_dict,report_type,from_date,to_date,rp_filename)
    rp_data <- translate_tbl_column(rp_data,ui_elem)
  }
  # inv_exp_date_report
  if (report_type == 'inv_exp_date_report'){
    from_date <- strftime(Sys.Date())
    to_date <- from_date
    rp_data <- create_excel_report(
      config_dict, report_type, from_date, to_date, rp_filename)
    rp_data <- translate_tbl_column(rp_data,ui_elem)
  }
  # inventoryValueReport
  if (report_type == 'inventoryValueReport'){
    summary_sheet_name <- ui_elem$actual[ui_elem$label=='summary']
    missing_price_sheet_name <- ui_elem$actual[ui_elem$label=='missing_price']
    totalNSXcostName <- ui_elem$actual[ui_elem$label=='total_inv_value']
    
    # refresh information
    inventory <- update_inventory(config_dict)
    
    removeCountry <- TRUE # format the vendor
    if (removeCountry){
      inventory$vendor <- gsub('-.*$','',inventory$vendor)
    }
    val_by_vendor <- inventory %>% group_by(vendor) %>% summarise(
      total_inv_value = sum(total_inv_value,na.rm=T)) %>% ungroup
    val_by_vendor <- val_by_vendor[!is.na(val_by_vendor$vendor),]
    vendor_list <- gsub('-.*$','',val_by_vendor$vendor)
    
    # add total cost, and format the ouput
    tmp <- val_by_vendor[1:2,]
    tmp[1,] <- ''
    tmp$vendor[2] <- ui_elem$actual[ui_elem$label=='total_inv_value']
    tmp$total_inv_value[2] <- sum(val_by_vendor$total_inv_value,na.rm=T)
    val_by_vendor <- rbind(val_by_vendor,tmp)
    val_by_vendor$total_inv_value <- format(
      as.numeric(val_by_vendor$total_inv_value), big.mark=",")
    
    # this report use a new excel for now
    wb <- createWorkbook()
    addWorksheet(wb, summary_sheet_name)
    addWorksheet(wb, missing_price_sheet_name)
    
    # write missing_price
    missing_price <- inventory[is.na(inventory$ave_pack_import_cost),] %>%
      select(prod_code,name,ref_smn)
    missing_price <- rename_table(missing_price,ui_elem)
    writeData(wb, sheet=missing_price_sheet_name, missing_price)
    # write summary sheet
    val_by_vendor <- rename_table(val_by_vendor,ui_elem)
    writeData(wb, sheet=summary_sheet_name, val_by_vendor)
    rp_data <- val_by_vendor
    for (i in 1:length(vendor_list)){
      addWorksheet(wb, vendor_list[i])
      tmp_df <- inventory[grepl(vendor_list[i],inventory$vendor),] %>%
        select(name,ref_smn,lot,exp_date,remaining_qty,ave_pack_import_cost,
               total_inv_value)
      tmp_df <- rename_table(tmp_df,ui_elem)
      writeData(wb, sheet=vendor_list[i], tmp_df)
    }
    saveWorkbook(wb,rp_filename,overwrite = T)
  }
  if (report_type == 'inventoryAuditReport'|
      report_type == 'inventoryOrderReport'){
    # read the form
    orig_file <- config_dict$value[config_dict$name=='report_form_path']
    wb <- loadWorkbook(orig_file)
    
    # read the inventory
    inventoryReport <- update_inventory(config_dict)
    # set all negative number to 0
    inventoryReport <- inventoryReport[inventoryReport$remaining_qty>0,]
    
    # if this is ordering report, group and sum
    if (report_type == 'inventoryOrderReport'){
      inventoryReport <- inventoryReport %>% group_by(prod_code) %>% 
        summarise(total_remaining_qty = sum(remaining_qty)) %>% ungroup
      # merge with prod_info so that we get zero items as well
      inventoryReport <- merge(inventoryReport,product_info %>% 
                                 select(prod_code,type),all.y=T)
    }
    #recover human-readble info
    inventoryReport <- merge(
      inventoryReport, product_info %>% select(
        prod_code,name,vendor,ref_smn,warehouse_id))
    inventoryReport <- merge(
      inventoryReport,warehouse_info %>% select(warehouse_id,warehouse))
    # for order report, use sales_summary
    sales_summary <- get_sales_summary(config_dict)
    inventoryReport <- merge(inventoryReport,sales_summary %>% select(
      prod_code,ave_mth_sale), all.x=T)
    
    # add ordering unit
    ordering_unit <- get_ordering_unit(packaging)
    inventoryReport <- merge(inventoryReport, ordering_unit, all.x=T)
    # select the appropriate column
    if (report_type == 'inventoryOrderReport'){
      inventoryReport <- inventoryReport %>%
        select(vendor, ref_smn, warehouse, name, total_remaining_qty, 
               unit, ave_mth_sale)
      inventoryReport$mth_supply_left <- inventoryReport$total_remaining_qty / 
        inventoryReport$ave_mth_sale
    }else{
      inventoryReport <- inventoryReport %>%
        select(name,vendor,ref_smn,remaining_qty, unit,
               lot,exp_date,warehouse)
    }
    rp_data <- inventoryReport
    # # formatting the data frame
    # for (i in (1:length(names(inventoryReport)))){
    #   oldname <- names(inventoryReport)[i]
    #   # print(oldname)
    #   if (length(ui_elem$actual[ui_elem$label==oldname])==1){
    #     names(inventoryReport)[names(inventoryReport)==oldname] <- 
    #       ui_elem$actual[ui_elem$label==oldname]
    #   }
    # }
    # write data to destination file then open file
    writeData(wb, 1, rp_data, startRow=5, startCol=1)
    saveWorkbook(wb,rp_filename,overwrite = T)
  }
  return(rp_data)
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
  if (nrow(tmp[duplicated(tmp %>% select(pxk_num,prod_code,lot)),])>0){
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
    customer_name, sale_date, pxk_num, name, ref_smn, unit, qty, unit_price,
    unit_import_cost, unit_profit, total_profit, profit_margin)
  return(tmp)
}

# a report comprise of report_name, from_date, to_date and rp_data
write_report_data <- function(
  rp_form,rp_filename,report_name,report_info,rp_data,from_date,to_date){
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
  return(1)
}

create_excel_report <- function(config_dict,report_type,from_date,to_date,
                                rp_filename){
  conn <- db_open(config_dict)
  report_info <- dbReadTable(conn,"output_info")
  report_info <- report_info[report_info$type=='report_output',]
  ui_elem <- dbReadTable(conn,"localisation")
  ui_elem <- ui_elem[
    ui_elem$app_lang == config_dict$value[config_dict$name=='app_lang'],]
  ui_elem <- ui_elem[ui_elem$group=='ui_elements',]
  dbDisconnect(conn)
  
  # get the input,output file, report name
  report_name <- ui_elem$actual[ui_elem$label==report_type]
  rp_form <- config_dict$value[config_dict$name=='report_form_path']

  # get the report data
  if (report_type == 'sale_profit_report'){
    output_rp <- get_sales_report(config_dict,from_date,to_date)
    output_rp <- clean_duplicates(
      output_rp,col_list = c("customer_name", "sale_date", "pxk_num"))
  }
  if (report_type == 'inv_exp_date_report'){
    rp_data <- get_inv_exp_report(config_dict)
  }
  write_report_data(
    rp_form,rp_filename,report_name,report_info,rp_data,from_date,to_date)
  
  return(rp_data)
}

# data for inventory sorted by date
get_inv_exp_report <- function(config_dict){
  output_rp <- update_inventory(config_dict)
  output_rp$remaining_days <- output_rp$intexp_date-Sys.time()
  output_rp$label[output_rp$remaining_days<180] <- 'less_than_6mth'
  output_rp$label[output_rp$remaining_days<90] <- 'less_than_3mth'
  output_rp <- output_rp[order(output_rp$intexp_date),]
  output_rp <- merge(output_rp,ui_elem,all.x=T)
  output_rp$note <- output_rp$actual
  output_rp <- output_rp %>% select(name,vendor,ref_smn,remaining_qty,
                                    exp_date,note)
  return(output_rp)
}