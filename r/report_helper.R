# first stage in reporting is to get everything into single table format
# using create_lookup_tbl
create_lookup_tbl <- function(input,table_name,config_dict,translate_colname=TRUE){
  if (table_name == 'sale_profit_report'){
    from_date <- input$from_date # from_date and to_date depends on rp type
    to_date <- input$to_date
    rp_data <- get_sales_report(config_dict,from_date,to_date)
    lookup_tbl_output <- clean_duplicates(
      rp_data,col_list = c("customer_name", "sale_date", "pxk_num"))
  }
  if (table_name == 'inv_exp_date_report'){
    rp_data <- update_inventory(config_dict)
    rp_data$remaining_days <- rp_data$intexp_date-Sys.time()
    rp_data$label[rp_data$remaining_days<180] <- 'less_than_6mth'
    rp_data$label[rp_data$remaining_days<90] <- 'less_than_3mth'
    rp_data <- rp_data[order(rp_data$intexp_date),]
    rp_data <- merge(rp_data,ui_elem,all.x=T)
    rp_data$note <- rp_data$actual
    lookup_tbl_output <- rp_data %>% select(vendor,name,ref_smn,remaining_qty,
                                  exp_date,note,prod_code)
  }
  if (table_name == 'inv_value_report'){
    # refresh information
    lookup_tbl_output <- update_inventory(config_dict)
  }
  if (table_name == 'inv_order_report'){
    lookup_tbl_output <- create_inv_order_rp(config_dict,tender_include=T)
  }
  if (table_name == 'inv_audit_report'){
    # read the inventory
    rp_data <- update_inventory(config_dict)
    # set all negative number to 0
    rp_data <- rp_data[rp_data$remaining_qty>0,]
    # add ordering unit
    ordering_unit <- get_ordering_unit(packaging)
    rp_data <- merge(rp_data,ordering_unit,all.x=T)
    rp_data <- rp_data %>% 
      select(vendor,name,ref_smn,lot,exp_date,remaining_qty,unit,prod_code)
    lookup_tbl_output <- round_report_col(rp_data,'remaining_qty')
  }
  if (table_name=='inventory'){
    lookup_tbl_output <- update_inventory(config_dict)
    lookup_tbl_output$remaining_qty <- round(
      lookup_tbl_output$remaining_qty, digits=2)
    lookup_tbl_output$unit <- NULL
    ordering_unit <- get_ordering_unit(packaging)
    lookup_tbl_output <- merge(
      lookup_tbl_output, ordering_unit,all.x=T)
    lookup_tbl_output <- merge(
      lookup_tbl_output, product_info %>% 
        select(prod_code,name,vendor,ref_smn), all.x = T) %>%
      select(name, vendor, ref_smn, lot, exp_date, remaining_qty, unit)
  }
  # query on simple table
  if (table_name=='product_info'){
    lookup_tbl_output <- product_info %>% 
      select(prod_code, comm_name, vendor, ref_smn, name)
  }
  if (table_name=='import_price'){
    lookup_tbl_output <- merge(import_price,product_info %>% 
                                 select(prod_code,name,vendor,ref_smn))
    lookup_tbl_output <- merge(lookup_tbl_output,currency)
    lookup_tbl_output <- lookup_tbl_output %>% 
      select(name,vendor,ref_smn,import_price,currency,min_order)
  }
  if (table_name=='sale_log'){
    lookup_tbl_output <- merge(sale_log, product_info %>% select(
      prod_code,name,vendor,ref_smn))
    lookup_tbl_output <- merge(lookup_tbl_output,pxk_info %>% select(
      pxk_num,customer_id,sale_datetime))
    lookup_tbl_output$customer_id <- as.numeric(
      lookup_tbl_output$customer_id)
    lookup_tbl_output <- merge(
      lookup_tbl_output,customer_info %>% select(customer_id,customer_name))
    lookup_tbl_output$sale_date <- gsub(
      ' .*$', '', lookup_tbl_output$sale_datetime)
    lookup_tbl_output <- lookup_tbl_output %>%
      select(pxk_num, sale_date, customer_name, name, ref_smn, qty, unit, 
             lot, note)
  }
  if (table_name=='import_log'){
    lookup_tbl_output <- merge(import_log, product_info%>% select(
      prod_code,name)) %>% 
      select(name,unit,qty,lot,exp_date,po_name,actual_unit_cost)
    
  }
  # format the table
  if (translate_colname){
    lookup_tbl_output <- translate_tbl_column(lookup_tbl_output,ui_elem)
  }
  
  return(lookup_tbl_output)
}

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

build_rp_data <- function(report_type, input, translate=TRUE, prodcode.rm=TRUE){



  # options listed in functions
  if (translate){ # default TRUE : translate the column name
    rp_data <- translate_tbl_column(rp_data,ui_elem)
  }
  if (prodcode.rm){ # default TRUE : remove prod_code column
    rp_data$prod_code <- NULL
  }
  
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

# create a tender tracking table


# this funcion expect a table with prod_code, qty, vendor_id columns, it then
# add the import price
get_import_price <- function(po_data, stringQty = 'qty'){
  req_col <- c('prod_code','vendor_id',stringQty)
  for (i in req_col){
    if (!(i %in% names(po_data))){ stop(paste('error', i, 'not found'))}
  }
  
  # save the name of qty column, then rename it to qty
  oldname <- stringQty
  names(po_data)[names(po_data)==stringQty] <- 'qty'
  
  # load all import price
  po_data <- merge(
    po_data, import_price %>% 
      select(prod_code, import_price, vendor_id, currency_code, min_order),
    all.x=T)
  # if an item has min_order = NA, set it to 1
  po_data$min_order[is.na(po_data$min_order)] <- 1
  #calculate qty/min_order ratio, then choose one with min of this ratio
  po_data$order_ratio <- po_data$qty/po_data$min_order
  po_data <- po_data %>%group_by(prod_code) %>% 
    mutate(min_ratio = min(order_ratio)) %>% ungroup
  po_data <- po_data[po_data$order_ratio==po_data$min_ratio,]
  
  # check for prod_code duplications before returning results
  if (nrow(po_data[duplicated(po_data %>% select(prod_code,qty)),])>0){
    stop('po_data contains duplicated line')
  }else{
    # restore the name
    names(po_data)[names(po_data)=='qty'] <- stringQty
    # remove all useless columns
    removed_col <- c('min_order','currency_code', 'vendor_id', 'vendor', 
                     'order_ratio','min_ratio','last_updated', 'lot','exp_date',
                     'actual_unit_cost','note')
    for (i in removed_col){ po_data[,i] <- NULL }
    if ('stt' %in% names(po_data)){
      po_data <- po_data[order(po_data$stt),]
    }
    return(po_data)
  }
}