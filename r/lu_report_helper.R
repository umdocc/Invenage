# first stage in reporting is to get everything into single table format
# using create_lookup_tbl
create_lookup_tbl <- function(
  input,table_name,config_dict,translate_colname=TRUE){
  lu_to_date=input$lur_to_date
  if (table_name == 'sale_profit_report'){
    from_date <- input$from_date # from_date and to_date depends on rp type
    rp_data <- get_sales_report(config_dict,from_date,lu_to_date)
    lookup_tbl_output <- clean_duplicates(
      rp_data,col_list = c("customer_name", "sale_date", "pxk_num"))
  }
  if (table_name == 'inv_exp_date_report'){
    rp_data <- update_inventory(config_dict, to_date = lu_to_date)
    rp_data$remaining_days <- rp_data$intexp_date-Sys.time()
    rp_data$label[rp_data$remaining_days<180] <- 'less_than_6mth'
    rp_data$label[rp_data$remaining_days<90] <- 'less_than_3mth'
    rp_data <- rp_data[order(rp_data$intexp_date),]
    rp_data <- merge(rp_data,ui_elem,all.x=T)
    rp_data$note <- rp_data$actual
    lookup_tbl_output <- rp_data %>% select(vendor,comm_name,ref_smn,remaining_qty,
                                  exp_date,note,prod_code)
  }
  if (table_name == 'inv_value_report'){
    # refresh information
    lookup_tbl_output <- update_inventory(config_dict, to_date = lu_to_date)
  }
  if (table_name == 'inv_order_report'){
    lookup_tbl_output <- create_inv_order_rp(config_dict,tender_include=T)
  }
  if (table_name == 'inv_audit_report'){
    # read the inventory
    rp_data <- update_inventory(config_dict, to_date = lu_to_date)
    # set all negative number to 0
    rp_data <- rp_data[rp_data$remaining_qty>0,]
    # add ordering unit
    ordering_unit <- get_ordering_unit(packaging)
    rp_data <- merge(rp_data,ordering_unit,all.x=T)
    rp_data <- rp_data %>% 
      select(vendor,comm_name,ref_smn,lot,exp_date,remaining_qty,unit,prod_code)
    lookup_tbl_output <- round_report_col(rp_data,'remaining_qty')
  }
  if (table_name=='inventory'){
    lookup_tbl_output <- update_inventory(config_dict, to_date = lu_to_date)
    lookup_tbl_output$remaining_qty <- round(
      lookup_tbl_output$remaining_qty, digits=2)
    lookup_tbl_output$unit <- NULL
    ordering_unit <- get_ordering_unit(packaging)
    lookup_tbl_output <- merge(
      lookup_tbl_output, ordering_unit,all.x=T)
    lookup_tbl_output <- merge(
      lookup_tbl_output, product_info %>% 
        select(prod_code,comm_name,packaging_str,vendor,ref_smn,warehouse_id), 
      all.x = T)
    lookup_tbl_output <- merge(lookup_tbl_output,warehouse_info %>% 
                                 select(warehouse,warehouse_id))
    lookup_tbl_output <- lookup_tbl_output %>% 
      select(comm_name, vendor, ref_smn, lot, exp_date, remaining_qty, unit,
                                                      warehouse)
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
    # filter on to_date
    lookup_tbl_output <- lookup_tbl_output[
      lookup_tbl_output$sale_datetime<=as.Date(lu_to_date,format='%Y-%m-%d'),]
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
      select(name,unit,qty,lot,exp_date,po_name,actual_unit_cost,delivery_date)
    lookup_tbl_output <- lookup_tbl_output[
      lookup_tbl_output$delivery_date<=as.Date(lu_to_date,format='%Y-%m-%d'),]
  }
  # format the table
  if (translate_colname){
    lookup_tbl_output <- translate_tbl_column(lookup_tbl_output,ui_elem)
  }
  
  return(lookup_tbl_output)
}

create_full_report <- function(input,report_filename='lu_tbl.xlsx'){
  # first create the raw table
  table_label <- uistr_to_label(input$lu_report_tbl_selector)
  # print(table_label)
  lu_report_output <- create_lookup_tbl(
    input,table_name=table_label,config_dict,translate_colname=F)
  # use one single output file location for all reports
  dest_path <- file.path(config_dict$value[config_dict$name=='report_out_path'],
                         report_filename)
  
  # these tables will need more processing, so use separate function to write them
  # which also return the full dest_path so that we can open later
  if(table_label=='inv_value_report'){
    write_inv_value_rp(lu_report_output,dest_path)
  }else{
    # these tables can be written directly to excel and only need col translate
    lu_report_output <- translate_tbl_column(lu_report_output,ui_elem)
    write.xlsx(lu_report_output, dest_path,row.names=F)
  }
  
  # open the file after creation
  system2('open',dest_path,timeout = 2)
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

write_inv_value_rp <- function(lu_report_output,rp_filename){

  # gather all informations
  summary_sheet_name <- ui_elem$actual[ui_elem$label=='summary']
  missing_price_sheet_name <- ui_elem$actual[ui_elem$label=='missing_price']
  totalNSXcostName <- ui_elem$actual[ui_elem$label=='total_inv_value']
  val_by_vendor <- lu_report_output %>% group_by(vendor) %>% summarise(
    total_inv_value = sum(total_inv_value,na.rm=T), .groups='drop') %>% ungroup
  val_by_vendor <- val_by_vendor[!is.na(val_by_vendor$vendor),]
  # get vendor list here
  vendor_list <- gsub('-.*$','',val_by_vendor$vendor)
  vendor_list <- vendor_list[!duplicated(vendor_list)]
  # add total cost, and format the ouput
  tmp <- val_by_vendor[1:2,]
  tmp[1,] <- NA
  tmp$vendor[2] <- ui_elem$actual[ui_elem$label=='total_inv_value']
  tmp$total_inv_value[2] <- sum(val_by_vendor$total_inv_value,na.rm=T)
  val_by_vendor <- rbind(val_by_vendor,tmp)

  val_by_vendor$total_inv_value <- format(
    as.numeric(val_by_vendor$total_inv_value), trim = T, big.mark=",")
  
  wb <- createWorkbook()
  addWorksheet(wb, summary_sheet_name)
  addWorksheet(wb, missing_price_sheet_name)
  # write missing_price
  missing_price <- lu_report_output[is.na(lu_report_output$ave_pack_import_cost),] %>%
    select(prod_code, comm_name, ref_smn)
  missing_price <- translate_tbl_column(missing_price,ui_elem)
  writeData(wb, sheet=missing_price_sheet_name, missing_price)
  # write summary sheet
  output_rp <- translate_tbl_column(val_by_vendor,ui_elem)
  writeData(wb, sheet=summary_sheet_name, output_rp)

  for (i in 1:length(vendor_list)){
    addWorksheet(wb, vendor_list[i])
    tmp_df <- lu_report_output[grepl(vendor_list[i],lu_report_output$vendor),] %>%
      select(comm_name,ref_smn,lot,exp_date,remaining_qty,ave_pack_import_cost,
             total_inv_value)
    tmp_df <- translate_tbl_column(tmp_df,ui_elem)
    writeData(wb, sheet=vendor_list[i], tmp_df)
  }
  saveWorkbook(wb,rp_filename,overwrite = T)
  return(rp_filename)
}

get_sales_report <- function(config_dict, from_date='2019-11-04',
                             to_date = Sys.Date()){
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
      prod_code,comm_name,vendor,ref_smn,warehouse_id))
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

render_lu_report_list <- function(input, iid){renderUI({
  group_label <- report_type[
    report_type$actual==input$lu_report_group_selector,] %>% select(label)
  lu_report_tbl_list <- merge(group_label,ui_elem)[,'actual']
  selectInput(
    inputId = iid,
    label = ui_elem$actual[ui_elem$label=='choose_table'],
    choices = lu_report_tbl_list
  )
})}

create_tender_track <- function(sale_log,customer_id = 1){
  
  # build basic details from sale_log
  full_sale_log <- merge(sale_log,pxk_info)
  tmp <- convert_to_pack(full_sale_log,packaging,'qty','pack_qty')
  
  # read only the active tender
  tmp2 <- merge(tender_detail,tender_info %>% 
                  select(tender_id, customer_id, active, warehouse_id))
  tmp2 <- tmp2[tmp2$active==1,]
  tmp2 <- tmp2[tmp2$warehouse_id!=0,]
  tmp2 <- convert_to_pack(
    tmp2,packaging,'tender_qty','tender_pack_qty')
  
  #remove null tender_id
  tmp <- tmp[!is.na(tmp$tender_id),]
  tmp <- tmp %>% group_by(prod_code,tender_id,customer_id,warehouse_id) %>% 
    summarise(total_sale_pack = sum(pack_qty))
  
  tmp <- merge(tmp2, tmp,
               by = c('prod_code','tender_id','customer_id','warehouse_id'),
               all = T)
  
  # calculate final_rm_pack_qty
  tmp$final_rm_pack_qty <- tmp$final_rm_qty/tmp$units_per_pack
  
  # if total_sale_pack/tender_pack_qty is na we use 0
  tmp$total_sale_pack[is.na(tmp$total_sale_pack)] <- 0
  tmp$tender_pack_qty[is.na(tmp$tender_pack_qty)] <- 0
  tmp$tender_pack_remain <- tmp$tender_pack_qty-tmp$total_sale_pack
  
  return(tmp)
}

# update items with tender_id = 0 to correct tender_id using logic
update_tender_id <- function(config_dict, exclude_code=5){
  
  # rebuild sale_log, fill na with default tender_id = 0
  full_sale_log <- merge(sale_log,pxk_info)
  full_sale_log$tender_id[is.na(full_sale_log$tender_id)] <- 0
  full_sale_log <- convert_to_pack(
    full_sale_log, packaging, 'qty', 'sale_qty_pack')
  full_sale_log$new_tender_id <- full_sale_log$tender_id
  
  # we are not interested in items with payment_code = 5 as it is previously 
  # returned
  full_sale_log <- full_sale_log[full_sale_log$payment_code!=5,]
  
  # sort by sale_datetime
  full_sale_log <- full_sale_log[order(full_sale_log$sale_datetime),]
  
  # load the tender track, filter using tender customer only
  tender_track <- create_tender_track(sale_log)
  tender_track$active <- NULL
  tender_customer <- tender_info %>% select(customer_id,active) %>% 
    filter(customer_id!=0) %>% filter(!duplicated(customer_id))
  tmp <- merge(tender_track,tender_customer,by='customer_id',all.x=T)
  tmp <- tmp[!is.na(tmp$active),]
  
  # filter tender_track,
  tmp$final_rm_pack_qty[is.na(tmp$final_rm_pack_qty)] <- 0
  tmp <- tmp[
    (tmp$tender_pack_remain - tmp$final_rm_pack_qty) >0, ]
  tmp <- tmp[order(tmp$tender_id),]
  
  # run through the tender_track, then check items in sale log, tender_id
  for (i in 1:nrow(tmp)){
    prod_code_2edit <- tmp$prod_code[i]
    customer_2edit <- tmp$customer_id[i]
    amount_cap <- tmp$tender_pack_remain[i]-tmp$final_rm_pack_qty[i]
    test <- full_sale_log[
      full_sale_log$prod_code==prod_code_2edit & 
        full_sale_log$customer_id==customer_2edit & 
        full_sale_log$new_tender_id==0,]
    while ((nrow(test)>0) & (amount_cap>0)){
      print(i)
      print(paste('edit item',test$prod_code[1],'in pxk',test$pxk_num[1]))
      # edit the 1st item full_sale_log
      full_sale_log$new_tender_id[
        full_sale_log$prod_code==prod_code_2edit &
          full_sale_log$customer_id==customer_2edit &
          full_sale_log$new_tender_id==0][1] <- tmp$tender_id[i]
      test <- full_sale_log[ # rebuild the test data
        full_sale_log$prod_code==prod_code_2edit &
          full_sale_log$customer_id==customer_2edit &
          full_sale_log$new_tender_id==0,]
      amount_cap <- amount_cap - full_sale_log$sale_qty_pack[
        full_sale_log$prod_code==prod_code_2edit &
          full_sale_log$customer_id==customer_2edit &
          full_sale_log$new_tender_id==0][1]
    }
  }
  
  record_2edit <- full_sale_log[
    full_sale_log$new_tender_id!=full_sale_log$tender_id,]
  if (nrow(record_2edit)>0){
    # writing to database
    conn <- db_open(config_dict)
    for (i in 1:nrow(record_2edit)){
      query <- paste0(
        "update sale_log set tender_id = ", record_2edit$new_tender_id[i],
        " where pxk_num = ", record_2edit$pxk_num[i], " and prod_code like '",
        record_2edit$prod_code[i],"' and stt = ",record_2edit$stt[i])
      # print(query)
      dbExecute(conn,query)
    }
  }
  dbDisconnect(conn)
  
}

# this function will create a report for inventory order
create_inv_order_rp <- function(config_dict,tender_include=T,vendor_id=0){
  rp_data <- update_inventory(config_dict)
  rp_data <- rp_data %>% group_by(prod_code) %>%
    summarise(remaining_qty = sum(remaining_qty),.groups='drop')
  
  # merge with prod_info so that we get zero items as well
  rp_data <- merge(rp_data,product_info %>% filter(active==T) %>%
                     select(prod_code,type,std_mth_stock,min_mth_stock),all.y=T)
  rp_data$remaining_qty[is.na(rp_data$remaining_qty)] <- 0
  
  # add sales_summary
  sales_summary <- get_sales_summary(config_dict)
  rp_data <- merge(rp_data,sales_summary %>% select(
    prod_code,ave_mth_sale), all.x=T)
  
  #add other info, and round up numbers
  rp_data <- add_inv_report_info(rp_data)
  rp_data <- round_report_col(
    rp_data, col_list = c('ave_mth_sale', 'remaining_qty'), decimal = 2)
  
  # finalise all the required columns
  rp_data <- rp_data %>%
    select(vendor, comm_name, ref_smn, warehouse, remaining_qty,
           unit, ave_mth_sale,prod_code,std_mth_stock,min_mth_stock)
  
  # include final tender remaining if tender_include = T
  if (tender_include){
    # read and summarise the tender track, group by tender_id
    tender_track <- create_tender_track(sale_log)
    tender_track <- tender_track %>% group_by(prod_code,tender_id) %>% 
      summarise(total_tender_pack_rem = sum(tender_pack_remain),.groups='drop')
    
    # loop through tender track and merge with rp_data
    for (i in unique(tender_track$tender_id)){
      tmp <- tender_track[tender_track$tender_id==i,]
      tmp[,paste0('tender',i,'_pack_remain')] <- tmp$total_tender_pack_rem
      tmp$tender_id <- NULL;tmp$total_tender_pack_rem <- NULL
      rp_data <- merge(rp_data,tmp,all.x=T)
    }
  }
  
  # finally clean up columns with no data
  for (col_name in names(rp_data)){
    if(all(is.na(rp_data[[col_name]]))){
      rp_data[[col_name]] <- NULL
    }
  }
  
  # add suggested order, but only suggest items less than min_mon_stock
  rp_data$suggested_order <- round(rp_data$ave_mth_sale*rp_data$std_mth_stock-
    rp_data$remaining_qty,digits=0)
  rp_data$mth_stock_left <- rp_data$remaining_qty/rp_data$ave_mth_sale
  rp_data$suggested_order[rp_data$mth_stock_left>rp_data$min_mth_stock] <- 0
  
  # check the import price internally and 
  
  return(rp_data)
}

