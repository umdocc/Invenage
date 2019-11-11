# All functions shared by the UIs
# call all required packages
# ------------------------------- Base functions -------------------------------
# the db_open create the appropriate connection for Invenage
db_open <- function(config_dict){
  db_type <- config_dict$value[config_dict$name=='db_type']
  if (db_type == 'SQLite'){
    database_path <- config_dict$value[config_dict$name=='db_file']
    sqlite.driver <- dbDriver("SQLite")
    conn <- dbConnect(sqlite.driver, dbname = database_path)
    return(conn)
  }
}

# get current_pxk is a function that use the database connection object conn
get_current_pxk <- function(cofig_dict){
  conn <- db_open(config_dict)
  pxk_num_list <- dbGetQuery(conn,'select pxk_num from pxk_info')
  current_pxk <- dbGetQuery(conn,
                    'select pxk_num from pxk_info where completed = 0')
  dbDisconnect(conn)
  if (nrow(current_pxk)>0){
    newPXK = current_pxk$pxk_num[1]
  }else{
    currentDate <- strftime(Sys.time(),'%d%m%y')
    i <- 1;newPXKNum <- F
    while (!newPXKNum){
      tmp_num <- as.numeric(paste0(strftime(Sys.time(),'%d%m%y'),
                                  sprintf("%02d",i)))
      # print(tmpNum)
      if (length(pxk_num_list[pxk_num_list$pxk_num==tmp_num,'pxk_num'])==0){
        newPXK <- tmp_num
        newPXKNum <- T
      }else{
        i <- i+1
      }
    }
  }
  return(newPXK)
}

# function to rebuild the Inventory table from import_log and sale_log
# if moreThanZero ==T, it will only return items with positive stock
update_inventory <- function(config_dict, pos_item=TRUE){
  
  conn <- db_open(config_dict)
  import_log <- dbReadTable(conn,"import_log")
  sale_log <- dbReadTable(conn,"sale_log")
  pxk_info <- dbReadTable(conn,"pxk_info")
  product_info <- dbReadTable(conn,'product_info')
  dbDisconnect(conn)
  
  tmp <- import_log %>% select(prod_code,unit,qty,lot,exp_date,warehouse_id)
  tmp <- convert_to_pack(tmp,packaging,'qty','importQty')
  tmp <- tmp %>% group_by(prod_code,unit,lot,warehouse_id) %>% 
    summarise(totalImportQty = sum(importQty)) %>% ungroup()
  tmp2 <- sale_log %>% select(prod_code,unit,qty,lot,warehouse_id)
  # for sale_log we need to merge with warehouse_id
    tmp2 <- convert_to_pack(tmp2,packaging,'qty','saleQty')
  tmp2 <- tmp2 %>% group_by(prod_code,unit,lot,warehouse_id) %>% 
    summarise(totalSaleQty = sum(saleQty)) %>% ungroup()
  totalInventory <- merge(tmp,tmp2,all=T,
                          by=c('prod_code','unit','lot','warehouse_id'))
  totalInventory$totalSaleQty[is.na(totalInventory$totalSaleQty)] <- 0
  totalInventory$totalImportQty[is.na(totalInventory$totalImportQty)] <- 0
  totalInventory$remaining_qty <- totalInventory$totalImportQty - 
    totalInventory$totalSaleQty
  
  # keep only the available items
  if (pos_item){
    threshold <- 0.001
    totalInventory <- totalInventory[
      totalInventory$remaining_qty>threshold,] %>% distinct()
  }
  # recover the exp_date
  exp_dateData <- import_log[!duplicated(import_log[c('prod_code','lot')]),] %>% 
    select(prod_code,lot,exp_date) %>% distinct()
  
  # merge, distinct and remove NA
  totalInventory <- merge(totalInventory,exp_dateData,all.x=T) %>% distinct()
  inventory <- totalInventory[!is.na(totalInventory$prod_code),]
  
  # calculate the intexp_date, which is the exp_date in standard format
  inventory$exp_date <- gsub('/','-',inventory$exp_date)
  inventory$exp_date <- gsub(' .*$','',inventory$exp_date)
  inventory$intexp_date <- parse_date_time(
    inventory$exp_date,c('%Y-%m','%m-%Y','%d-%m-%Y','%Y-%m-%d'))
  
  # recover static information
  product_info <- product_info %>% select(prod_code,name,ref_smn,vendor)
  inventory <- merge(inventory,product_info,all.x=T)
  ave_import_cost <- get_est_import_cost(
    import_log, algorithm='weighted_average')
  
  inventory <- merge(inventory,ave_import_cost,all.x=T)
  inventory$total_inv_value <-
    inventory$ave_pack_import_cost*inventory$remaining_qty
  
  # remove NAs
  inventory <- inventory[!is.na(inventory$prod_code),]
  return(inventory)
}

# convertToPack is a critical function
convert_to_pack <- function(inputDF,packaging,stringSL,packString){
  inputDF <- merge(
    inputDF,packaging %>% select(prod_code,unit,units_per_pack),all.x=T)
  # check integrity
  if(nrow(inputDF[is.na(inputDF$units_per_pack),])>0){
    print(inputDF[is.na(inputDF$units_per_pack),])
    stop('inputDF contains unrecognised packaging')
  }
  inputDF[[packString]] <- inputDF[[stringSL]]/inputDF$units_per_pack
  # clean up
  inputDF$unit <- 'pack'
  inputDF[[stringSL]] <- NULL
  # inputDF$units_per_pack <- NULL
  return(inputDF)
}

# function to select customer using the database to look at PXK
get_cust_list <- function(config_dict){
  conn <- db_open(config_dict)
  pxk_info <- dbReadTable(conn,"pxk_info")
  customer_info <- dbReadTable(conn,"customer_info")
  dbDisconnect(conn)
  current_pxk <- pxk_info[pxk_info$completed==0,'pxk_num']
  # if current_pxk has completion code then we force customer_name
  if (length(current_pxk)>0){
    current_cust_id <- pxk_info$customer_id[pxk_info$pxk_num==current_pxk]
    cust_choice <- customer_info$customer_name[
      customer_info$customer_id==current_cust_id]
  }else{
    cust_choice <- customer_info$customer_name
  }
  return(cust_choice)
}

# the getAvailablelot function get a list of available lot, it returns a vector
# if sortType ='fifo', the earliest exp_date will be on top
get_avail_lot <- function(current_prod_code,config_dict,sort_type='fifo'){
  inventory <- update_inventory(config_dict)
  if (sort_type == 'fifo'){
    avail_lot <- inventory[inventory$prod_code==current_prod_code,]
    avail_lot <- avail_lot[order(avail_lot$intexp_date,
                                       na.last = F, # put NA lot first
                                       decreasing = F),] #lowest exp_date first
  }
  avail_lot <- avail_lot$lot
  return(avail_lot)
}

# function to rebuild the productInfo HTML string
build_prod_info <- function(config_dict,input){
  conn <- db_open(config_dict)
  product_info <- dbReadTable(conn,"product_info")
  dbDisconnect(conn)
  inventory <- update_inventory(config_dict)
  
  current_select <- product_info[product_info$name==input$prod_name_selector,]

  total_available <- inventory[inventory$prod_code == current_select$prod_code &
                            inventory$lot == input$lot_selector, 'remaining_qty']
  current_exp_date <- inventory[
    inventory$prod_code == current_select$prod_code &
                      inventory$lot == input$lot_selector, 'exp_date']
  packaging_str <- packaging[
    packaging$prod_code == current_select$prod_code &
      packaging$unit == input$unit_selector,]
  packaging_str <- paste0(packaging_str$units_per_pack[1],
                          packaging_str$unit[1],'/pack')
  return(paste("REF: ",current_select$ref_smn,'<br/>',
               ui_elem$actual[ui_elem$label=='prod_code'],':',
               current_select$prod_code, '<br/>',
               ui_elem$actual[ui_elem$label=='vendor'],':',
               current_select$vendor, '<br/>',
               ui_elem$actual[ui_elem$label=='exp_date'],':',
               current_exp_date, '<br/>',
               ui_elem$actual[ui_elem$label=='total_available'],':',
               total_available, '<br/>',
               ui_elem$actual[ui_elem$label=='packaging_str'],
               ':',packaging_str)
  )
}

get_current_po_info <- function(config_dict){
  file_list <- data.frame(
    full_path = list.files(config_dict$value[config_dict$name=='po_path'],
                           recursive = T,full.name=T),stringsAsFactors = F)
  file_list$basename <- basename(file_list$full_path)
  file_list$dirname <- dirname(file_list$full_path)
  
  # keep only relevant files
  grep_str <- paste0(config_dict$value[config_dict$name=='po_file_include'],
                     '|','draft|Draft')
  file_list <- file_list[grepl(grep_str,file_list$full_path),]
  exclude_str <- gsub(';','|',
                      config_dict$value[config_dict$name=='po_path_exclude'])
  file_list <- file_list[!grepl(exclude_str,file_list$full_path),]
}

# get_import_price is a function to auto get the import price for a po
# the algorithm sort by minimum order amount
get_import_price <- function(po_data,import_price){
  tmp <- merge(import_price, po_data %>% select(prod_code,qty),
               all.x=T)
  tmp <- tmp[!is.na(tmp$qty),]
  tmp$min_order[is.na(tmp$min_order)] <- 1
  # get only rows where qty is more than min order
  tmp <- tmp[(tmp$qty/tmp$min_order)>1,] %>% select(prod_code,import_price,
                                                    min_order,qty)
  tmp <- tmp %>% group_by(prod_code) %>% mutate(min_price=min(import_price)) %>%
    ungroup()
  tmp <- tmp[tmp$min_price==tmp$import_price,]
  return(tmp)
}

# this function build the po from draft using Draft PO Name
build_po_from_draft <- function(draft_name,config_dict){
  # read tables from database
  conn <- db_open(config_dict)
  import_price <- dbReadTable(conn,'import_price')
  product_info <- dbReadTable(conn,'product_info')
  dbDisconnect(conn)
  
  # since this is for po, we need to use currency_code >1 for generating prices
  import_price <- import_price[import_price$currency_code>1,]
  
  po_list <- get_current_po_info(config_dict)
  po_list <- po_list[po_list$basename==draft_name,]
  #read the draft
  po_data <- read.xlsx(po_list$full_path)
  
  # attach prod_code
  po_data <- merge(po_data,product_info,all.x=T)
  # attach unit_price
  po_data <- get_import_price(po_data,import_price)
  #re-attach all data
  po_data <- merge(po_data,product_info %>% select(prod_code,name,ref_smn),
                   by='prod_code',all.x=T)
  # build other info
  po_data$no <- 1:nrow(po_data)
  
  # prepare data for writing
  col_list <- unlist( # list of columns to write
    strsplit(config_dict$value[config_dict$name=='po_draft_cols'],';'))
  po_data <- po_data[,col_list]
  
  # getting total line
  po_total <- po_data[1,]; po_total$no <- 0; po_total$name <- 'Total'
  po_total$ref_smn <- ''; po_total$qty <- ''
  po_total$import_price <- sum(po_data$qty*po_data$import_price)
  po_data <- rbind(po_data,po_total)
  
  po_form <- file.path(config_dict$value[config_dict$name=='form_path'],
                       'po_form.xlsx')
  data_start_row <- config_dict$value[config_dict$name=='po_data_start_row']
  data_start_col <- config_dict$value[config_dict$name=='po_data_start_col']
  po_base_name <- gsub('Draft|draft',
                       paste0('PO.',gsub('^.*/','',po_list$dirname)),
                       po_list$basename)
  dest_file <- file.path(po_list$dirname,po_base_name)
  wb <- loadWorkbook(po_form)
  writeData(wb, sheet=1, po_data, startRow = data_start_row,
            startCol = data_start_col,colNames = F)
  
  saveWorkbook(wb,dest_file,overwrite = T)
  
}

# function to build estimated import cost from import_log
get_est_import_cost <- function(import_log, algorithm='weighted_average'){
  if (algorithm=='weighted_average'){
    import_log <- convert_to_pack(import_log,packaging,stringSL='qty',
                               packString = 'pack_qty')
    import_log$pack_import_cost <-
      import_log$actual_unit_cost*import_log$units_per_pack
    import_log$total_import_cost <-
      import_log$pack_import_cost*import_log$pack_qty
    tmp <- import_log %>% group_by(prod_code,lot) %>%
      summarise(total_pack = sum(pack_qty),
                sum_import_cost = sum(total_import_cost)) %>%
      ungroup
    tmp$ave_pack_import_cost <- tmp$sum_import_cost/tmp$total_pack
    tmp <- tmp %>% select(prod_code,lot,ave_pack_import_cost)
    return(tmp)
  }
}

# the rename table takes a table and rename the column based on 
# the localisation table (ui_elem)
rename_table <- function(tmp_df,ui_elem){
  oldnames = data.frame(stt = 1:length(tmp_df), label = names(tmp_df))
  rename_dict <- merge(oldnames,ui_elem,all.x=T)
  rename_dict <- rename_dict[order(rename_dict$stt),]
  names(tmp_df) <- rename_dict$actual
  return(tmp_df)
}

# roll back x months from current month, not counting current month,
# return the beginning date of the rolled back month
roll_back_date <- function(rolling_mth){
  beginDate <- as.Date(as.character(cut(Sys.Date(), "month")),'%Y-%m-%d')
  backDate <- as.Date(as.character(cut(beginDate - 28*rolling_mth,'month')))
  return(backDate)
}

# create_report function
create_report <- function(report_type,config_dict,input){
  ui_elem <- localisation[localisation$group=='ui_elements',]
  
  # get the input,output file
  rp_form <- config_dict$value[config_dict$name=='report_form_path']
  rp_file <- file.path(
    config_dict$value[config_dict$name=='report_out_path'], paste0(
      config_dict$value[config_dict$name=='company_name'], '.',
      ui_elem$actual[ui_elem$label==report_type],'.',
      format(Sys.Date(),config_dict$value[config_dict$name=='date_format']),
      '.xlsx') )
  
  if (report_type == 'sale_profit_report'){
    from_date <- input$from_date
    to_date <- input$to_date
    wb <- loadWorkbook(rp_form)
    output_rp <- get_sales_report(config_dict,from_date,to_date)
    output_rp <- clean_duplicates(
      output_rp,col_list = c("customer_name", "sale_date", "pxk_num"))
    output_rp <- format_output_tbl(output_rp,ui_elem)
    
    #writing data
    writeData(wb,sheet=1,from_date, startRow = 2, startCol = 2)
    writeData(wb,sheet=1,to_date, startRow = 3, startCol = 2)
    writeData(wb,sheet=1,output_rp, startRow = 5)
    saveWorkbook(wb,rp_file,overwrite = T)
  }
  
  if (report_type == 'inv_exp_date_report'){
    output_rp <- update_inventory(config_dict)
    output_rp$remaining_days <- output_rp$intexp_date-Sys.time()
    output_rp$label[output_rp$remaining_days<180] <- 'less_than_6mth'
    output_rp$label[output_rp$remaining_days<90] <- 'less_than_3mth'
    output_rp <- output_rp[order(output_rp$intexp_date),]
    output_rp <- merge(output_rp,ui_elem,all.x=T)
    output_rp$note <- output_rp$actual
    output_rp <- output_rp %>% select(name,vendor,ref_smn,remaining_qty,
                                      exp_date,note)
    wb <- createWorkbook()
    addWorksheet(wb, 'Sheet1')
    writeData(wb,sheet='Sheet1',output_rp)
    saveWorkbook(wb,rp_file,overwrite = T)
  }
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
    
    for (i in 1:length(vendor_list)){
      addWorksheet(wb, vendor_list[i])
      tmp_df <- inventory[grepl(vendor_list[i],inventory$vendor),] %>%
        select(name,ref_smn,lot,exp_date,remaining_qty,ave_pack_import_cost,
               total_inv_value)
      tmp_df <- rename_table(tmp_df,ui_elem)
      writeData(wb, sheet=vendor_list[i], tmp_df)
    }
    saveWorkbook(wb,rp_file,overwrite = T)
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
    # select the appropriate column
    if (report_type == 'inventoryOrderReport'){
      inventoryReport <- inventoryReport %>%
        select(name,vendor,ref_smn,total_remaining_qty,warehouse,ave_mth_sale)
    }else{
      inventoryReport <- inventoryReport %>%
        select(name,vendor,ref_smn,remaining_qty,
               lot,exp_date,warehouse)
    }
    
    # write data to destination file then open file
    writeData(wb, 1, inventoryReport, startRow=5, startCol=1)
    saveWorkbook(wb,rp_file,overwrite = T)
  }
  return(rp_file)
}

# format the table column name from label to localised output
format_output_tbl <- function(input_dt,ui_elem){
  rename_tbl <- data.frame(label=names(input_dt))
  rename_tbl <- merge(rename_tbl,ui_elem)
  
  # rename using old school method is much safer
  oldnames <- rename_tbl$label
  newnames <- rename_tbl$actual
  for(i in 1:length(oldnames)) names(input_dt)[
    names(input_dt) == oldnames[i]] = newnames[i]
  return(input_dt)
}

create_lookup_tbl <- function(table_name,config_dict,local_name=TRUE){
  if (table_name=='inventory'){
    lookup_tbl_output <- update_inventory(config_dict)
    lookup_tbl_output <- merge(
      lookup_tbl_output, product_info %>% select(prod_code,name,ref_smn),
      all.x = T) %>%
      select(name,ref_smn,lot,exp_date,remaining_qty)
  }else{
    # query on simple table
    if (table_name=='product_info'){
      query <- paste("SELECT prod_code,name,vendor,ref_smn from product_info")
    }
    if (table_name=='import_price'){
      query <- paste("SELECT product_info.name, product_info.vendor,
                        product_info.ref_smn, import_price.import_price,
                        import_price.currency_code,
                        import_price.min_order, import_price.last_updated
                        FROM import_price INNER JOIN product_info
                        ON import_price.prod_code = product_info.prod_code")
    }
    if (table_name=='sale_log'){
      query <- paste("SELECT product_info.name, product_info.vendor,
                        product_info.ref_smn, sale_log.unit,
                        sale_log.unit_price, sale_log.qty,
                        sale_log.lot, sale_log.pxk_num, customer_info.customer_name
                        FROM sale_log INNER JOIN product_info
                        ON sale_log.prod_code = product_info.prod_code
                        INNER JOIN pxk_info
                        ON sale_log.pxk_num = pxk_info.pxk_num
                        INNER JOIN customer_info
                        ON pxk_info.customer_id = customer_info.customer_id"
      )
    }
    if (table_name=='import_log'){
      query <- paste("SELECT product_info.name, product_info.vendor,
                        product_info.ref_smn, import_log.unit,
                        import_log.qty, import_log.po_name,
                        import_log.lot, import_log.exp_date, 
                        import_log.delivery_date
                        FROM import_log INNER JOIN product_info
                        ON import_log.prod_code = product_info.prod_code"
      )
    }
    conn <- db_open(config_dict)
    lookup_tbl_output <- dbGetQuery(conn,query)
    dbDisconnect(conn)
  }
  if (local_name){
    lookup_tbl_output <- format_output_tbl(lookup_tbl_output,ui_elem)
  }
  return(lookup_tbl_output)
}

# get_latest_price is a function to get the last price sold to a customer
get_latest_price <- function(customer_name,prod_name,current_unit,
                             sale_lookup,pxk_info){

  latest_price <- -9999
  # filter through sale_lookup to find price
  tmp <- sale_lookup[sale_lookup$name == prod_name & 
                       sale_lookup$customer_name == customer_name &
                       sale_lookup$unit == current_unit,]
  tmp <- tmp[!is.na(tmp$unit_price),]
  # if we can find something, update latest price
  if (nrow(tmp)>0){
    tmp <- merge(tmp,pxk_info %>% select(pxk_num,sale_datetime))
    if (class(tmp$sale_datetime) == "character"){
      tmp$sale_datetime <- strptime(tmp$sale_datetime,'%Y-%m-%d %H:%M:%S')
      latest_price <- tmp$unit_price[
        tmp$sale_datetime == max(tmp$sale_datetime)]
    }
  }
  return(latest_price)
}

# a function to get the sales summary by prod_code
get_sales_summary <- function(config_dict,max_backdate=365){
  conn <- db_open(config_dict)
  tmp <- dbReadTable(conn,'sale_log')
  packaging <- dbReadTable(conn,'packaging')
  pxk_info <- dbReadTable(conn,'pxk_info')
  dbDisconnect(conn)
  tmp <- convert_to_pack(tmp,packaging,'qty','pack_qty')
  tmp <- merge(tmp, pxk_info %>% select(pxk_num,sale_datetime))
  min_date <- Sys.time() - as.difftime(max_backdate, unit = "days")
  tmp <- tmp[tmp$sale_datetime>min_date,]
  tmp <- tmp %>% group_by(prod_code) %>% summarise(
    oldest_sale_datetime = min(sale_datetime),total_sale_pack=sum(pack_qty)) %>%
    ungroup
  tmp$oldest_sale_datetime <- strptime(tmp$oldest_sale_datetime,
                                       "%Y-%m-%d %H:%M:%S")
  tmp$days_diff <- as.numeric(
    difftime(Sys.time(),tmp$oldest_sale_datetime,units = 'days'))
  tmp$ave_mth_sale <- 30*(tmp$total_sale_pack/tmp$days_diff)
  return(tmp)
}

get_sales_report <- function(config_dict, from_date='2019-11-04',
                             to_date = '2019-11-10'){
  # getting variables ready
  from_date <- strptime(from_date,'%Y-%m-%d')
  to_date <- strptime(to_date,'%Y-%m-%d')
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

# a function to replace duplicated line with NA
clean_duplicates <- function(
  data_df, col_list = c('customer_name','sale_date','pxk_num')){
  i <- nrow(data_df)
  while (i>=2){
    if (data_df[(i-1),col_list]==data_df[i,col_list]){
      data_df[i,col_list] <- NA
    }
    i <- i-1
  }
  return(data_df)
}

# create fifo_sale_log create a sale table with ammended import data using
# a fifo algorithm, it is needed for sale report accuracy
create_fifo_sale_log <- function(sale_log,import_log){
  return('under dev')
}