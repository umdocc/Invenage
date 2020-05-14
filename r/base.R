# ----------------------- general operation functions --------------------------
# create list of local po
get_local_po_list <-  function(config_dict){
  po_path <- config_dict$value[config_dict$name=='po_path']
  po_search_str <- config_dict$value[config_dict$name=='po_file_include']
  
  # R regex fix, for scanning PO
  po_search_str <- gsub('\\.','\\\\.',po_search_str)
  po_list <- get_file_info(po_path)
  po_list <- po_list[grepl(po_search_str,po_list$file_name),]
  # remove locked xcel files
  po_list <- po_list[!grepl('\\$',po_list$file_name),]
  po_list$po_name <- gsub('\\.xlsx|\\.xls','',po_list$file_name)
  return(po_list)
}
# function to update po_info
update_po_info <- function(config_dict){
  po_path <- config_dict$value[config_dict$name=='po_path']
  # compare with remote database
  local_po <- get_local_po_list(config_dict)
  remote_po <- reload_tbl(config_dict,'po_info')
  
  # update with new local_po
  new_po <- check_exist(local_po,remote_po,'po_name')
  new_po <- new_po[!new_po$exist,]
  # write new data to database
  if (nrow(new_po)>0){
    new_po$completed <- 0
    new_po$finalised <- 0
    new_po$note <- ''
    new_po$exist <- NULL
    db_write(
      config_dict,'po_info',new_po %>% select(po_name,completed,finalised,note))
  }
}


# function to scan the configured directory for po and update the db
update_po_info <- function(config_dict){
  po_path <- config_dict$value[config_dict$name=='po_path']
  po_search_str <- config_dict$value[config_dict$name=='po_file_include']
  
  # R regex fix, for scanning PO
  po_search_str <- gsub('\\.','\\\\.',po_search_str)
  po_list <- get_file_info(po_path)
  po_list <- po_list[grepl(po_search_str,po_list$file_name),]
  
  # check data from server
  po_info <- reload_tbl(config_dict,'po_info')
  po_list <- check_exist(po_list, po_info, check_col = 'file_name')
  
  # if there is sth new write to database
  append_po <- po_list[!po_list$exist,] %>% select(file_name,relative_path)
  if (nrow(append_po)>0){
    append_po$completed <- 0
    append_po$finalised <- 0
    append_po$note <- NA 
    # writing to database
    db_write(config_dict,'po_info',append_po)
  }
}

# this function write new data, as well as update actual_unit_cost to db
load_po_to_db <- function(po_name,config_dict){
  
  # reload all required tables
  product_info <- reload_tbl(config_dict,'product_info')
  packaging <- reload_tbl(config_dict,'packaging')
  import_log <- reload_tbl(config_dict,'import_log')
  
  # read the PO, excluding locked file(s)
  po_path <- config_dict$value[config_dict$name=='po_path']
  po_list <- list.files(po_path, recursive = T)
  po_list <- po_list[grepl(po_name,po_list)]
  po_list <- po_list[!grepl('\\$',po_list)]
  full_path <- file.path(po_path, po_list)
  po_data <- read_excel_po(full_path)
  po_data <- merge(po_data,product_info %>% select(ref_smn,vendor,prod_code),
                   all.x=T)
  
  #remove qty = 0 items and items with no lot
  po_data <- po_data[po_data$qty >0,]
  po_data <- po_data[!is.na(po_data$lot),]
  # remove the "'" in lot/date
  po_data$lot <- gsub("'","",po_data$lot)
  po_data$exp_date <- gsub("'","",po_data$exp_date)
  
  # append other information
  po_data$delivery_date <- Sys.Date() # delivery_date
  po_data$actual_currency_code <- 1
  po_data$warehouse_id <- 1
  po_data <- merge(po_data,vendor_info,all.x=T)
  
  # add unit
  ordering_unit <- get_ordering_unit(packaging)
  ordering_unit <- ordering_unit[!duplicated(ordering_unit$prod_code),]
  po_data <- merge(po_data,ordering_unit, all.x=T)
  
  po_data <- po_data %>% 
    select(prod_code,unit,qty,po_name,lot,exp_date,actual_unit_cost,
           actual_currency_code,delivery_date,warehouse_id,vendor_id,note)
  
  
  
  # check and remove existing entries
  po_data <- check_exist(po_data,import_log, 
                         check_col = c('prod_code','qty','lot','po_name'))
  
  # create a copy to update price, then drop existing entries
  po_price <- po_data
  po_data <- po_data[!po_data$exist,]
  po_data$exist <- NULL
  
  
  # writing to database
  if (nrow(po_data)>0){
    db_write(config_dict,'import_log',po_data)
  }
  
  #update price
  # keep only rows with price to prevent writing NA in database
  po_price <- po_price[!is.na(po_price$actual_unit_cost),]
  if (nrow(po_price)>0){
  conn <- db_open(config_dict)
  for (i in 1:nrow(po_price)){
    query <- paste0('update import_log set actual_unit_cost = ',
                    po_price$actual_unit_cost[i],
                    ' where po_name like ','"',po_price$po_name[i],'"',
                    ' AND qty = ',po_price$qty[i], 
                    ' AND lot like ','"',po_price$lot[i],'"',
                    ' AND prod_code like ','"',po_price$prod_code[i],'"')
    dbExecute(conn,query)
  }
  dbDisconnect(conn)
  }
}

col_name_to_label <- function(config_dict,out_data){
  rename_dict <- reload_tbl(config_dict,'guess_table')
  rename_dict <- rename_dict[rename_dict$guess_type == 'rename_dict',]
  for (input_name in names(out_data)){
    if(input_name %in% rename_dict$input_str){
      new_name <- rename_dict$output_str[rename_dict$input_str==input_name]
      # print(input_name);print(new_name)
      names(out_data)[names(out_data)==input_name] <- new_name
    }
  }
  return(out_data)
}

get_vendor_from_filename <- function(config_dict,full_file_path){
  vendor <- NA
  vendor_dict <- reload_tbl(config_dict,'guess_table')
  vendor_dict <- vendor_dict[vendor_dict$guess_type == 'vendor_dict',]
  for (vendor_name in vendor_dict$input_str){
    if (grepl(vendor_name,full_file_path)){
      vendor <- vendor_dict$output_str[vendor_dict$input_str==vendor_name]
    }
  }
  return(vendor)
}

# the read_excel_po function scan a search string on a specified column then
# auto adjust, clean data etc
read_excel_po <- function(
  full_file_path,search_str = 'Description', search_col = 2){
  tmp <- read.xlsx(full_file_path, skipEmptyRows = F)
  start_pt <- which(tmp[,search_col]==search_str)
  out_data <- read.xlsx(full_file_path, startRow = start_pt)
  out_data <- col_name_to_label(config_dict,out_data)
  out_data <- out_data[!is.na(out_data$ref_smn),]
  out_data$vendor <- get_vendor_from_filename(config_dict, full_file_path)
  out_data$po_name <- gsub('\\.xlsx','',basename(full_file_path))
  out_data <- out_data %>% 
    select(stt,name,qty,ref_smn,lot,exp_date,actual_unit_cost,note,vendor,po_name)
  return(out_data)
}

# simple db_write auto append
db_write <- function(config_dict,table_name,x){
  conn <- db_open(config_dict)
  dbWriteTable(conn,table_name,x,append=T)
  dbDisconnect(conn)
}

# db_open create a conn object that database call can use
db_open <- function(config_dict){
  db_type <- config_dict$value[config_dict$name=='db_type']
  if (db_type == 'SQLite'){
    database_path <- config_dict$value[config_dict$name=='db_file']
    # sqlite.driver <- dbDriver("SQLite")
    conn <- dbConnect(drv = RSQLite::SQLite(), dbname = database_path)
  }
  if (db_type == 'MariaDB'){
    conn <- dbConnect(
      drv = RMariaDB::MariaDB(), 
      username = config_dict$value[config_dict$name=='sql_usr'],
      password = config_dict$value[config_dict$name=='sql_pswd'], 
      host = config_dict$value[config_dict$name=='sql_host'],
      port = 3306, dbname = 'invenage')
  }
  return(conn)
}

# reload table from database
reload_tbl <- function(config_dict,tbl_name){
  conn <- db_open(config_dict)
  output_tbl <- dbReadTable(conn,tbl_name)
  dbDisconnect(conn)
  
  # special change for each table
  if (tbl_name=='product_info'){
    output_tbl$search_str <- paste(
      output_tbl$ref_smn, output_tbl$comm_name, sep='-')
  }
  if (tbl_name=='product_type'){
    output_tbl <- merge(output_tbl,ui_elem)
  }
  return(output_tbl)
}

check_exist <- function(current_df, existing_df, check_col = 'file_name'){
  existing_df$exist <- T
  existing_df <- existing_df[,c(check_col,'exist')]
  output_df <- merge(current_df, existing_df, all.x=T)
  output_df$exist[is.na(output_df$exist)] <- F
  return(output_df)
}

# function to be added to base.R
get_file_info <- function(file_path){
  file_info <- list.files(file_path,recursive=T)
  # get list of locked files
  lock_file_exist <- F
  if (any(grepl('~\\$',file_info))){
    lock_file_exist <- T
    locked_file <- data.frame(
      file_name = gsub('~\\$','',basename(file_info[grepl('~\\$',file_info)])), 
      locked = T)}
  
  # compile the po_info data frame
  file_info <- file_info[!grepl('~\\$',file_info)]
  file_info <- data.frame(
    file_name = basename(file_info), relative_path = dirname(file_info))
  if (lock_file_exist){
    file_info <- merge(file_info,locked_file,all.x=T)
  }else{
    file_info$locked <- NA
  }
  return(file_info)
}

# get the ui_elem df
get_ui_elem <- function(config_dict){
  app_lang <- config_dict$value[config_dict$name=='app_lang']
  conn <- db_open(config_dict)
  localisation <- dbReadTable(conn,"localisation")
  dbDisconnect(conn)
  # use the configured language
  localisation <- localisation[localisation$app_lang==app_lang,]
  # extract ui_elem
  ui_elem <- localisation[localisation$group=='ui_elements',]
  return(ui_elem)
}

# function to rebuild the inventory from database
# if pos_items ==T, it will only return items with positive stock
# if summarised = T, all lot data will be summarised, leaving only total for
# a prod_code, this should only be used with pos_items = T
update_inventory <- function(config_dict, pos_item=TRUE, summarised = FALSE){
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
  
  # cleaning
  inventory <- inventory[!is.na(inventory$prod_code),] #remove NA
  # we will no longer need the unit, as everything should be in ordering_unit
  inventory$unit <- NULL
  
  if (summarised){
    inventory <- inventory %>% 
      group_by(prod_code, name, vendor, ref_smn) %>% 
      summarise(total_remain_qty = sum(remaining_qty), 
                total_inv_value = sum(total_inv_value))
  }
  
  return(inventory)
}

# critial function, but I cant explain it :((
convert_to_pack <- function(inputDF,packaging,stringSL,packString){
  inputDF <- merge(
    inputDF,packaging %>% select(prod_code,unit,units_per_pack),all.x=T)
  # check integrity
  if(nrow(inputDF[is.na(inputDF$units_per_pack),])>0){
    print(inputDF[is.na(inputDF$units_per_pack),])
    stop('inputDF contains unrecognised packaging')
  }
  inputDF[[packString]] <- as.numeric(inputDF[[stringSL]])/as.numeric(
    inputDF$units_per_pack)
  # clean up
  inputDF$unit <- 'pack'
  inputDF[[stringSL]] <- NULL
  return(inputDF)
}