# --------------------- config and localisation --------------------------------

# create config_dict and build the paths inside, require app_path
# if the db_config flag is true, it will read config from db, merge with local,
# and prioritise local config in case of duplicates
create_config_dict <- function(app_path,location='home'){
  home_path <- path.expand('~')
  home_path <- gsub('\\\\','/',home_path) #windows fix
  home_path <- gsub('/Documents','',home_path)
  config_path <- file.path(home_path,'invenage_data','invenage_conf.csv')
  
  if (file.exists(config_path)){
    config_dict <- read.csv(config_path, stringsAsFactors = F)
  }else{
    stop('invenage_conf.csv not found!')
  }
  
  # # build paths in config_dict
  config_dict <- build_config_dict_path(config_dict)
  
  return(config_dict)
}

build_config_dict_path <- function(config_dict){
  # build paths in config_dict
  config_dict$value[config_dict$type=='abs'] <-
    gsub(';','/',config_dict$value[config_dict$type=='abs'])
  config_dict$value[config_dict$type=='relative'] <-
    gsub(';','/',config_dict$value[config_dict$type=='relative'])
  config_dict$value[config_dict$type=='relative'] <-
    file.path(app_path,config_dict$value[config_dict$type=='relative'])
  return(config_dict)
}

# create ui_elem
create_ui_elem <- function(){
  reload_tbl(config_dict,"localisation")
  app_lang <- config_dict$value[config_dict$name=='app_lang']
  localisation <- localisation[localisation$app_lang==app_lang,]
  ui_elem <- localisation[localisation$group=='ui_elements',]
  return(ui_elem)
}

# return the actual value of an ui_elem label
get_actual <- function(label_str){
  actual_str <- ui_elem$actual[ui_elem$label==label_str]
  return(actual_str)
}

# --------------------------- database functions -------------------------------

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

# simple db_write auto append
db_write <- function(config_dict,table_name,x){
  conn <- db_open(config_dict)
  dbWriteTable(conn,table_name,x,append=T)
  dbDisconnect(conn)
}

# one line db read
db_read_query <- function(query){
  conn <- db_open(config_dict)
  dataout <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  return(dataout)
}

# one-line db exec
db_exec_query <- function(query){
  conn <- db_open(config_dict)
  dbExecute(conn,query)
  dbDisconnect(conn)
}

# append to a table, then reload it
append_tbl_rld <- function(config_dict,tbl_name,x){
  conn <- db_open(config_dict)
  dbWriteTable(conn,tbl_name,x,append=T)
  read_tbl(conn,tbl_name)
  dbDisconnect(conn)
}

# ----------------------- general operation functions --------------------------
# this function translate a strring back into label by a single line ui_elem
# if a table is provided, it will also translate back to the code/id
uistr_to_label <- function(uistr,table_name=NULL){
  
  ui_label <- ui_elem$label[ui_elem$actual==uistr]
  
  if (!is.null(table_name)){
    db_table <- get(table_name)
    label_colname <- names(db_table)[grepl('_label',names(db_table))]
    code_colname <- names(db_table)[grepl('_code|_id',names(db_table))]
    ui_code <- db_table[db_table[,label_colname]==ui_label,code_colname]
  }else{ui_code <- ui_label}
  
  return(ui_code)
}

create_lu_report_list <- function(config_dict){
  lu_report_list <- unlist(strsplit(
    config_dict$value[config_dict$name=='lu_report_list_label'],';'))
  lu_report_list <- data.frame(label = lu_report_list)
  return(lu_report_list)
}

# use shinyalert specify ony labels
show_alert <- function(big_label,small_label,msg_type='error'){
  shinyalert(title = ui_elem$actual[ui_elem$label==big_label],
             text = ui_elem$actual[ui_elem$label==small_label], 
             type = msg_type)
}

get_tender_status <- function(config_dict, current_tender_id){

  # filter sale_log and tender_details
  tender_detail <- tender_detail[tender_detail$tender_id==current_tender_id,]
  sale_log <- sale_log[!is.na(sale_log$tender_id),]
  tender_sale <- sale_log[sale_log$tender_id==current_tender_id,]
  
  # convert the tender_detail, tender_sale to pack
  tender_detail_pk <- convert_to_pack(
    tender_detail,packaging,'tender_qty','tender_qty_pack') %>%
    select(prod_code,unit,tender_qty_pack)
  tender_sale_pk <- convert_to_pack(tender_sale,packaging,'qty','sale_qty_pack')
  tender_sale_pk <- tender_sale_pk %>% group_by(prod_code,unit) %>% 
    summarise(total_tender_sale = sum(sale_qty_pack), .groups = 'drop')
  
  # merge with tender_detail
  tender_detail_pk <- merge(tender_detail_pk,tender_sale_pk, all.x=T)
  tender_detail_pk$total_tender_sale[
    is.na(tender_detail_pk$total_tender_sale)] <- 0
  tender_detail_pk$tender_pk_remaining <- tender_detail_pk$tender_qty_pack -
    tender_detail_pk$total_tender_sale
  
  return(tender_detail_pk)
  
}

# create list of local po
get_local_po_list <-  function(config_dict){
  po_path <- config_dict$value[config_dict$name=='po_path']
  po_search_str <- config_dict$value[config_dict$name=='po_file_include']
  
  # R regex fix, for scanning PO
  po_search_str <- gsub('\\.','\\\\.',po_search_str)
  po_list <- get_file_info(po_path)
  po_list <- po_list[grepl(po_search_str,po_list$file_name),]
  
  # remove locked excel files, pdf files
  po_list <- po_list[!grepl('\\$',po_list$file_name),]
  po_list <- po_list[!grepl('pdf',po_list$file_name),]
  po_list$po_name <- gsub('\\.xlsx|\\.xls','',po_list$file_name)
  return(po_list)
}

# function to update po_info
update_po_info <- function(config_dict){
  # compare with remote database
  local_po <- get_local_po_list(config_dict)
  remote_po <- po_info
  
  # update with new local_po
  new_po <- check_exist(local_po,remote_po,'po_name')
  new_po <- new_po[!new_po$exist,]
  # write new data to database
  if (nrow(new_po)>0){
    new_po$completed <- 0
    new_po$finalised <- 0
    new_po$note <- ''
    new_po$exist <- NULL
    append_po <- new_po %>% select(po_name,completed,finalised,note)
    append_tbl_rld(config_dict,'po_info',append_po)
  }
}

col_name_to_label <- function(config_dict,out_data){
  
  rename_dict <- guess_table[guess_table$guess_type == 'rename_dict',]
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
  vendor_dict <- guess_table[guess_table$guess_type == 'vendor_dict',]
  for (vendor_name in vendor_dict$input_str){
    if (grepl(vendor_name,full_file_path)){
      vendor <- vendor_dict$output_str[vendor_dict$input_str==vendor_name]
    }
  }
  return(vendor)
}






# read_tbl is sub-routine for both reload_tbl and write_rld_tbl
# it assumes connection object conn
read_tbl <- function(conn,tbl_name){
  output_tbl <- dbReadTable(conn,tbl_name)
  
  # special change for each table
  if (tbl_name=='product_info'){
    output_tbl$packaging_str[is.na(output_tbl$packaging_str)] <- ''
    output_tbl$packaging_str[output_tbl$packaging_str=='NA'] <- ''
    output_tbl$search_str <- paste(
      output_tbl$ref_smn, output_tbl$comm_name, output_tbl$packaging_str, 
      sep=' ')
  }
  if (tbl_name=='report_type'){
    output_tbl <- merge(output_tbl,ui_elem %>% select(label,actual),
        by.x='group_label', by.y = 'label')
    lu_report_list <- create_lu_report_list(config_dict)
    output_tbl <- merge(lu_report_list,output_tbl)
  }
  if (tbl_name=='product_type'){
    output_tbl <- merge(output_tbl,ui_elem) }
  
  if (tbl_name=='payment_type'){
    output_tbl <- merge(output_tbl,ui_elem,
                        by.x='payment_label',by.y='label')}
  
  assign(tbl_name,output_tbl, envir = .GlobalEnv)
}

# reload table from database
reload_tbl <- function(config_dict,tbl_name_lst){
  conn <- db_open(config_dict)
  # run through the list of tables, load them and assign to .GlobalEnv
  for (tbl_name in tbl_name_lst){
    read_tbl(conn,tbl_name)
  }
  dbDisconnect(conn)
}



check_exist <- function(current_df, existing_df, check_col = 'file_name'){
  existing_df$exist <- T
  existing_df <- existing_df[,c(check_col,'exist')]
  output_df <- merge(current_df, existing_df, all.x=T)
  output_df$exist[is.na(output_df$exist)] <- F
  return(output_df)
}

# get a list of files in a given path
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
  
  tmp <- import_log %>% select(prod_code,unit,qty,lot,exp_date,warehouse_id)
  tmp <- convert_to_pack(tmp,packaging,'qty','importQty')
  tmp <- tmp %>% group_by(prod_code,unit,lot,warehouse_id) %>% 
    summarise(totalImportQty = sum(importQty), .groups = 'drop') %>% ungroup()
  tmp2 <- sale_log %>% select(prod_code,unit,qty,lot,warehouse_id)
  
  # for sale_log we need to merge with warehouse_id
  tmp2 <- convert_to_pack(tmp2,packaging,'qty','saleQty')
  tmp2 <- tmp2 %>% group_by(prod_code,unit,lot,warehouse_id) %>% 
    summarise(totalSaleQty = sum(saleQty), .groups = 'drop') %>% ungroup()
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
                total_inv_value = sum(total_inv_value), .groups = 'drop')
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

