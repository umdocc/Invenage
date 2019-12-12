
# ----------------------- general operation functions --------------------------

# db_open create a conn object that database call can use
db_open <- function(config_dict){
  db_type <- config_dict$value[config_dict$name=='db_type']
  if (db_type == 'SQLite'){
    database_path <- config_dict$value[config_dict$name=='db_file']
    sqlite.driver <- dbDriver("SQLite")
    conn <- dbConnect(sqlite.driver, dbname = database_path)
  }
  if (db_type == 'MariaDB'){
    conn <- dbConnect(drv = RMariaDB::MariaDB(), 
                      username = config_dict$value[config_dict$name=='sql_usr'],
                      password = config_dict$value[config_dict$name=='sql_pswd'], 
                      host = config_dict$value[config_dict$name=='sql_host'],
                      port = 3306, dbname = 'invenage')
  }
  return(conn)
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

# function to rebuild the inventory from import_log and sale_log
# if pos_items ==T, it will only return items with positive stock
# we will use this a lot, so it should connect direct;y to database
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
  # inputDF$units_per_pack <- NULL
  return(inputDF)
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