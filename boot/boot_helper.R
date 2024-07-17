# --------------------- config and uielem --------------------------------

# create config_dict and build the paths inside, require app_path
# if the db_config flag is true, it will read config from db, merge with local,
# and prioritise local config in case of duplicates
load_local_config <- function(local_config_path){

  if (file.exists(local_config_path)){
    config_dict <- read.table(local_config_path, sep = "\t", header = T,
                              stringsAsFactors = F)
  }else{
    stop(paste('config file not found in',local_config_path))
  }
  
  config_dict$source_rank <- 1 # local=1;db=2
  
  # add comment column if not yet in config_dict, as db dict have comment
  if (!('comment' %in% names(config_dict))){
    config_dict$comment <- ''
  }
  
  return(config_dict)
}

build_config_path <- function(config_dict){
  
  # build paths in config_dict
  config_dict$value[config_dict$type=='relative'] <-
    file.path(app_path,config_dict$value[config_dict$type=='relative'])
  
  # fix windows styling
  config_dict$value[config_dict$type=='relative'|config_dict$type=='abs'] <-
    gsub('\\\\','/',config_dict$value[
      config_dict$type=='relative'|config_dict$type=='abs'])
  
  return(config_dict)
}

load_db_config <- function(local_config){
  admin_id <- as.integer(
    local_config$value[local_config$name=='admin_id'])
  
  db_config <- dbGetQuery(conn,paste0('select * from config where admin_id=',
                                      admin_id,' or admin_id=0'))
  
  # if there is duplicated items in db_config, use the one with admin_id
  db_config <- db_config %>% arrange(desc(admin_id))
  db_config <- db_config[!duplicated(db_config$name),]
  
  #remove admin_id and finalise
  db_config$admin_id <- NULL
  db_config$source_rank <- 2
  # db_config <- build_config_dict_path(db_config)
  
  return(db_config)
}

#create a wide format config and format the data type
create_config <- function(local_config,db_config){
  
  # bind the two config, sort by source_rank, then remove duplicates
  config_dict <- rbind(local_config,db_config)
  config_dict <- config_dict[order(config_dict$source_rank),]
  config_dict <- config_dict[!duplicated(config_dict$name),]
  
  #build the path
  config_dict <- build_config_path(config_dict)
  
  # convert to wide format
  config <- config_dict %>% select(name,value)
  config <- spread(config,name,value)
  
  # convert boolean variable
  var_list <- config_dict$name[config_dict$type=='boolean']
  if(length(var_list)>0){
    for (var_name in var_list){
      config[[var_name]] <- (config[[var_name]]==1|
                               grepl("T",config[[var_name]]))
    }
  }
  
  return(config)
}

# create ui_elem
create_uielem <- function(config){
  
  query <- paste0(
    "select * from uielem where app_lang='",config$app_lang,"'")
  uielem <- dbGetQuery(conn,query)
  
  uielem <- spread(uielem %>% select(label,actual),label,actual)
  
  return(uielem)
}

# 
# split a long string separated by ';' to recover the list of strings
split_semi <- function(input_str){
  
  if(grepl(';',input_str)){
  output_str <- unlist(strsplit(input_str,';'))
  }else{
    output_str <- input_str
    }
  
  return(output_str)
}
# 


# # ----------------------- general operation functions --------------------------

# # use shinyalert specify ony labels
# show_alert <- function(big_label,small_label,msg_type='error'){
#   shinyalert(title = get_actual(big_label), 
#              text = get_actual(small_label), 
#              type = msg_type)
# }
# 
# get_tender_status <- function(config_dict, current_tender_id){
#   
#   # filter sale_log and tender_details
#   tender_detail <- tender_detail[tender_detail$tender_id==current_tender_id,]
#   sale_log <- sale_log[!is.na(sale_log$tender_id),]
#   tender_sale <- sale_log[sale_log$tender_id==current_tender_id,]
#   
#   # convert the tender_detail, tender_sale to pack
#   tender_detail_pk <- convert_to_pack(
#     tender_detail,packaging,'tender_qty','tender_qty_pack') %>%
#     select(prod_code,unit,tender_qty_pack)
#   tender_sale_pk <- convert_to_pack(tender_sale,packaging,'qty','sale_qty_pack')
#   tender_sale_pk <- tender_sale_pk %>% group_by(prod_code,unit) %>% 
#     summarise(total_tender_sale = sum(sale_qty_pack), .groups = 'drop')
#   
#   # merge with tender_detail
#   tender_detail_pk <- merge(tender_detail_pk,tender_sale_pk, all.x=T)
#   tender_detail_pk$total_tender_sale[
#     is.na(tender_detail_pk$total_tender_sale)] <- 0
#   tender_detail_pk$tender_pk_remaining <- tender_detail_pk$tender_qty_pack -
#     tender_detail_pk$total_tender_sale
#   
#   return(tender_detail_pk)
#   
# }
# 
# col_name_to_label <- function(config_dict,out_data){
#   
#   rename_dict <- guess_table[guess_table$guess_type == 'rename_dict',]
#   for (input_name in names(out_data)){
#     if(input_name %in% rename_dict$input_str){
#       new_name <- rename_dict$output_str[rename_dict$input_str==input_name]
#       # print(input_name);print(new_name)
#       names(out_data)[names(out_data)==input_name] <- new_name
#     }
#   }
#   return(out_data)
# }
# 
# get_vendor_from_filename <- function(config_dict,full_file_path){
#   vendor <- NA
#   vendor_dict <- guess_table[guess_table$guess_type == 'vendor_dict',]
#   for (vendor_name in vendor_dict$input_str){
#     if (grepl(vendor_name,full_file_path)){
#       vendor <- vendor_dict$output_str[vendor_dict$input_str==vendor_name]
#     }
#   }
#   return(vendor)
# }
# 
# 
# 
# 
# 
# 
# # read_tbl is sub-routine for both reload_tbl and write_rld_tbl
# # it assumes connection object conn
# read_tbl <- function(conn,tbl_name){
#   output_tbl <- dbReadTable(conn,tbl_name)
#   
#   # special change for each table
#   if (tbl_name=='product_info'){
#     output_tbl$packaging_str[is.na(output_tbl$packaging_str)] <- ''
#     output_tbl$packaging_str[output_tbl$packaging_str=='NA'] <- ''
#     output_tbl$search_str <- paste(
#       output_tbl$ref_smn, output_tbl$comm_name, output_tbl$packaging_str, 
#       sep=' ')
#   }
#   if (tbl_name=='report_type'){
#     output_tbl <- merge(output_tbl,ui_elem %>% select(label,actual),
#                         by.x='group_label', by.y = 'label')
#     lu_report_list <- create_lu_report_list(config_dict)
#     output_tbl <- merge(lu_report_list,output_tbl)
#   }
#   if (tbl_name=='product_type'){
#     output_tbl <- merge(output_tbl,ui_elem) }
#   
#   if (tbl_name=='payment_type'){
#     output_tbl <- merge(output_tbl,ui_elem,
#                         by.x='payment_label',by.y='label')}
#   
#   assign(tbl_name,output_tbl, envir = .GlobalEnv)
# }
# 
# # reload table from database
# reload_tbl <- function(config_dict,tbl_name_lst){
#   conn <- db_open(config_dict)
#   # run through the list of tables, load them and assign to .GlobalEnv
#   for (tbl_name in tbl_name_lst){
#     read_tbl(conn,tbl_name)
#   }
#   dbDisconnect(conn)
# }
# 
# 
# 
# check_exist <- function(current_df, existing_df, check_col = 'file_name'){
#   existing_df$exist <- T
#   existing_df <- existing_df[,c(check_col,'exist')]
#   output_df <- merge(current_df, existing_df, all.x=T)
#   output_df$exist[is.na(output_df$exist)] <- F
#   return(output_df)
# }
# 
# # get a list of files in a given path
# get_file_info <- function(file_path){
#   if(file.exists(file_path)){
#     file_info <- list.files(file_path,recursive=T)
#     # get list of locked files
#     lock_file_exist <- F
#     if (any(grepl('~\\$',file_info))){
#       lock_file_exist <- T
#       locked_file <- data.frame(
#         file_name = gsub('~\\$','',basename(file_info[grepl('~\\$',file_info)])), 
#         locked = T)}
#     
#     # compile the po_info data frame
#     file_info <- file_info[!grepl('~\\$',file_info)]
#     file_info <- data.frame(
#       file_name = basename(file_info), relative_path = dirname(file_info))
#     if (lock_file_exist){
#       file_info <- merge(file_info,locked_file,all.x=T)
#     }else{
#       file_info$locked <- NA
#     }
#   }else{
#     file_info <- 'error no file found'
#   }
#   return(file_info)
# }
# 
# # get the ui_elem df
# get_ui_elem <- function(config_dict){
#   app_lang <- config_dict$value[config_dict$name=='app_lang']
#   conn <- db_open(config_dict)
#   localisation <- dbReadTable(conn,"localisation")
#   dbDisconnect(conn)
#   # use the configured language
#   localisation <- localisation[localisation$app_lang==app_lang,]
#   # extract ui_elem
#   ui_elem <- localisation[localisation$group=='ui_elements',]
#   return(ui_elem)
# }
# 
# # function to rebuild the inventory from database
# # if pos_items ==T, it will only return items with positive stock
# # if summarised = T, all lot data will be summarised, leaving only total for
# # a prod_code, this should only be used with pos_items = T
# update_inventory <- function(config_dict, pos_item=TRUE, summarised = FALSE,
#                              to_date = Sys.Date(), from_date="1900-01-01"){
#   # pre-process import_log as tmp
#   tmp <- db_read_query(paste0(
#     "select * from import_log where delivery_date between '",from_date,
#     "' and '",to_date,"';"
#   ))
#   tmp <- convert_to_pack(tmp,packaging,'qty','importQty')
#   tmp <- tmp %>% group_by(prod_code,unit,lot,warehouse_id) %>% 
#     summarise(totalImportQty = sum(importQty), .groups = 'drop')
#   
#   # process sale_log as tmp2
#   tmp2 <- db_read_query(paste0(
#     "select * from sale_log inner join pxk_info 
#     on sale_log.pxk_num = pxk_info.pxk_num 
#     where pxk_info.sale_datetime between '",from_date,"' and '",
#     to_date+1,"'"))
#   # for sale_log we need to merge with warehouse_id
#   tmp2 <- convert_to_pack(tmp2,packaging,'qty','saleQty')
#   tmp2 <- tmp2 %>% group_by(prod_code,unit,lot,warehouse_id) %>% 
#     summarise(totalSaleQty = sum(saleQty), .groups = 'drop')
#   totalInventory <- merge(tmp,tmp2,all=T,
#                           by=c('prod_code','unit','lot','warehouse_id'))
#   totalInventory$totalSaleQty[is.na(totalInventory$totalSaleQty)] <- 0
#   totalInventory$totalImportQty[is.na(totalInventory$totalImportQty)] <- 0
#   totalInventory$remaining_qty <- totalInventory$totalImportQty - 
#     totalInventory$totalSaleQty
#   
#   # keep only the available items
#   if (pos_item){
#     threshold <- 0.001
#     totalInventory <- totalInventory[
#       totalInventory$remaining_qty>threshold,] %>% distinct()
#   }
#   # recover the exp_date
#   exp_dateData <- import_log[!duplicated(import_log[c('prod_code','lot')]),] %>% 
#     select(prod_code,lot,exp_date) %>% distinct()
#   
#   # merge, distinct and remove NA
#   totalInventory <- merge(totalInventory,exp_dateData,all.x=T) %>% distinct()
#   inventory <- totalInventory[!is.na(totalInventory$prod_code),]
#   
#   # calculate the intexp_date, which is the exp_date in standard format
#   inventory$exp_date <- gsub('/','-',inventory$exp_date)
#   inventory$exp_date <- gsub(' .*$','',inventory$exp_date)
#   inventory$intexp_date <- parse_date_time(
#     inventory$exp_date,c('%Y-%m','%m-%Y','%d-%m-%Y','%Y-%m-%d'))
#   
#   # cleaning
#   inventory <- inventory[!is.na(inventory$prod_code),] #remove NA
#   
#   # we will no longer need the unit, as everything should be in ordering_unit
#   inventory$unit <- NULL
#   
#   return(inventory)
# }
# 
# # critial function, but I cant explain it :((
# convert_to_pack <- function(inputDF,packaging,stringSL,packString){
#   inputDF <- merge(
#     inputDF,packaging %>% select(prod_code,unit,units_per_pack),all.x=T)
#   # check integrity
#   if(nrow(inputDF[is.na(inputDF$units_per_pack),])>0){
#     print(inputDF[is.na(inputDF$units_per_pack),])
#     stop('inputDF contains unrecognised packaging')
#   }
#   inputDF[[packString]] <- as.numeric(inputDF[[stringSL]])/as.numeric(
#     inputDF$units_per_pack)
#   # clean up
#   inputDF$unit <- 'pack'
#   inputDF[[stringSL]] <- NULL
#   return(inputDF)
# }
# 
# # round all numeric column of a data frame
# round_df <- function(df, digits) {
#   nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
#   
#   df[,nums] <- round(df[,nums], digits = digits)
#   
#   (df)
# }
# 
# write_and_open_report <- function(input_df,format='.xlsx'){
#   file_path <- file.path(config$report_out_path,
#                          paste0(config$report_name_default,format))
#   write.xlsx(input_df,file_path)
#   system2("open",file_path,timeout = 2)
# }
# 
# # simple function to get r display human number
# format_num <- function(input_vector){
#   input_vector <- formatC(input_vector, 
#                           format='f',big.mark = ',',digits = 2)
#   return(input_vector)
# }
# 
# gen_empty_df <- function(){
#   output_df <- data.frame(message = 'this is an empty data frame')
#   return(output_df)
# }
# 
# # ------------------- file operation functions ---------------------------------
# 
# # check file and display a custom message based on label
# check_file_exist <- function(file_path,notfound_label){
#   if(!file.exists(file_path)){
#     show_alert("error",notfound_label,"error")
#     file_exist <- F
#   }else{
#     file_exist <- T}
#   return(file_exist)
# }
# 
# # open a file using default app with timeout so shiny will not hang
# open_file_wtimeout <- function(file_path){
#   system2('open',file_path,timeout = 2)
# }
