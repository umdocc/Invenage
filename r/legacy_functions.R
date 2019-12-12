# legacy_functions are disabled by default

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



# roll back x months from current month, not counting current month,
# return the beginning date of the rolled back month
roll_back_date <- function(rolling_mth){
  beginDate <- as.Date(as.character(cut(Sys.Date(), "month")),'%Y-%m-%d')
  backDate <- as.Date(as.character(cut(beginDate - 28*rolling_mth,'month')))
  return(backDate)
}

# create fifo_sale_log create a sale table with ammended import data using
# a fifo algorithm, it is needed for sale report accuracy
create_fifo_sale_log <- function(sale_log,import_log,pxk_info){
  import_log$delivery_date <- strptime(import_log$delivery_date,'%Y-%m-%d')
  sale_log <- merge(sale_log,pxk_info,all.x=T)
  sale_log$sale_datetime <- strptime(sale_log$sale_datetime,
                                     '%Y-%m-%d %H:%M:%S')
  # pack conversion
  sale_log <- convert_to_pack(sale_log,packaging,'qty','pack_qty')
  sale_log$pack_price <- sale_log$unit_price*sale_log$units_per_pack
  sale_log$pack_cost <- NA
  import_log <- convert_to_pack(import_log,packaging,'qty','pack_qty')
  import_log$pack_cost <- import_log$actual_unit_cost*import_log$units_per_pack
  import_log <- import_log[order(import_log$delivery_date),]
  sale_log <- sale_log[order(sale_log$sale_datetime),]
  for (i in 1:nrow(sale_log)){
    print(i)
    tmp <- import_log[import_log$prod_code == sale_log$prod_code[i] &
                        import_log$lot == sale_log$lot[i] ,][1,]
    if (!is.na(tmp$pack_qty)){
      if (tmp$pack_qty>=sale_log$pack_qty[i]){
        sale_log$pack_cost[i] <- tmp$pack_cost[1]
        new_pack_num <- tmp$pack_qty - sale_log$pack_qty[i]
        import_log[import_log$prod_code == sale_log$prod_code[i] &
                     import_log$lot == sale_log$lot[i] ,]$pack_qty[1] <- 
          new_pack_num
      }}
  }
  return('under dev')
}