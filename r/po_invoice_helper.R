# helper for the po_invoice tab

get_exw_price <- function(data_df,local_price=F){
  # get the colnames to use later on
  current_names <- names(data_df)
  # until we can figure out, use only import price
  exw_price <- import_price[import_price$currency_code!=1,]
  exw_price <- merge(exw_price,data_df,all.y=T)
  
  # if there is missing price, print them here then set to 0
  if (length(exw_price$prod_code[is.na(exw_price$import_price)])>0){
    print('the following prices are missing, try to update them asap')
    print(exw_price$prod_code[is.na(exw_price$import_price)])
    # set min_order to 1 and import price to 0
    exw_price$import_price[is.na(exw_price$import_price)] <- 0
    exw_price$min_order[is.na(exw_price$min_order)] <- 1
  }
  
  # use the qty to min_order ratio, to select the correct price
  exw_price$order_ratio <- exw_price$qty/exw_price$min_order
  exw_price <- exw_price %>% group_by(prod_code) %>% 
    mutate(min_order_ratio = min(order_ratio))
  exw_price <- exw_price %>% filter(order_ratio==min_order_ratio) %>% 
    rename(exw_price = import_price)
  new_names <- c(current_names,'exw_price')
  
  # return the data frame with added columns
  exw_price <- exw_price[,new_names]
  return(exw_price)
}

write_excel_data <- function(file_path,data_df,sheet=1,startcol=1,startrow=1){
  wb <- loadWorkbook(file_path)
  writeData(wb,sheet=1,x=data_df,startCol = startcol,startRow = startrow)
  saveWorkbook(wb,file_path,overwrite = T)
}