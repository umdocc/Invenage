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

# try to be smart about writing excel
# if file path exist, it will load the workbook, otherwise will create
write_excel_data <- function(file_path,data_df,sheet=1,startcol=1,startrow=1){
  if (file.exists(file_path)){
  wb <- loadWorkbook(file_path)
  }else{
    wb <- createWorkbook(file_path)
  }
  writeData(wb,sheet=1,x=data_df,startCol = startcol,startRow = startrow)
  saveWorkbook(wb,file_path,overwrite = T)
}

# this function handle all invoice update, it will need only the shiny input
update_invoice_data <- function(input){
  # collect all data
  current_invoice_num <- input$vendor_invoice_num
  append_invoice <- data.frame(
    vendor_id = vendor_info$vendor_id[vendor_info$vendor==input$invoice_vendor],
    invoice_num = current_invoice_num,
    invoice_amount = as.numeric(input$invoice_amount),
    currency_code = currency$currency_code[
      currency$currency==input$invoice_currency],
    payment_id = input$payment_id,
    invoice_note = input$invoice_note,
    invoice_cd_num = input$invoice_cd_num,
    po_name = input$invoice_po_num
  )
  # update by first removing the old data
  
  del_query <- paste0("delete from vendor_invoice where invoice_num like'",
                      input$vendor_invoice_num,"'")
  db_exec_query(del_query)
  
  append_tbl_rld(config_dict,'vendor_invoice',append_invoice)
  show_alert('success','invoice_update_success','success')
}




