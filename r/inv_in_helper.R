####### helper deals with underlying actions and logics
read_excel_po <- function(
  full_file_path,search_str = 'Description', search_col = 2){
  out_data <- read_excel_table(full_file_path, search_str, search_col)
  if(is.data.frame(out_data)){
    out_data <- col_name_to_label(config_dict,out_data)
    out_data <- out_data[!is.na(out_data$ref_smn),]
    out_data$vendor <- get_vendor_from_filename(config_dict, full_file_path)
    out_data$po_name <- gsub('\\.xlsx','',basename(full_file_path))
    # ref_smn needs to be string
    out_data$ref_smn <- as.character(out_data$ref_smn)
    out_data <- out_data %>% 
      select(
        stt,name,qty,ref_smn,lot,exp_date,actual_unit_cost,note,vendor,po_name)
  }
  return(out_data)
}

# this function read in the table, with search str and search_col
read_excel_table <- function(
  full_file_path,search_str = 'Description', search_col = 2){
  start_pt <- get_excel_tbl_startrow(full_file_path,search_str, search_col)
  if(length(start_pt)==0){
    show_alert('error','tbl_start_pt_not_found','error')
    out_data <- 'error'
  }else{
    out_data <- read.xlsx(
      full_file_path, startRow = start_pt+1, detectDates = T)
  }
  return(out_data)
}

get_excel_tbl_startrow <- function(
  full_file_path,search_str = 'Description', search_col = 2){
  tmp <- read.xlsx(full_file_path, skipEmptyRows = F)
  if (search_str %in% names(tmp)){
    start_pt <- 0
  }else{
    start_pt <- which(tmp[,search_col]==search_str)
  }
  return(start_pt)
}

get_po_filepath <- function(po_name,config_dict){
  po_path <- config_dict$value[config_dict$name=='po_path']
  po_list <- list.files(po_path, recursive = T)
  po_list <- po_list[grepl(po_name,po_list)]
  po_list <- po_list[!grepl('\\$',po_list)]
  full_path <- file.path(po_path, po_list)
  return(full_path)
}

sync_po_to_db <- function(po_name){
  out_msg <- '' #init the output message
  # print(po_name)
  # read the po data
  full_path <- get_po_filepath(po_name,config_dict)
  po_data <- read_excel_po(full_path)
  

  
  if(is.data.frame(po_data)){
    # add note columns if not presented in the po
    po_data <- add_missing_col(po_data,'note')
    # merge and trim po_data to required columns
    po_data <- merge(po_data,product_info %>% select(ref_smn,vendor,prod_code),
                     all.x=T)
    
    # write  the EXW price for po
    write_po_price(po_name,po_data)
    
    #remove qty = 0 items and items with no lot
    po_data <- po_data[po_data$qty >0,]
    po_data <- po_data[(!is.na(po_data$lot)) & (po_data$lot!=''),]
    
    # remove the "'" in lot/date
    po_data$lot <- gsub("'","",po_data$lot)
    po_data$exp_date <- gsub("'","",po_data$exp_date)
    
    # at this stage, proceed if only we have data
    if (nrow(po_data)>0){
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
        print('writing to import_log'); print(po_data)
        db_write(config_dict,'import_log',po_data)
        out_msg <- paste0(
          out_msg, '\n',ui_elem$actual[ui_elem$label=='add_lotdate_success'])
      }
      
      #update price
      # keep only rows with price to prevent writing NA in database
      po_price <- po_price[!is.na(po_price$actual_unit_cost),]
      if (nrow(po_price)>0){
        print('updating price(s)'); print(po_price)
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
        out_msg <- paste0(
          out_msg, '\n',ui_elem$actual[ui_elem$label=='update_cost_success'])
      }
      if (out_msg==''){
        out_msg <- ui_elem$actual[ui_elem$label=='nothing_to_update']
      }
    }
    # reload import_log
    reload_tbl(config_dict,'import_log')
    
    # show an alert
    big_msg <- ui_elem$actual[ui_elem$label=='done']
    shinyalert(title = big_msg, text = out_msg, type = "success")
  }
}

# function to process the inv_in button
process_inv_in_buttton <- function(config_dict,input){
  # reload the table
  in_prod_code <- product_info$prod_code[
    product_info$search_str==input$in_prodname_select]

  in_vendor_id <- vendor_info$vendor_id[vendor_info$vendor==input$in_vendor]
  
  # enforce lot and date
  if (input$in_lot==''|is.na(input$in_lot)){in_lot <- 'nolot'
  }else{in_lot <- input$in_lot}
  if (input$in_expdate==''|is.na(input$in_expdate)){in_date <- 'nodate'
  }else{in_date <- input$in_expdate}
  
  
  # create append import_log
  append_import_log <- data.frame(
    prod_code = in_prod_code,
    unit = input$in_unit,
    qty = input$in_qty,
    po_name = paste0('import.',Sys.Date()),
    lot = in_lot,
    exp_date = in_date,
    actual_unit_cost = as.numeric(input$in_actual_unit_cost),
    actual_currency_code = 1,
    delivery_date = Sys.Date(),
    warehouse_id = product_info$warehouse_id[
      product_info$prod_code==in_prod_code],
    vendor_id = in_vendor_id,
    note = input$in_note,
    in_invoice_num = input$in_invoice_num,
    in_vat_percent = input$in_vat_percent,
    in_warehouse_id = warehouse_info$warehouse_id[
      warehouse_info$warehouse==input$in_warehouse]
  )
  
  # during data check, the error_label gets overwritten with label
  # integrity and duplication check
  error_label <- check_data(append_import_log)
  error_label <- dup_check(append_import_log,tbl_name = 'import_log')
  
  # if error_label='', all checks passed and we can write to db
  if(error_label==''){
    # writing to database
    append_tbl_rld(config_dict,'import_log',append_import_log)
  }
}