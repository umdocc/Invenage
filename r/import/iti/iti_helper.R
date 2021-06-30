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