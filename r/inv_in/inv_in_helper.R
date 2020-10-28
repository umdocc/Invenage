####### helper deals with underlying actions and logics

ivi_load_ui <- function(input,output,ui_list){
  if ('latest_import_tbl' %in% ui_list){
    output$latest_import_tbl <- render_import_log()
  }
  return(output)
}





# function to process the inv_in button
ivi_exec_inv_in <- function(input,output){
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
  
  # print(append_import_log)
  
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