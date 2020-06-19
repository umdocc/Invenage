# function to process the inv_in button
process_inv_in_buttton <- function(config_dict,input){
  # reload the table
  in_prod_code <- product_info$prod_code[
    product_info$search_str==input$in_prodname_select]
  current_date <- Sys.Date()
  in_warehouse <- product_info$warehouse_id[
    product_info$prod_code==in_prod_code]
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
    po_name = paste0('import.',current_date),
    lot = in_lot,
    exp_date = in_date,
    actual_unit_cost = as.numeric(input$in_actual_unit_cost),
    actual_currency_code = 1,
    delivery_date = current_date,
    warehouse_id = in_warehouse,
    vendor_id = in_vendor_id,
    note = paste(
      vendor_info$vendor[vendor_info$vendor_id==in_vendor_id], input$in_note,
      sep = ';')
  )
  tmp <- check_exist(append_import_log,import_log,
                    c('prod_code','unit','qty','po_name','lot','exp_date'))
  if (any(tmp$exist)){
    show_alert('error','previously_entered','error')
  }else{
    # writing to database
    append_tbl_rld(config_dict,'import_log',append_import_log)
  }
}

