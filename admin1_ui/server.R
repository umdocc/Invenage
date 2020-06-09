# Invenage server.R
source("global.R",local = F)
require(dplyr)
require(DT)
shinyServer(function(input, output,session) {
  session$onSessionEnded( function(){
    stopApp()
  }) # quit on session end 
  
  # ---------------------------- ui configuration ------------------------------
  # hide ui tab by using logic
  
  if (FALSE){
  hideTab(inputId = "main", target = ui_elem$actual[ui_elem$label=='inv_out'])
  }
  # ------------------------------- inv_out UI ---------------------------------
  # sidebar
  output$customer_selector <- render_customer_list(
    'customer_name', type='inv_out', input) # customer
  output$prod_name_select <- render_prod_name_list(
    input,config_dict,'prod_name_select') # prod_name
  output$qty_selector <- render_qty(iid='qty_selector') #Qty
  output$unit_selector <- render_unit(input,iid='unit_selector') #Unit
  output$lot_select <- render_lot(input, iid='lot_select') # Lot
  output$warehouse_selector <- render_warehouse(
    input, 'warehouse_selector',warehouse_info) # warehouse
  output$unit_price <- render_price(input,iid='unit_price')
  output$payment_selector <- render_payment_type(input,
    iid = 'payment_type',ui_type = 'inv_out') # payment
  output$tender_name <- render_tender_list('tender_name', config_dict, input)
  output$pxk_note <- render_note(iid='pxk_note') #Note
  output$prod_info_str <- render_prod_info(input) #product Info pane
  output$sys_msg <- render_sys_message('ready')
  # side
  output$current_pxk_info <- render_current_pxk_infostr(
    config_dict) #current pxk info
  output$current_pxk_tbl <- render_invout_pxktable()
  output$invout_stt_list <- render_invout_stt_list(
    config_dict, 'invout_stt_list')
  
  # inv_out UI buttons handlers
  observeEvent(input$inventory_out, { # inv_out button
    exec_inv_out(input,output,config_dict) # write to database
      


    # refresh the UI after sucessfull inventory_out
    output$customer_selector <- render_customer_list(
      'customer_name', type='inv_out', input)
    output$prod_name_selector <- render_prod_name_list(
      input,product_info,'prod_name_select') # prod_name
    output$qty_selector <- render_qty(iid='qty_selector') #Qty
    output$lot_select <- render_lot(input, iid='lot_select') # Lot
    output$prod_info_str <- render_prod_info(input) #product Info pane
    output$pxk_note <- render_note(iid='pxk_note') #Note
    output$current_pxk_info <- render_current_pxk_infostr(
      config_dict) #current pxk info
    output$current_pxk_tbl <- render_invout_pxktable() # reload the PXK table
    output$invout_stt_list <- render_invout_stt_list( # reload invout_stt_list
      config_dict, 'invout_stt_list')
    output$man_pxk_list <- render_pxk_list(
      input,config_dict,'man_pxk_list') #reload pxk_list in pxk_man as well
    
  })
  
  observeEvent(input$del_invout_stt,{
    current_pxk <- get_current_pxk(config_dict)
    current_stt_list <- get_pxk_entry_num(current_pxk,config_dict)
    # if there is record in current pxk, allow delete
    if (length(current_stt_list)>0){
      stt_to_proc <- as.character(input$invout_stt_list)
      delete_pxk(current_pxk,stt_to_proc,config_dict)
    }
    # reload UI
    output$invout_stt_list <- render_invout_stt_list(
      config_dict, 'invout_stt_list')
    output$prod_name_selector <- render_prod_name_list(
      input,product_info,'prod_name_select') # prod_name, required for prod_info
    output$qty_selector <- render_qty(iid='qty_selector') #Qty
    output$lot_select <- render_lot(input, iid='lot_select') # Lot
    output$current_pxk_tbl <- render_invout_pxktable()
    output$prod_info_str <- render_prod_info(input)
  })
  
  observeEvent(input$complete_form,{
    # getting data to write to excel
    # finalised_pxk_warehouse <- input$warehouse_selector
    finalised_pxk_num <- get_current_pxk(config_dict)
    
    # update completed field in databse
    conn <- db_open(config_dict)
    query <- paste0("update pxk_info set completed = 1
                    where pxk_num = ",finalised_pxk_num)

    dbExecute(conn,query)

    dbDisconnect(conn)
    
    # UI refresh
    output$customer_selector <- render_customer_list(
      'customer_name', type='inv_out', input) # customer
    output$current_pxk_tbl <- render_invout_pxktable() # current_pxk_tbl
    output$current_pxk_info <- render_current_pxk_infostr(
      config_dict) #current pxk info
    output$prod_name_selector <- render_prod_name_list(
      input,product_info,'prod_name_select') # prod_name
    output$qty_selector <- render_qty(iid='qty_selector') #Qty
    output$lot_select <- render_lot(input, iid='lot_select') # Lot
    output$prod_info_str <- render_prod_info(input) #product Info pane
    output$pxk_note <- render_note(iid='pxk_note') #Note
    
    # create the excel for current pxk
    dest_path <- create_pxk_file(finalised_pxk_num)
    # open the file
    system2('open',dest_path,timeout = 2)
  })

  
  # ------------------------------- inv_in UI ----------------------------------
  output$in_prodname_select <- render_prod_name_list(
    input,config_dict,'in_prodname_select') # prod_name
  output$in_vendor <- render_in_vendor(iid = 'in_vendor', input, config_dict)
  output$in_unit <- render_unit(input,'in_unit',type='inv_in')
  output$in_actual_unit_cost <- render_in_cost(
    'in_actual_unit_cost', input, config_dict)
  output$in_note <- render_note('in_note')
  output$po_list_2load <-  render_po_list('po_list_2load', config_dict)
  
  # main table
  output$latest_import_tbl <- render_import_tbl()
  
  # ----------- buttons
  # create and append import_log
  observeEvent(input$inv_in,{
    # extract information
    # reload the table
    in_prod_code <- product_info$prod_code[
      product_info$search_str==input$in_prodname_select]
    current_date <- Sys.Date()
    in_warehouse <- product_info$warehouse_id[
      product_info$prod_code==in_prod_code]
    in_vendor_id <- vendor_info$vendor_id[vendor_info$vendor==input$in_vendor]
    
    # create append import_log
    # print(paste(as.numeric(input$in_actual_unit_cost)))
    append_import_log <- data.frame(
      prod_code = in_prod_code,
      unit = input$in_unit,
      qty = input$in_qty,
      po_name = paste0('import.',current_date),
      lot = input$in_lot,
      exp_date = input$in_expdate,
      actual_unit_cost = as.numeric(input$in_actual_unit_cost),
      actual_currency_code = 1,
      delivery_date = current_date,
      warehouse_id = in_warehouse,
      vendor_id = in_vendor_id,
      note = paste(
        vendor_info$vendor[vendor_info$vendor_id==in_vendor_id], input$in_note,
        sep = ';')
    )
    
    # writing to database
    conn <- db_open(config_dict)
    dbWriteTable(conn, 'import_log', append_import_log, append = T)
    dbDisconnect(conn)
    
    # refresh the UI
    output$latest_import_tbl <- render_import_tbl()
  })
  
  # load the excel po
  observeEvent(input$load_excel_po,{
    po_name <- input$po_list_2load
    small_msg <- load_po_to_db(po_name,config_dict)
    big_msg <- ui_elem$actual[ui_elem$label=='done']
    shinyalert(title = big_msg, text = small_msg, type = "success")
  })
  # ------------------------------- lookup UI ----------------------------------
  output$lookup_tbl_output <- DT::renderDataTable({
    table_name <- ui_elem$label[
      ui_elem$actual==input$lu_tbl_selector]
    create_lookup_tbl(table_name,config_dict)
  },rownames=F)
  observeEvent(input$print_lu_tbl,{
    table_name <- ui_elem$label[
      ui_elem$actual==input$lu_tbl_selector]
    lu_tbl_out <- create_lookup_tbl(table_name,config_dict)
    dest_path <- file.path(app_path,'lu_tbl.xlsx')
    write.xlsx(lu_tbl_out, dest_path,row.names=F)
    system2('open',dest_path,timeout = 2)
  })
  # ----------------------------- report UI ------------------------------------
  
  output$report_tbl_ouput <- DT::renderDataTable({
    report_type <- ui_elem$label[ui_elem$actual==input$report_type]
    rp_filename <- get_rp_filename(report_type, config_dict)
    create_report(report_type,rp_filename,config_dict,input)
  },rownames=F)
  
  # create the report and open it
  observeEvent(input$printReport, {
    # gather all data
    report_type <- ui_elem$label[ui_elem$actual==input$report_type]
    rp_data <- build_rp_data(report_type,input)
    to_date <- input$to_date
    
    # #debug
    # print(report_type);print(rp_data)
    
    #from_date and to_date depends on rp type
    if (report_type == 'sale_profit_report'){
      from_date <- input$from_date 
    }else{
      from_date <- strftime(Sys.Date())
    }
    if (report_type == 'inv_value_report'){
      rp_filename <- write_inv_value_rp()
    }else{
      rp_filename <- write_report_data(report_type, rp_data, from_date, to_date)
    }
    system(paste0('open ','"',rp_filename,'"'))
  })

  # ---------------------------- pxk_man UI ------------------------------------
  # sidebar
  output$man_pxk_list <- render_pxk_list(
    input,config_dict,'man_pxk_list') #pxk_list
  output$man_pxk_cust_select <- render_customer_list(
    'customer_change',type='customer_change', input)
  output$manpxk_pay_change <- render_payment_type(input, # payment change
    iid = 'manpxk_pay_change',ui_type = 'man_pxk') 
  
  #main
  output$man_pxk_info <- render_man_pxk_info(input)
  output$pxk_detail <- render_man_pxktable(input)
  output$stt_select <- render_pxkman_stt_list(
    input,config_dict, iid='stt_select') #select_stt
  
  # button handlers
  observeEvent(input$delete_stt_man,{
    selected_pxk_num <- as.integer(input$man_pxk_list)
    full_stt_list <- as.character(
      get_pxk_entry_num(selected_pxk_num,config_dict))
    trans_list <- data.frame(label=c(full_stt_list,'all'),
                             localised=c(full_stt_list,
                                 ui_elem$actual[ui_elem$label=='all']))
    stt_to_proc <- as.character(
      trans_list$label[
      as.character(trans_list$localised)==as.character(input$stt_select)]
    )
    delete_pxk(selected_pxk_num,stt_to_proc,config_dict)
    # refresh the UI
    output$pxk_detail <- render_man_pxktable(input) # reload the pxk_man table
    output$stt_select <- render_pxkman_stt_list(
    input,config_dict, iid='stt_select')
  })
  
  observeEvent(input$edit_pxk_info,{
    selected_pxk_num <- as.integer(input$man_pxk_list)
    # translate changed data
    new_customer <- input$customer_change
    new_customer_id <- customer_info$customer_id[
      customer_info$customer_name == new_customer]
    new_payment <- input$manpxk_pay_change
    tmp <- merge(payment_type,ui_elem,by.x = 'payment_label', by.y = 'label')
    new_payment_code <- tmp$payment_code[
      tmp$actual == new_payment]
    
    # writing to database
    conn <- db_open(config_dict)
    query <- paste(
      'update pxk_info set customer_id =', new_customer_id,', payment_code =',
      new_payment_code, "where pxk_num =", selected_pxk_num)
    dbExecute(conn,query)
    dbDisconnect(conn)
    
    # refresh the UI
    output$man_pxk_info <- render_man_pxk_info(input) # pxk_info row
    output$pxk_detail <- render_man_pxktable(input) # reload the pxk_man table
    output$stt_select <- render_pxkman_stt_list(
      input, config_dict, iid='stt_select')
  })
  # table edit handler
  observeEvent(input$pxk_detail_cell_edit,{
    cell <- input$pxk_detail_cell_edit
    current_pxk_num <- input$man_pxk_list
    errorsFree <- edit_db_pxk(cell,current_pxk_num)
    output$pxk_detail <- render_man_pxktable(input) #refresh the pxk
  })
  
  observeEvent(input$print_pxk_man,{
    man_pxk_num <- input$man_pxk_list
    dest_path <- create_pxk_file(man_pxk_num) # create the pxk
    system2('open',dest_path,timeout = 2) #open the file
  })
  # -------------------------- update_db UI -----------------------------------
  # add prod box
  output$add_prod_code <- render_prod_code_list('add_prod_code', allow_add = T)
  output$add_name <- render_name_list(input, 'add_name', allow_add = T)
  output$add_ref_smn <- render_ref_list(input, 'add_ref', allow_add = T)
  output$add_ordering_unit <- render_add_order_unit(
    input, 'add_ordering_unit', allow_add = T)
  output$add_orig_vendor <- render_add_vendor(
    input, 'add_orig_vendor', allow_add = T)
  output$add_warehouse <- render_add_warehouse(
    input,'add_warehouse', allow_add = T)
  output$add_prod_type <- render_add_prod_type(input, 'add_prod_type')
  
  # add packaging box
  output$add_pkg_prod_name <- render_prod_name_list(
    input,config_dict,'add_pkg_prod_name')
  output$add_pkg_str <- render_add_pkg_str(input)
  
  # add customer box
  output$add_customer_name <- render_customer_list(
    iid = 'add_customer_name',type = 'add_customer',input)
  
  # add_prod button
  observeEvent(input$add_product,{
    add_prod_to_db(input,output) # add to database
    
    # reload UI
    output$prod_name_select <- render_prod_name_list(
      input, product_info, 'prod_name_select') # prod_name
    output$in_prodname_select <- render_prod_name_list(
      input,product_info,'in_prodname_select') # prod_name
    output$in_unit <- render_unit(input,'in_unit',type='inv_in') # in_unit
    output$unit_selector <- render_unit(input,iid='unit_selector') #out_unit
    output$add_pkg_prod_name <- render_prod_name_list(
      input,product_info,'add_pkg_prod_name') # refresh add_pkg_prod_list
    output$add_pkg_str <- render_add_pkg_str(input)
    })
  # add_pkg button
  observeEvent(input$add_pkg,{
    add_pkg_to_db(input,output) # add to database
    
    # reload UI
    output$in_unit <- render_unit(input,'in_unit',type='inv_in') # in_unit
    output$unit_selector <- render_unit(input,iid='unit_selector') #out_unit
  })
  
  # add_customer button
  observeEvent(input$add_customer,{
    add_customer_to_db(input)
  })
})