require(dplyr)
require(DT)

# server
shinyServer(function(input, output,session) {
  session$onSessionEnded( function(){
    stopApp()
  }) # quit on session end 
  
  # ---------------------------- ui configuration ------------------------------
  # hide ui tab by using logic
  for (tab_label in hidden_tab){
  hideTab(inputId = "main", target = ui_elem$actual[ui_elem$label==tab_label])
  }
  # ------------------------------- inv_out UI ---------------------------------
  # sidebar
  output <- reload_ui(input,output,
    c('customer_selector','prod_name_select','qty_selector','unit_selector',
      'lot_select','warehouse_selector','unit_price','payment_selector',
      'tender_name','pxk_note','prod_info_str','sys_msg','current_pxk_info',
      'current_pxk_tbl','invout_stt_list'))
  
  # inv_out UI buttons handlers
  observeEvent(input$inventory_out, { # inv_out button
    exec_inv_out(input,output) # write to database
    # refresh the ui
    output <- reload_ui(input,output,
      c('customer_selector','prod_name_select','qty_selector','lot_select',
        'pxk_note','prod_info_str','current_pxk_info','current_pxk_tbl',
        'invout_stt_list','man_pxk_list'))
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
    output <- reload_ui(input,output,
      c('invout_stt_list','current_pxk_tbl','prod_info_str'))
  })
  
  observeEvent(input$complete_form,{
    
    finalised_pxk_num <- get_current_pxk(config_dict)
    
    # update completed field in databse
    conn <- db_open(config_dict)
    query <- paste0("update pxk_info set completed = 1
                    where pxk_num = ",finalised_pxk_num)
    dbExecute(conn,query)
    read_tbl(conn,'pxk_info')
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
    # writing to database
    process_inv_in_buttton(config_dict,input)
  
    # refresh the UI
    output$latest_import_tbl <- render_import_tbl()
  })
  
  # load the excel po
  observeEvent(input$load_excel_po,{
    po_name <- input$po_list_2load
    load_po_to_db(po_name,config_dict)

    # refresh the UI
    output$latest_import_tbl <- render_import_tbl()
  })
  # --------------------------- lu_report UI -------------------------------

  reload_ui(input,output,'lu_report_tbl_selector')
  output$lu_report_tbl <- DT::renderDataTable({
    table_name <- ui_elem$label[
      ui_elem$actual==input$lu_report_tbl_selector]
    create_lookup_tbl(input,table_name,config_dict)
  },rownames=F)
  observeEvent(input$print_lu_report,{
    # similar to the above but made it into excel format
    create_full_report(input)
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
  reload_ui(input,output,c('add_orig_vendor'))
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
    output <- reload_ui(input,output,c('in_unit','unit_selector'))
    
  })
  
  # add_customer button
  observeEvent(input$add_customer,{
    add_customer_to_db(input)
    output <- reload_ui(input,output,'customer_selector')

  })
  # ------------------------------- hr_log tab ---------------------------------
  output <- reload_ui(input,output,
    c('admin_name','hour_logged','task_desc','admin_activity_log'))

  observeEvent(input$task_input,{
    write_activity_log(input)
    output <- reload_ui(input,output,'admin_activity_log')
  })
  
  # ------------------------- invoice_update tab -------------------------------
  output <- reload_ui(input,output,
    c('invoice_vendor','vendor_invoice_num','invoice_currency',
      'invoice_amount','invoice_cd_num',
      'invoice_po_num'))
  observeEvent(input$update_invoice,{
    update_invoice_data(input)
    output <- reload_ui(
      input,output,
      c('vendor_invoice_num','invoice_cd_num','invoice_amount'))
  })
})