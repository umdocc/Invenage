# server
shinyServer(function(input, output,session) {
  session$onSessionEnded( function(){
    stopApp()
  }) # quit on session end
  
# ------------------------------ ui configuration ------------------------------
  # hide ui tab by using logic
  for (tab_label in hidden_tab){
  hideTab(inputId = "main", target = uielem[[tab_label]])
  }
  
#  ----------------------- create delivery note - cdn --------------------------
  load_cdn_data(input) # data
  # UI
  output <- cdn_load_ui(
    input, output, c("cdn_customer", "cdn_prod_name", "cdn_qty", "cdn_unit",
                     "cdn_warehouse","cdn_lot","cdn_payment_type","cdn_unit_price",
                     "cdn_promo_price","cdn_tender_name","cdn_note",
                     "cdn_prod_info","cdn_pxk_info","cdn_pxk_data"))
  
  # buttons handlers
  observeEvent(input$add_cdn_entry, { # inv_out button
    cdn_add_entry(input,output) # write to database
  })
  
  observeEvent(input$cdn_complete_pxk,{
    cdn_complete_pxk(input,output) # execute command to complete the pxk
  })

# -------------------------- add_import_item - aii -----------------------------

  # UI load
  output <- aii_load_ui(input,output,
    c('aii_prod_name', "aii_vendor", 'aii_invoice_num', 
      "aii_invoice_warehouse", "aii_qty", "aii_unit", "aii_lot",
      "aii_exp_date", "aii_unit_cost", "aii_vat_percent", "aii_import_data"))
  # buttons
#   # create and append import_log
#   observeEvent(input$inv_in,{
#     process_inv_in_buttton(config_dict,input)     # writing to database
#     # refresh the UI
#     output$latest_import_tbl <- render_output_tbl('import_log')
#   })
#   
# # ---------------------------- reports menu ------------------------------------
#   # ----------------------------- po_inventory tab -----------------------------
#   output <- pir_load_ui(input,output,"pir_out_tbl")
#   observeEvent(input$print_inventory_report,{
#     # similar to the above but made it into excel format
#     create_inventory_report(input)
#   })
#   # ---------------------- sale_log_report tab ---------------------------------
#   output$sale_log_report_tbl <- render_sale_log_report_tbl(input)
#   observeEvent(input$print_sale_log_report,{
#     # similar to the above but made it into excel format
#     create_sale_log_report(input,print_report = T)
#   })
# 
#   # ---------------------- import_log_report tab ----------------------------
#   output$import_log_report_tbl <- render_import_log_report_tbl(input)
#   observeEvent(input$print_import_log_report,{
#     # similar to the above but made it into excel format
#     create_import_log_report(input,print_report = T)
#   })
#   
#   
#   # -------------------------- udb UI -----------------------------------
#   # add prod box
#   output$add_prod_code <- render_prod_code_list('add_prod_code', allow_add = T)
#   output$add_name <- render_name_list(input, 'add_name', allow_add = T)
#   output$add_ref_smn <- render_ref_list(input, 'add_ref', allow_add = T)
#   output$add_ordering_unit <- render_add_order_unit(
#     input, 'add_ordering_unit', allow_add = T)
#   reload_ui(input,output,c('add_orig_vendor'))
#   output$add_warehouse <- render_add_warehouse(
#     input,'add_warehouse', allow_add = T)
#   output$add_prod_type <- render_add_prod_type(input, 'add_prod_type')
#   
#   # add packaging box
#   output$add_pkg_prod_name <- render_product_list('add_pkg_prod_name')
#   output$add_pkg_str <- render_add_pkg_str(input)
#   
#   # add customer box
#   output$add_customer_name <- render_customer_list(
#     iid = 'add_customer_name',type = 'add_customer',input)
#   
#   # ----------------------------- update_product ----------------------------
#   udp_load_ui(input,output,"udp_vendor")
#   
#   # add_prod button
#   observeEvent(input$udp_add_product,{
#     udp_add_product(input,output) # add to database
#     
#     # reload UI
#     output$prod_name_select <- render_product_list('prod_name_select')
#     output$in_prodname_select <- render_product_list('in_prodname_select')
#     output$in_unit <- render_unit(input,'in_unit',type='inv_in') # in_unit
#     output$unit_selector <- render_unit(input,iid='unit_selector') #out_unit
#     output$add_pkg_prod_name <- render_product_list('add_pkg_prod_name')
#     output$add_pkg_str <- render_add_pkg_str(input)
#     })
#   # add_pkg button
#   observeEvent(input$add_pkg,{
#     add_pkg_to_db(input,output) # add to database
#     output <- reload_ui(input,output,c('in_unit','unit_selector'))
#     
#   })
#   
#   # add_customer button
#   observeEvent(input$add_customer,{
#     add_customer_to_db(input)
#     output <- reload_ui(input,output,'customer_selector')
# 
#   })
#   
#   # piu upload pdf button, will copy to a destination file
#   observe({
#     file_ext <- tools::file_ext(input$bankslip_upload$datapath)
#     file_name <- paste0(get_config('bankslip_location'),"Bankslip.",file_ext)
#     file.copy(input$bankslip_upload$datapath, file_name,
#               overwrite = T)
#     })
#     
#   
#   # ------------------------------- hr_log tab ---------------------------------
#   output <- reload_ui(input,output,
#     c('admin_name','hour_logged','task_desc','admin_activity_log',
#       'hrl_del_stt','hrl_sum_plot'))
# 
#   observeEvent(input$task_input,{
#     write_activity_log(input)
#     output <- reload_ui(input,output,
#       c('admin_name','hour_logged','task_desc','admin_activity_log',
#         'hrl_del_stt','hrl_sum_plot'))
#   })
#   observeEvent(input$del_hrl_entry,{
#   del_hrl_stt(input)
#     output <- reload_ui(input,output,
#       c('admin_activity_log','hrl_del_stt','hrl_sum_plot'))
#   })
#   
#   observeEvent(input$admin_activity_log_cell_edit,{
#     edit_dt(input,tbl_name='admin_activity_log')
#     output <- reload_ui(input,output,
#                         c('admin_activity_log','hrl_del_stt'))
#     
#   })
#   
#   # ---------------------------- uvi tab ------------------------------------
#   output <- reload_ui(input,output,
#     c('invoice_vendor','vendor_invoice_num','invoice_currency',
#       'invoice_amount','invoice_cd_num','uvi_invoice_date',
#       'invoice_po_num','vendor_invoice_tbl','piu_bankslip_vendor',
#       'piu_bankslip_invoice_num'))
#   observeEvent(input$update_invoice,{
#     update_invoice_data(input)
#     output <- reload_ui(
#       input,output,
#       c('vendor_invoice_num', 'invoice_amount', 'vendor_invoice_tbl',
#         'piu_bankslip_vendor', 'piu_bankslip_invoice_num'))
#   })
#   
#   # ------------------------- update_import_price tab --------------------------
#   output <- reload_ui(input,output,
#               c('uip_prod_name', 'uip_vendor', 'uip_import_price',
#                 'uip_currency','uip_min_order'))
#   observeEvent(input$uip_update_button,{
#     update_price_from_uip(input)
#   })
#   
#   # ------------------------- update_vendor tab --------------------------------
#   output <- reload_ui(
#     input,output,
#     c('uv_vendor', 'uv_vendor_orig', 'uv_vendor_local', 'vendor_info_tbl'))
#   observeEvent(input$uv_update_vendor,{
#     update_vendor_from_udv(input)
#     output <- reload_ui(
#       input,output,
#       c('uv_vendor', 'vendor_info_tbl', 'piu_bankslip_vendor', 'invoice_vendor',
#         'in_vendor', 'add_orig_vendor', 'uip_vendor'))
#   })
# 
#   
#   # ----------------------------- sync_excel_po tab ----------------------------
#   output <- sep_load_ui(input,output,
#     c('sep_po_list','sep_po_data_tbl'))
#   # button handler
#   observeEvent(input$sep_write_import_price,{
#     sep_add_po_import_price(input,output)
#   })
#   
#   # load the excel po
#   observeEvent(input$sep_sync_excel_po,{
#     po_name <- input$sep_po_list
#     sync_po_to_db(po_name)
#     
#     # refresh the UI
#     output$latest_import_tbl <- render_output_tbl('import_log')
#   })
#   
# 
#   # ---------------------- service_and_warranty menu ---------------------------
#   #            -------------- tech_service_log tab -------------
#   output <- tsl_load_ui(input,output,split_semi(config$tsl_ui_items))
#   
#   # button handler
#   observeEvent(input$tsl_add_entry,{
#     exec_tsl_add_entry(input,output)
#   })
#   #            ------------ tech_service_warranty tab ------------
#   output <- tsw_load_ui(input,output,split_semi(config$tsw_ui_items))
  
})
