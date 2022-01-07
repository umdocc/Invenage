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
  # append import_log
  observeEvent(input$aii_add_entry,{
    output <- aii_add_entry(input,output)     # writing to database
  })
  
  # -------------------------- sync_excel_po - sep -----------------------------
  
  # UI & data load
  sep_load_data()
  output <- sep_load_ui(input,output, c('sep_po_name'))
  
  # buttons handler
  observeEvent(input$sep_sync_po,{
    output <- sep_sync_po2db(input,output)     # writing to database
  })

})
