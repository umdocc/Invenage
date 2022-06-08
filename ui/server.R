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
  
  output <- init_cdn(input,output) #init
  
  # buttons handlers
  observeEvent(input$add_cdn_entry, { # inv_out button
    cdn_add_entry(input,output) # write to database
  })
  observeEvent(input$cdn_complete_pxk,{
    cdn_complete_pxk(input,output) # execute command to complete the pxk
  })

#  ----------------------- manage delivery note - mdn --------------------------
  
  output <- mdn_load_ui(input,output,
    c("mdn_pxk_num", "mdn_pxk_info","mdn_pxk_data"))

  
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
  # sep_load_data()
  # output <- sep_load_ui(input,output, c('sep_po_name'))
  
  # buttons handler
  observeEvent(input$sep_add_po,{
    output <- sep_add_po2db(input,output)     # writing to database
  })
  observeEvent(input$sep_add_unit_cost,{
    output <- sep_update_unit_cost(input)     # writing to database
  })
  
  # ------------------------ po_inventory_report - pir -------------------------
  
  # UI & data load
  output <- pir_load_ui(input,output, c('pir_data'))
  
  # buttons handler
  observeEvent(input$pir_create_report,{
    pir_create_report(input)    # writing to database
  })
  
  # ------------------------------ update_db menu ------------------------------
  # update_product_info --------------------------------------------------------
  output <- upi_init(input,output) #init
  observeEvent(input$upi_add_product,{
    output <- upi_add_product(input, output)   # writing to database
  })
  
})
