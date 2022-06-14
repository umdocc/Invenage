# server
shinyServer(function(input, output,session) {
  session$onSessionEnded( function(){
    stopApp()
  }) # quit on session end
  
  # ------------------------------ ui configuration ----------------------------
  # hide ui tab by using logic
  for (tab_label in hidden_tab){
    hideTab(inputId = "main", target = uielem[[tab_label]])
  }
  
  #  ---------------------- create delivery note - cdn -------------------------
  
  output <- init_cdn(input,output) #init
  
  # buttons handlers
  observeEvent(input$add_cdn_entry, { # inv_out button
    cdn_add_entry(input,output) # write to database
  })
  observeEvent(input$cdn_complete_pxk,{
    cdn_complete_pxk(input,output) # execute command to complete the pxk
  })
  
  # ------------------------- add_import_item - aii ----------------------------
  
  output <- aii_init(input,output)
  
  # button handlers
  observeEvent(input$aii_add_entry,{
    output <- aii_add_entry(input,output)     # writing to database
  })
  
  # -------------------------- sync_excel_po - sep -----------------------------
  
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
  
  # ------------------------ import_log_report - ilr ---------------------------
  
  output <- ilr_init(input,output) #init
  
  #button handlers
  observeEvent(input$ilr_print_report,{
    output <- ilr_print_report(input, output)   #print report
  })
  
  # ------------------------ sale_log_report - slr -----------------------------
  
  output <- slr_init(input,output) #init
  
  #button handlers
  observeEvent(input$slr_print_report,{
    output <- slr_print_report(input, output)   #print report
  })
  
  # ------------------------------ update_db menu ------------------------------
  
  # update_product_info --------------------------------------------------------
  
  output <- upi_init(input,output) #init
  
  # button handlers
  observeEvent(input$upi_add_product,{
    output <- upi_add_product(input, output)   # writing to database
  })
  observeEvent(input$upi_add_pkg,{
    output <- upi_append_pkg(input, output)   # writing to database
  })
  
})
