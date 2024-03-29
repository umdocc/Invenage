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

  # -------------------------------- sale menu ---------------------------------  
  
  # create delivery note -------------------------------------------------------
  
  output <- init_cdn(input,output) #init
  
  # buttons handlers
  observeEvent(input$add_cdn_entry, { # inv_out button
    cdn_add_entry(input,output) # write to database
  })
  observeEvent(input$cdn_complete_pxk,{
    cdn_complete_pxk(input,output) # execute command to complete the pxk
  })
  
  # sale_log_report ------------------------------------------------------------
  
  output <- msl_init(input,output) #init
  
  #button handlers
  observeEvent(input$msl_print_report,{
    output <- msl_print_report(input, output)   #print sale report
  })
  observeEvent(input$msl_reload,{
    output <- msl_init(input,output)   
  })
  observeEvent(input$msl_del_line,{
    output <- msl_del_line(input, output)   
  })
  observeEvent(input$msl_edit_line,{
    output <- msl_edit_line(input, output)   
  })

  # ------------------------------- import menu --------------------------------
  
  # add import item ------------------------------------------------------------
  
  output <- aii_init(input,output)
  
  # button handlers
  observeEvent(input$aii_add_entry,{
    output <- aii_add_entry(input,output)     # writing to database
  })
  
  # sync excel po --------------------------------------------------------------
  
  # buttons handler
  observeEvent(input$sep_add_po,{
    output <- sep_add_po2db(input,output)     # writing to database
  })
  observeEvent(input$sep_add_unit_cost,{
    output <- sep_update_unit_cost(input)     # writing to database
  })
  
  # manage import log ----------------------------------------------------------
  
  output <- mil_init(input,output) #init
  
  #button handlers
  observeEvent(input$mil_print_report,{
    output <- mil_print_report(input, output)   #print report
  })
  observeEvent(input$mil_del_line,{
    output <- mil_del_line(input, output)   
  })
  observeEvent(input$mil_edit_line,{
    output <- mil_edit_line(input, output)   
  })
  
  # vendor import invoice ------------------------------------------------------
  
  output <- vii_init(input,output)
  
  # button handlers
  observeEvent(input$vii_add_data,{
    output <- vii_add_data(input,output)     # writing to database
  })
  
  # ----------------------------- report menu ----------------------------------
  
  # po_inventory_report --------------------------------------------------------
  # UI & data load
  output <- pir_load_ui(input,output, c('pir_data'))
  
  # buttons handler
  observeEvent(input$pir_create_report,{
    pir_create_report(input)    # writing to database
  })
  
  # customer_pricing_report ----------------------------------------------------
  # UI & data load
  output <- cpr_init(input,output)
  observeEvent(input$cpr_create_report,{
    cpr_create_report(input)    # writing to database
  })
  
# -------------------------------- update_db menu ------------------------------
  # update_product_info --------------------------------------------------------
  
  output <- upi_init(input,output) #init
  
  # button handlers
  observeEvent(input$upi_add_product,{
    output <- upi_add_product(input, output)   # add product to db
  })
  observeEvent(input$upi_add_pkg,{
    output <- upi_append_pkg(input, output)   # add pkg to db
  })
  
  # update_vendor_info --------------------------------------------------------
  
  output <- uvi_init(input,output) #init
  
  # button handlers
  observeEvent(input$uvi_add_vendor,{
    output <- uvi_add_vendor(input, output)   # add vendor to db
  })

  # update_customer_info -------------------------------------------------------
  
  output <- uci_init(input,output) #init
  
  # button handlers
  observeEvent(input$uci_add_customer,{
    output <- uci_add_customer(input, output)   # add customer to db
  })
  
    
})
