uci_load_ui <- function(input, output, ui_list){
  
  if ("uci_data" %in% ui_list){
    output$uci_data <- render_uci_data()
  }
  if ("uci_customer_name" %in% ui_list){
    output$uci_customer_name <- render_uci_customer_name()
  }
  return(output)
}

uci_init <- function(input, output){
  output <- uci_load_ui(
    input, output, 
    c("uci_data", "uci_customer_name"))
  return(output)
}

uci_add_customer <- function(input, output){
  tmp <- customer_info %>% filter(customer_name == input$uci_customer_name)
  if(nrow(tmp)==0){
    append_data <- data.frame(
      customer_name = input$uci_customer_name,
      customer_address = input$uci_customer_address,
      customer_email = input$uci_customer_email,
      customer_tfn = input$uci_customer_tfn
    )
    db_append_tbl("customer_info",append_data)
    
    # build the customer code
    new_cid <- db_read_query(paste0(
      "select * from customer_info where customer_name='",
      append_data$customer_name,"'"))$customer_id
    update_query <- paste0("update customer_info set customer_code=concat('",
                           config$uci_code_prefix,"',lpad(",new_cid,",",
                           config$uci_code_num_width,",0)) where customer_id=",
                           new_cid)
    db_exec_query(update_query)
    
    show_success()
    gbl_load_tbl("customer_info")
    output <- uci_load_ui(
      input, output, 
      c("uci_data", "uci_customer_name"))
  }
  
  
  return(output)
}

# render table for the pxk_man tab
render_uci_data <- function(input){DT::renderDataTable({
  display_cols <- split_semi(config$uci_display_col)
  output_tbl <- customer_info %>% arrange(desc(customer_id))
  output_tbl <- output_tbl[,display_cols]
  output_tbl <- translate_tbl_column(output_tbl)
  DT::datatable(output_tbl, options = list(pageLength = 10), 
                rownames=F)
})
}

# render product filter
render_uci_customer_name <- function(input){renderUI({
  list_choice <- customer_info$customer_name
  selectizeInput(
    inputId = "uci_customer_name", label = uielem$customer_name,
    choices = list_choice,
    selected = list_choice[1],
    options = list(create = T))
})
}