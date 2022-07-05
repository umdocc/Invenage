aii_load_ui <- function(input,output,ui_list){
  if ('aii_prod_name' %in% ui_list){
    output$aii_prod_name <- render_aii_prod_name(input)
  }
  if ('aii_invoice_num' %in% ui_list){
    output$aii_invoice_num <- render_aii_invoice_num(input)
  }
  if ('aii_invoice_warehouse' %in% ui_list){
    output$aii_invoice_warehouse <- render_aii_invoice_warehouse(input)
  }
  if ('aii_qty' %in% ui_list){
    output$aii_qty <- render_aii_qty(input)
  }
  if ('aii_unit' %in% ui_list){
    output$aii_unit <- render_aii_unit(input)
  }
  if ('aii_lot' %in% ui_list){
    output$aii_lot <- render_aii_lot(input)
  }
  if ('aii_exp_date' %in% ui_list){
    output$aii_exp_date <- render_aii_exp_date(input)
  }
  if ('aii_vendor' %in% ui_list){
    output$aii_vendor <- render_aii_vendor(input)
  }
  if ('aii_unit_cost' %in% ui_list){
    output$aii_unit_cost <- render_aii_unit_cost(input)
  }
  if ('aii_vat_percent' %in% ui_list){
    output$aii_vat_percent <- render_aii_vat_percent(input)
  }
  if ('aii_import_data' %in% ui_list){
    output$aii_import_data <- render_aii_import_data(input)
  }
  return(output)
}

aii_init <- function(input, output){
  output <- aii_load_ui(
    input,output,
    c('aii_prod_name', "aii_vendor", 'aii_invoice_num', 
      "aii_invoice_warehouse", "aii_qty", "aii_unit", "aii_lot",
      "aii_exp_date", "aii_unit_cost", "aii_vat_percent", "aii_import_data"))
  return(output)
}


render_aii_prod_name <- function(input){renderUI({

  selectizeInput(
    inputId = "aii_prod_name", label = uielem$comm_name,
    choices = prod_choices$prod_search_str,
    selected = prod_choices$prod_search_str[1],
    options = list(create = F))
})
}
 
render_aii_qty <- function(input){renderUI({

    selectizeInput(
    inputId = "aii_qty", label = uielem$qty,
    choices = 1:20000, selected = 1,
    options = list(create = T))
})
}

render_aii_unit <- function(input){renderUI({

  current_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$aii_prod_name]

  unit_choices <- packaging$unit[packaging$prod_code==current_prod_code]

  selectizeInput(
    inputId = "aii_unit", label = uielem$unit,
    choices = unit_choices, selected = unit_choices[1],
    options = list(create = F))
})
}

render_aii_invoice_num <- function(input){renderUI({
  selectizeInput(
    inputId = "aii_invoice_num", label = uielem$invoice_num,
    choices = "", selected = "",
    options = list(create = T))
})
}

render_aii_invoice_warehouse <- function(input){renderUI({

  warehouse_choice <- warehouse_info$warehouse

  #render ui
  selectizeInput(
    inputId = "aii_invoice_warehouse", label = uielem$invoiced_warehouse,
    choices = warehouse_choice, selected = warehouse_choice[1],
    options = list(create = F))
})
}

render_aii_lot <- function(input){renderUI({
  current_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$aii_prod_name]

  lot_select <- inventory[inventory$prod_code==current_prod_code,] %>%
    arrange(intexp_date)
  lot_choices <- lot_select$lot
  lot_select <- lot_select[
    lot_select$intexp_date==max(lot_select$intexp_date),]$lot
  #render ui
  selectizeInput(
    inputId = "aii_lot", label = uielem$lot,
    choices = lot_choices, selected = lot_select,
    options = list(create = T))
})
}

render_aii_exp_date <- function(input){renderUI({
  
  selectizeInput(
    inputId = "aii_exp_date", label = uielem$exp_date,
    choices = "", selected = "",
    options = list(create = T))
})
}

render_aii_vat_percent <- function(input){renderUI({
  current_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$aii_prod_name]
  current_vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor==input$aii_vendor
  ]
  import_hist <- import_log[import_log$prod_code==current_prod_code,]
  selected_vat <- import_hist$in_vat_percent[
    import_hist$vendor_id==current_vendor_id][1]

  #render ui
  selectizeInput(
    inputId = "aii_vat_percent", label = uielem$in_vat_percent,
    choices = 1:20, selected = selected_vat,
    options = list(create = T))
})
}

render_aii_unit_cost <- function(input){renderUI({

  current_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$aii_prod_name]

  import_hist <- import_log[import_log$prod_code==current_prod_code,]
  import_hist <- import_hist[import_hist$unit==input$aii_unit,]
  
  cost_selected <- import_hist$actual_unit_cost[
    import_hist$delivery_date==max(import_hist$delivery_date)]
  #render ui
  selectizeInput(
    inputId = "aii_unit_cost", label = uielem$unit_price,
    choices = import_hist$actual_unit_cost, selected = cost_selected,
    options = list(create = T))
})
}

render_aii_vendor <- function(input){renderUI({
  
  current_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$aii_prod_name]
  import_hist <- import_log[import_log$prod_code==current_prod_code,]

  selected_vendor_id <- import_hist$vendor_id[
    import_hist$delivery_date==max(import_hist$delivery_date)][1]
  selected_vendor <- vendor_info$vendor[
    vendor_info$vendor_id == selected_vendor_id]
  
  #render ui
  selectizeInput(
    inputId = "aii_vendor", label = uielem$vendor,
    choices = vendor_info$vendor,
    selected = selected_vendor,
    options = list(create = F))
})
}

# render table for the pxk_man tab
render_aii_import_data <- function(input){DT::renderDataTable({
  
  # get the table tthen display it using DTdatatable
  output_tbl <- get_aii_import_data()
  DT::datatable(output_tbl, options = list(pageLength = 15), rownames=F,
                editable = 'cell')
  
})
}

get_aii_import_data <- function(
    from_date = "1990-01-01", to_date = "2099-12-30", for_display=T){
  
  output_tbl <- merge(import_log,product_info,by="prod_code",all.x=T) 
  output_tbl <- output_tbl %>% 
    filter(delivery_date > as.Date(from_date) & 
             delivery_date < as.Date(to_date))

  # if this is for display, format the output table
  if(for_display){
    output_tbl$dqty <- paste(output_tbl$qty,output_tbl$unit)
    output_tbl <- output_tbl %>% 
      select(id, comm_name, dqty, lot, exp_date, actual_unit_cost, 
             in_invoice_num, in_vat_percent, note) %>% arrange(desc(id))
    output_tbl <- translate_tbl_column(output_tbl)
  }
  
  return(output_tbl)

}
 
aii_add_entry <- function(input,output){
  
  current_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str==input$aii_prod_name]

  append_import_log <- data.frame(
    prod_code = current_prod_code,
    unit = input$aii_unit,
    qty = input$aii_qty,
    po_name = paste0(config$aii_manual_import_str,".",Sys.Date()),
    lot = input$aii_lot,
    exp_date = input$aii_exp_date,
    actual_unit_cost = input$aii_unit_cost,
    actual_currency_code = as.numeric(config$default_currency_code),
    delivery_date = Sys.Date(),
    warehouse_id = warehouse_info$warehouse_id[
      warehouse_info$warehouse==input$aii_invoice_warehouse],
    vendor_id = vendor_info$vendor_id[vendor_info$vendor==input$aii_vendor],
    in_invoice_num = input$aii_invoice_num,
    in_vat_percent = input$aii_vat_percent,
    in_warehouse_id = warehouse_info$warehouse_id[
      warehouse_info$warehouse==input$aii_invoice_warehouse],
    note = input$aii_note
    )
  
  # write to database
  aii_check_append_log(append_import_log)
  if(error_free){
    conn <- db_open()
    dbWriteTable(conn,"import_log",append_import_log,append=T)
    dbDisconnect(conn)
  }
  
  # reload global variables
  gbl_load_tbl("import_log")
  aii_clean_duplicated()
  gbl_update_inventory()
  output <- aii_load_ui(input,output,"aii_import_data")
  return(output)
}

aii_clean_duplicated <- function(){
  #import log duplication
  tmp <- import_log[
    duplicated(
      import_log %>% 
        select(prod_code,unit,qty,po_name,lot,in_invoice_num, 
               delivery_date, in_warehouse_id, in_vat_percent, note)),]
  
  if(nrow(tmp)>0){
    # clean duplicated
    conn <- db_open()
    for (i in 1:nrow(tmp)){
      query <- paste0("delete from import_log where id=",tmp$id[i])
      dbExecute(conn,query)
    }
    dbDisconnect(conn)
  }
  gbl_load_tbl("import_log")
}

aii_check_append_log <- function(append_import_log){
  if(is.na(as.numeric(append_import_log$actual_unit_cost))){
    show_error(uielem$unit_cost_notfound)
  }
}