ilr_load_ui <- function(input,output,ui_list){
  if ('ilr_data' %in% ui_list){
    output$ilr_data <- render_ilr_data(input)
  }
  if ('ilr_prod_filter' %in% ui_list){
    output$ilr_prod_filter <- render_ilr_prod_filter(input)
  }
  return(output)
}

ilr_init <- function(input, output){
  ilr_load_ui(
    input,output,
    c("ilr_data", "ilr_prod_filter"))
}

# render table for the pxk_man tab
render_ilr_data <- function(input){DT::renderDataTable({
  
  # get the table tthen display it using DTdatatable
  output_tbl <- get_ilr_data(input)
  
  DT::datatable(output_tbl, options = list(pageLength = 10), 
                rownames=F)
})
}

# render product filter
render_ilr_prod_filter <- function(input){renderUI({
  filter_choice <- c(uielem$all, prod_choices$prod_search_str)
  selectizeInput(
    inputId = "ilr_prod_filter", label = uielem$comm_name,
    choices = filter_choice,
    selected = uielem$all,
    options = list(create = F))
})
}

# filter the import_log
get_ilr_data <- function(input, trans_col=T){
  
  # prepare output
  output_tbl <- merge(
    import_log, 
    product_info %>% select(prod_code, comm_name), all.x = T)
  
  # filtering
  ilr_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$ilr_prod_filter]
  if(input$ilr_prod_filter != uielem$all){
    output_tbl <- output_tbl[output_tbl$prod_code == ilr_prod_code,]
  }
  
  # cleaning
  output_tbl <- output_tbl %>% 
    select(comm_name, unit, qty, po_name, in_invoice_num, delivery_date)
  if(trans_col){
    output_tbl <- translate_tbl_column(output_tbl)
  }
  
  return(output_tbl)
}

ilr_print_report <- function(input, output){
  
  output_tbl <- get_ilr_data(input)
  output_file <- file.path(
    config$report_out_path,paste0(config$report_name_default,".xlsx"))
  write.xlsx(output_tbl,output_file)
  show_success(type = "opening_excel", var_list = output_file)
  open_location(output_file)
}