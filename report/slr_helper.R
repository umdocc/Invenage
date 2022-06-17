slr_load_ui <- function(input,output,ui_list){
  if ('slr_data' %in% ui_list){
    output$slr_data <- render_slr_data(input)
  }
  if ('slr_pxk_num' %in% ui_list){
    output$slr_pxk_num <- render_slr_pxk_num(input)
  }
  if ('slr_pxk_lineid' %in% ui_list){
    output$slr_pxk_lineid <- render_slr_pxk_lineid(input)
  }
  if ('slr_customer' %in% ui_list){
    output$slr_customer <- render_slr_customer(input)
  }
  if ('slr_prod_name' %in% ui_list){
    output$slr_prod_name <- render_slr_prod_name(input)
  }
  if ('slr_pxk_line_col' %in% ui_list){
    output$slr_pxk_line_col <- render_slr_pxk_line_col(input)
  }
  if ('slr_pxk_line_col_content' %in% ui_list){
    output$slr_pxk_line_col_content <- render_slr_pxk_line_col_content(input)
  }
  if ('slr_confirm_code' %in% ui_list){
    output$slr_confirm_code <- render_slr_confirm_code(input)
  }
  return(output)
}

slr_init <- function(input,output){
  output <- slr_load_ui(
    input,output, 
    c('slr_data', "slr_pxk_num", "slr_pxk_lineid", "slr_customer",
      "slr_prod_name", "slr_pxk_line_col", "slr_pxk_line_col_content"))
  return(output)
}

# filter the sale log
get_slr_data <- function(input,for_display=T, trans_col=T){
  
  # get filtering input
  slr_current_pxk <- input$slr_pxk_num
  slr_cid <- customer_info$customer_id[
    customer_info$customer_name == input$slr_customer]
  if(length(slr_cid)==0){slr_cid <- 0}
  slr_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$slr_prod_name]
  if(length(slr_prod_code)==0){slr_prod_code <- "ALL"}
  
  # apply filtering
  if(slr_current_pxk!=0){
    output_tbl <- sale_log[
      sale_log$pxk_num==input$slr_pxk_num,]
  }else{
    output_tbl <- sale_log
  }
  if(slr_cid!=0){
    output_tbl <- output_tbl[
      output_tbl$customer_id == slr_cid,]
  }
  if(slr_prod_code!="ALL"){
    output_tbl <- output_tbl[
      output_tbl$prod_code == slr_prod_code,]
  }
  
  if(for_display){
    output_tbl <- merge(output_tbl, product_info %>%
                          select(prod_code, comm_name), all.x=T)
    output_tbl <- merge(output_tbl, customer_info %>%
                          select(customer_id, customer_name), all.x=T)
    output_tbl$sale_date <- as.Date(output_tbl$sale_datetime)
    output_tbl <- output_tbl %>% 
      select(id, comm_name, unit, unit_price, qty, lot, pxk_num, customer_name,
             sale_date)
    
  }
  
  if(trans_col){
    output_tbl <- translate_tbl_column(output_tbl)
  }
  
  return(output_tbl)
}

# render table for the pxk_man tab
render_slr_data <- function(input){DT::renderDataTable({
  
  # get the table then display it using DTdatatable
  output_tbl <- get_slr_data(input)
  DT::datatable(output_tbl, options = list(pageLength = 10, dom = 't'), 
                rownames=F)
  
})
}

render_slr_pxk_num <- function(input){renderUI({
  pxk_list <- unique(sale_log$pxk_num)
  pxk_list <- c(pxk_list,0)
  selectizeInput(
    inputId = "slr_pxk_num", label = uielem$pxk_num,
    choices = pxk_list,
    selected = 0,
    options = list(create = F))
})
}

render_slr_pxk_lineid <- function(input){renderUI({
  lineid_list <- get_slr_data(input)$id
  selectizeInput(
    inputId = "slr_pxk_lineid", label = uielem$line_id,
    choices = lineid_list,
    selected = NULL,
    options = list(create = F))
  
})
}

render_slr_customer <- function(input){renderUI({
  if(input$slr_pxk_num==0){
    customer_list <- c(uielem$all, customer_info$customer_name)
  }else{
    c_cid <- sale_log$customer_id[sale_log$pxk_num == input$slr_pxk_num]
    customer_list <- customer_info$customer_name[
      customer_info$customer_id == c_cid]
  }
  selectizeInput(
    inputId = "slr_customer", label = uielem$customer_name,
    choices = customer_list,
    selected = customer_list[1],
    options = list(create = F))
  
})
}

render_slr_prod_name <- function(input){renderUI({
  # may add sophisticated filters later on
  prod_list <- c(uielem$all, prod_choices$prod_search_str)
  selectizeInput(
    inputId = "slr_prod_name", label = uielem$comm_name,
    choices = prod_list,
    selected = prod_list[1],
    options = list(create = F))
  
})
}

render_slr_pxk_line_col <- function(input){renderUI({
  # may add sophisticated filters later on
  tmp <- db_read_query("select label, actual from uielem")
  editable_col <- data.frame(
    label = unlist(split_semi(config$slr_editable_col)))
  editable_col <- merge(editable_col, tmp)
  editable_col <- editable_col$actual
  selectizeInput(
    inputId = "slr_pxk_line_col", label = uielem$column,
    choices = editable_col,
    selected = editable_col[1],
    options = list(create = F))
  
})
}

render_slr_pxk_line_col_content <- function(input){renderUI({
  selectizeInput(
    inputId = "slr_pxk_line_col_content", label = uielem$content,
    choices = NULL,
    selected = NULL,
    options = list(create = T))
  
})
}

render_slr_confirm_code <- function(input){renderUI({
  selectizeInput(
    inputId = "slr_confirm_code", label = uielem$confirm_code,
    choices = c(1987,1956),
    selected = NULL,
    options = list(create = F))
  
})
}

slr_del_line <- function(input, output){
  query <- paste0("delete from sale_log where id = ", input$slr_pxk_lineid)
  # print(query)
  db_exec_query(query)
  gbl_load_tbl("sale_log")
  gbl_update_inventory()
  
  output <- slr_load_ui(
    input,output, 
    c('slr_data'))
  
  return(output)
}

slr_edit_line <- function(input, output){
  tmp <- db_read_query("select label, actual from uielem")
  col_2edit <- tmp$label[tmp$actual==input$slr_pxk_line_col]
  query <- paste0("update sale_log set ",col_2edit,"=",
                  input$slr_pxk_line_col_content,
                  " where id = ",input$slr_pxk_lineid)
  print(query)
  # db_exec_query(query)
  gbl_load_tbl("sale_log")
  gbl_update_inventory()
  slr_load_ui(
    input,output,
    c("slr_pxk_data"))
  return(output)
}

slr_print_report <- function(input, output){
  output_tbl <- get_slr_data(input)
  slr_current_pxk <- input$slr_pxk_num
  
  # if it is a pxk do something otherwise print the output
  if(slr_current_pxk!=0){
    print_pxk(slr_current_pxk)
  }else{
    output_file <- file.path(
      config$report_out_path,
      paste0(config$report_name_default,".xlsx"))
    write.xlsx(output_tbl, output_file)
    open_location(output_file)
  }
}
