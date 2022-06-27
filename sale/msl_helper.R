msl_load_ui <- function(input,output,ui_list){
  if ('msl_data' %in% ui_list){
    output$msl_data <- render_msl_data(input)
  }
  if ('msl_pxk_num' %in% ui_list){
    output$msl_pxk_num <- render_msl_pxk_num(input)
  }
  if ('msl_pxk_lineid' %in% ui_list){
    output$msl_pxk_lineid <- render_msl_pxk_lineid(input)
  }
  if ('msl_customer' %in% ui_list){
    output$msl_customer <- render_msl_customer(input)
  }
  if ('msl_prod_name' %in% ui_list){
    output$msl_prod_name <- render_msl_prod_name(input)
  }
  if ('msl_pxk_line_col' %in% ui_list){
    output$msl_pxk_line_col <- render_msl_pxk_line_col(input)
  }
  if ('msl_pxk_line_col_content' %in% ui_list){
    output$msl_pxk_line_col_content <- render_msl_pxk_line_col_content(input)
  }
  if ('msl_confirm_code' %in% ui_list){
    output$msl_confirm_code <- render_msl_confirm_code(input)
  }
  if ('msl_del_line_explan' %in% ui_list){
    output$msl_del_line_explan <- render_msl_del_line_explan(input)
  }
  if ('msl_edit_line_explan' %in% ui_list){
    output$msl_edit_line_explan <- render_msl_edit_line_explan(input)
  }
  
  return(output)
}

msl_init <- function(input,output){
  output <- msl_load_ui(
    input,output, 
    c('msl_data', "msl_pxk_num", "msl_pxk_lineid", "msl_customer",
      "msl_prod_name", "msl_pxk_line_col", "msl_pxk_line_col_content",
      "msl_del_line_explan", "msl_edit_line_explan","msl_confirm_code"))
  return(output)
}

# filter the sale log
get_msl_data <- function(input,for_display=T, trans_col=T){
  
  # get filtering input
  msl_current_pxk <- input$msl_pxk_num
  msl_cid <- customer_info$customer_id[
    customer_info$customer_name == input$msl_customer]
  if(length(msl_cid)==0){msl_cid <- 0}
  msl_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$msl_prod_name]
  if(length(msl_prod_code)==0){msl_prod_code <- "ALL"}
  
  # apply filtering
  if(msl_current_pxk!=0){
    output_tbl <- sale_log[
      sale_log$pxk_num==input$msl_pxk_num,]
  }else{
    output_tbl <- sale_log
  }
  if(msl_cid!=0){
    output_tbl <- output_tbl[
      output_tbl$customer_id == msl_cid,]
  }
  if(msl_prod_code!="ALL"){
    output_tbl <- output_tbl[
      output_tbl$prod_code == msl_prod_code,]
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
render_msl_data <- function(input){DT::renderDataTable({
  
  # get the table then display it using DTdatatable
  output_tbl <- get_msl_data(input)
  DT::datatable(output_tbl, options = list(pageLength = 10), 
                rownames=F)
  
})
}

render_msl_pxk_num <- function(input){renderUI({
  pxk_list <- unique(sale_log$pxk_num)
  pxk_list <- c(pxk_list,0)
  selectizeInput(
    inputId = "msl_pxk_num", label = uielem$pxk_num,
    choices = pxk_list,
    selected = 0,
    options = list(create = F))
})
}

render_msl_confirm_code <- function(input){renderUI({
  selectizeInput(
    inputId = "msl_confirm_code", label = uielem$confirm_code,
    choices = 9999,
    selected = 9999,
    options = list(create = T))
})
}

render_msl_pxk_lineid <- function(input){renderUI({
  lineid_list <- get_msl_data(input)$id
  selectizeInput(
    inputId = "msl_pxk_lineid", label = uielem$line_id,
    choices = lineid_list,
    selected = NULL,
    options = list(create = F))
  
})
}

render_msl_customer <- function(input){renderUI({
  if(input$msl_pxk_num==0){
    customer_list <- c(uielem$all, customer_info$customer_name)
  }else{
    c_cid <- sale_log$customer_id[sale_log$pxk_num == input$msl_pxk_num]
    customer_list <- customer_info$customer_name[
      customer_info$customer_id == c_cid]
  }
  selectizeInput(
    inputId = "msl_customer", label = uielem$customer_name,
    choices = customer_list,
    selected = customer_list[1],
    options = list(create = F))
  
})
}

render_msl_prod_name <- function(input){renderUI({
  # may add sophisticated filters later on
  prod_list <- c(uielem$all, prod_choices$prod_search_str)
  selectizeInput(
    inputId = "msl_prod_name", label = uielem$comm_name,
    choices = prod_list,
    selected = prod_list[1],
    options = list(create = F))
  
})
}

render_msl_pxk_line_col <- function(input){renderUI({
  # may add sophisticated filters later on
  tmp <- db_read_query("select label, actual from uielem")
  editable_col <- data.frame(
    label = split_semi(config$msl_editable_col))
  editable_col <- merge(editable_col, tmp)
  editable_col <- editable_col$actual
  selectizeInput(
    inputId = "msl_pxk_line_col", label = uielem$column,
    choices = editable_col,
    selected = editable_col[1],
    options = list(create = F))
  
})}

render_msl_pxk_line_col_content <- function(input, allow_create=T){renderUI({
  if(input$msl_pxk_line_col==uielem$customer_name){
    choice_list <- customer_info$customer_name
    allow_create <- F
  }
  selectizeInput(
    inputId = "msl_pxk_line_col_content", label = uielem$content,
    choices = choice_list,
    selected = choice_list[1],
    options = list(create = allow_create))
})}

render_msl_del_line_explan <- function(input){renderUI({
  
  html_code <- paste(
    uielem$del_line_with_id,em(input$msl_pxk_lineid))
  HTML(html_code)
  
})}

render_msl_edit_line_explan <- function(input){renderUI({
  if(input$msl_pxk_line_col==uielem$customer_name){
    html_code <- paste(
      uielem$edit_data, em(uielem$customer_name), 
      uielem$into, input$msl_pxk_line_col_content)
  }else{
  html_code <- paste(
    uielem$edit_line_with_id,em(input$msl_pxk_lineid), uielem$column,
    em(input$msl_pxk_line_col),uielem$into,input$msl_pxk_line_col_content)
  }
  HTML(html_code)
  
})}

msl_del_line <- function(input, output){
  if(as.numeric(input$msl_confirm_code)==as.numeric(config$msl_confirm_code)){
    query <- paste0("delete from sale_log where id = ", input$msl_pxk_lineid)
    # print(query)
    db_exec_query(query)
    gbl_load_tbl("sale_log")
    gbl_update_inventory()
    
    output <- msl_load_ui(
      input,output, 
      c('msl_data', "msl_confirm_code", "msl_pxk_lineid"))
    return(output)
  }else{
    show_error("invalid_confirm_code")
    return(output)
  }
}

msl_edit_line <- function(input, output){
  if(as.numeric(input$msl_confirm_code)==as.numeric(config$msl_confirm_code)){
    tmp <- db_read_query("select label, actual from uielem")
    col_2edit <- tmp$label[tmp$actual==input$msl_pxk_line_col]
    if(col_2edit=="customer_name"){
      new_cid <- customer_info$customer_id[
        customer_info$customer_name==input$msl_pxk_line_col_content]
      query <- paste0(
        "update pxk_info set customer_id='", new_cid,
        "' where pxk_num = ",input$msl_pxk_num)
      print(query)
    }else{
      query <- paste0("update sale_log set ",col_2edit,"='",
                      input$msl_pxk_line_col_content,
                      "' where id = ",input$msl_pxk_lineid)
      print(query)
      # db_exec_query(query)
    }
    gbl_load_tbl("sale_log")
    gbl_update_inventory()
    output <- msl_load_ui(
      input,output, 
      c("msl_data", "msl_confirm_code", "msl_pxk_lineid"))
    return(output)
  }else{
    show_error("invalid_confirm_code")
    return(output)    
  }
}

msl_print_report <- function(input, output){
  output_tbl <- get_msl_data(input)
  msl_current_pxk <- input$msl_pxk_num
  
  # if it is a pxk do something otherwise print the output
  if(msl_current_pxk!=0){
    print_pxk(msl_current_pxk)
  }else{
    output_file <- file.path(
      config$report_out_path,
      paste0(config$report_name_default,".xlsx"))
    write.xlsx(output_tbl, output_file)
    open_location(output_file)
  }
}
