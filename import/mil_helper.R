mil_init <- function(input, output){
  ilr_load_ui(
    input,output,
    c("mil_data", "mil_prod_filter", "mil_lineid", "mil_line_col",
      "mil_line_col_content", "mil_confirm_code",
      "mil_del_line_explan", "mil_edit_line_explan"))
}

ilr_load_ui <- function(input,output,ui_list){
  if ('mil_data' %in% ui_list){
    output$mil_data <- render_mil_data(input)
  }
  if ('mil_prod_filter' %in% ui_list){
    output$mil_prod_filter <- render_mil_prod_filter(input)
  }
  if ('mil_lineid' %in% ui_list){
    output$mil_lineid <- render_mil_lineid(input)
  }
  if ('mil_line_col' %in% ui_list){
    output$mil_line_col <- render_mil_line_col(input)
  }
  if ('mil_line_col_content' %in% ui_list){
    output$mil_line_col_content <- render_mil_line_col_content(input)
  }

  if ('mil_confirm_code' %in% ui_list){
    output$mil_confirm_code <- render_mil_confirm_code(input)
  }
  if ('mil_del_line_explan' %in% ui_list){
    output$mil_del_line_explan <- render_mil_del_line_explan(input)
  }
  if ('mil_edit_line_explan' %in% ui_list){
    output$mil_edit_line_explan <- render_mil_edit_line_explan(input)
  }
  
  return(output)
}

# render table for the pxk_man tab
render_mil_data <- function(input){DT::renderDataTable({
  
  # get the table tthen display it using DTdatatable
  output_tbl <- get_mil_data(input)
  
  DT::datatable(output_tbl, options = list(pageLength = 10), 
                rownames=F)
})
}

# render product filter
render_mil_prod_filter <- function(input){renderUI({
  filter_choice <- c(uielem$all, prod_choices$prod_search_str)
  selectizeInput(
    inputId = "mil_prod_filter", label = uielem$comm_name,
    choices = filter_choice,
    selected = uielem$all,
    options = list(create = F))
})
}

# filter the import_log
get_mil_data <- function(input, trans_col=T){
  
  # prepare output
  output_tbl <- merge(
    import_log, 
    product_info %>% select(prod_code, comm_name), all.x = T)
  
  # filtering
  ilr_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$mil_prod_filter]
  if(input$mil_prod_filter != uielem$all){
    output_tbl <- output_tbl[output_tbl$prod_code == ilr_prod_code,]
  }
  
  # cleaning
  ilr_display_col <- split_semi(config$ilr_display_col)
  output_tbl <- output_tbl[, ilr_display_col]
  output_tbl <- output_tbl %>% arrange(desc(id))
  if(trans_col){
    output_tbl <- translate_tbl_column(output_tbl)
  }
  
  return(output_tbl)
}

render_mil_lineid <- function(input){renderUI({
  lineid_list <- get_mil_data(input)$id
  selectizeInput(
    inputId = "mil_lineid", label = uielem$line_id,
    choices = lineid_list,
    selected = NULL,
    options = list(create = F))
  
})
}

render_mil_line_col <- function(input){renderUI({
  # may add sophisticated filters later on
  tmp <- db_read_query("select label, actual from uielem")
  editable_col <- data.frame(
    label = unlist(split_semi(config$ilr_editable_col)))
  editable_col <- merge(editable_col, tmp)
  editable_col <- editable_col$actual
  selectizeInput(
    inputId = "mil_line_col", label = uielem$column,
    choices = editable_col,
    selected = editable_col[1],
    options = list(create = F))
  
})
}

render_mil_line_col_content <- function(input){renderUI({
  selectizeInput(
    inputId = "mil_line_col_content", label = uielem$content,
    choices = NULL,
    selected = NULL,
    options = list(create = T))
  
})
}

mil_del_line <- function(input, output){
# 
#   # if confirm code match, proceed, else display error
#   if(as.numeric(input$mil_confirm_code)==as.numeric(config$db_confirm_code)){
#   query <- paste0("delete from sale_log where id = ", input$ilr_pxk_lineid)
#   # db_exec_query(query)
#   gbl_load_tbl("sale_log")
#   gbl_update_inventory()
# 
#   output <- ilr_load_ui(
#     input,output,
#     c('mil_data'))
#   }else{
#     show_error("invalid_confirm_code")
#   }
# 
#   return(output)
}

mil_edit_line <- function(input, output){
  if(as.numeric(input$mil_confirm_code)==as.numeric(config$db_confirm_code)){
    tmp <- db_read_query("select label, actual from uielem")
    col_2edit <- tmp$label[tmp$actual==input$mil_line_col]
    query <- paste0("update import_log set ",col_2edit,"='",
                    input$mil_line_col_content,
                    "' where id = ",input$mil_lineid)
    db_exec_query(query)
    
    #reload data and ui
    gbl_load_tbl("import_log")
    gbl_update_inventory()
    output <- ilr_load_ui(
      input,output, 
      c('mil_data'))
  }else{
    show_error("invalid_confirm_code")
  }
  return(output)
}

render_mil_confirm_code <- function(input){renderUI({
  selectizeInput(
    inputId = "mil_confirm_code", label = uielem$confirm_code,
    choices = 999,
    selected = 999,
    options = list(create = T))
})
}

render_mil_del_line_explan <- function(input){renderUI({
  
  html_code <- paste(
    uielem$del_line_with_id,em(input$mil_lineid))
  HTML(html_code)
  
})}

render_mil_edit_line_explan <- function(input){renderUI({
  
  html_code <- paste(
    uielem$edit_line_with_id,em(input$mil_lineid), uielem$column,
    em(input$mil_line_col),uielem$into,input$mil_line_col_content)
  HTML(html_code)
  
})}

mil_print_report <- function(input, output){
  
  output_tbl <- get_mil_data(input)
  output_file <- file.path(
    config$report_out_path,paste0(config$report_name_default,".xlsx"))
  write.xlsx(output_tbl,output_file)
  show_success(type = "opening_excel", var_list = output_file)
  open_location(output_file)
}