ilr_load_ui <- function(input,output,ui_list){
  if ('ilr_data' %in% ui_list){
    output$ilr_data <- render_ilr_data(input)
  }
  if ('ilr_prod_filter' %in% ui_list){
    output$ilr_prod_filter <- render_ilr_prod_filter(input)
  }
  if ('ilr_lineid' %in% ui_list){
    output$ilr_lineid <- render_ilr_lineid(input)
  }
  
  return(output)
}

ilr_init <- function(input, output){
  ilr_load_ui(
    input,output,
    c("ilr_data", "ilr_prod_filter", "ilr_lineid"))
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
  ilr_display_col <- split_semi(config$ilr_display_col)
  output_tbl <- output_tbl[, ilr_display_col]
  output_tbl <- output_tbl %>% arrange(desc(id))
  if(trans_col){
    output_tbl <- translate_tbl_column(output_tbl)
  }
  
  return(output_tbl)
}

render_ilr_lineid <- function(input){renderUI({
  lineid_list <- get_ilr_data(input)$id
  selectizeInput(
    inputId = "ilr_lineid", label = uielem$line_id,
    choices = lineid_list,
    selected = NULL,
    options = list(create = F))
  
})
}

render_ilr_pxk_line_col <- function(input){renderUI({
  # may add sophisticated filters later on
  tmp <- db_read_query("select label, actual from uielem")
  editable_col <- data.frame(
    label = unlist(split_semi(config$ilr_editable_col)))
  editable_col <- merge(editable_col, tmp)
  editable_col <- editable_col$actual
  selectizeInput(
    inputId = "ilr_pxk_line_col", label = uielem$column,
    choices = editable_col,
    selected = editable_col[1],
    options = list(create = F))
  
})
}

render_ilr_pxk_line_col_content <- function(input){renderUI({
  selectizeInput(
    inputId = "ilr_pxk_line_col_content", label = uielem$content,
    choices = NULL,
    selected = NULL,
    options = list(create = T))
  
})
}

ilr_del_line <- function(input, output){
  
  query <- paste0("delete from sale_log where id = ", input$ilr_pxk_lineid)
  # print(query)
  db_exec_query(query)
  gbl_load_tbl("sale_log")
  gbl_update_inventory()
  
  output <- ilr_load_ui(
    input,output, 
    c('ilr_data'))
  return(output)
}

ilr_edit_line <- function(input, output){
  tmp <- db_read_query("select label, actual from uielem")
  col_2edit <- tmp$label[tmp$actual==input$ilr_pxk_line_col]
  query <- paste0("update sale_log set ",col_2edit,"='",
                  input$ilr_pxk_line_col_content,
                  "' where id = ",input$ilr_pxk_lineid)
  print(query)
  db_exec_query(query)
  gbl_load_tbl("sale_log")
  gbl_update_inventory()
  output <- ilr_load_ui(
    input,output, 
    c('ilr_data'))
  return(output)
}


ilr_print_report <- function(input, output){
  
  output_tbl <- get_ilr_data(input)
  output_file <- file.path(
    config$report_out_path,paste0(config$report_name_default,".xlsx"))
  write.xlsx(output_tbl,output_file)
  show_success(type = "opening_excel", var_list = output_file)
  open_location(output_file)
}