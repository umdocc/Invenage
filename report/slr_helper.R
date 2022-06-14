slr_load_ui <- function(input,output,ui_list){
  if ('slr_data' %in% ui_list){
    output$slr_data <- render_slr_data(input)
  }
  if ('slr_pxk_num' %in% ui_list){
    output$slr_pxk_num <- render_slr_pxk_num(input)
  }
  if ('slr_pxk_stt' %in% ui_list){
    output$slr_pxk_stt <- render_slr_pxk_stt(input)
  }
  return(output)
}

slr_init <- function(input,output){
  output <- slr_load_ui(
    input,output, 
    c('slr_data', "slr_pxk_num", "slr_pxk_stt"))
  return(output)
}

get_slr_data <- function(input,for_display=T){
  slr_current_pxk <- input$slr_pxk_num
  
  if(slr_current_pxk!=0){
    output_tbl <- sale_log[
      sale_log$pxk_num==input$slr_pxk_num,]
  }else{
    output_tbl <- sale_log
  }
  
  if(for_display){
    
    output_tbl <- merge(output_tbl, product_info %>%
                          select(prod_code, comm_name), all.x=T)
    output_tbl <- output_tbl %>% 
      select(stt, comm_name, unit, unit_price, qty, lot, pxk_num)
    output_tbl <- translate_tbl_column(output_tbl)
  }
  
  return(output_tbl)
}

# render table for the pxk_man tab
render_slr_data <- function(input){DT::renderDataTable({
  
  # get the table then display it using DTdatatable
  output_tbl <- get_slr_data(input)
  DT::datatable(output_tbl, options = list(pageLength = 10), rownames=F,
                editable = 'cell')
  
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

render_slr_pxk_stt <- function(input){renderUI({
  stt_list <- sale_log$stt[sale_log$pxk_num==input$slr_pxk_num]
  selectizeInput(
    inputId = "slr_pxk_stt", label = NULL,
    choices = stt_list,
    selected = NULL,
    options = list(create = F))
  
})
}

slr_del_stt <- function(input){
  query <- paste0("delete from sale_log where pxk_num = ",input$slr_pxk_num,
                  " and stt = ",input$slr_pxk_stt)
  print(query)
  # db_exec_query(query)
  gbl_load_tbl("sale_log")
  gbl_update_inventory()
  slr_load_ui(
    input,output,
    c("slr_pxk_data"))
}
  