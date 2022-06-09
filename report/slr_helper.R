slr_load_ui <- function(input,output,ui_list){
  if ('slr_data' %in% ui_list){
    output$slr_data <- render_slr_data(input)
  }
  if ('slr_pxk_num' %in% ui_list){
    output$slr_pxk_num <- render_slr_pxk_num(input)
  }
  return(output)
}

slr_init <- function(input,output){
  output <- slr_load_ui(input,output, c('slr_data', "slr_pxk_num"))
  return(output)
}

# render table for the pxk_man tab
render_slr_data <- function(input){DT::renderDataTable({
  
  # get the table then display it using DTdatatable
  output_tbl <- sale_log
  DT::datatable(output_tbl, options = list(pageLength = 15), rownames=F,
                editable = 'cell')
  
})
}

render_slr_pxk_num <- function(input){renderUI({
  pxk_list <- unique(sale_log$pxk_num)
  selectizeInput(
    inputId = "slr_pxk_num", label = uielem$pxk_num,
    choices = pxk_list,
    selected = NULL,
    options = list(create = F))
})
}