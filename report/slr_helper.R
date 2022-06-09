slr_load_ui <- function(input,output,ui_list){
  if ('slr_data' %in% ui_list){
    output$slr_data <- render_slr_data(input)
  }
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