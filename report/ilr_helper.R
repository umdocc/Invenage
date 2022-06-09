ilr_load_ui <- function(input,output,ui_list){
  if ('ilr_data' %in% ui_list){
    output$ilr_data <- render_ilr_data(input)
  }
  return(output)
}

# render table for the pxk_man tab
render_ilr_data <- function(input){DT::renderDataTable({
  
  # get the table tthen display it using DTdatatable
  output_tbl <- get_aii_import_data()
  DT::datatable(output_tbl, options = list(pageLength = 15), rownames=F,
                editable = 'cell')
  
})
}