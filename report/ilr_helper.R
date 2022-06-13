ilr_load_ui <- function(input,output,ui_list){
  if ('ilr_data' %in% ui_list){
    output$ilr_data <- render_ilr_data(input)
  }
  return(output)
}

ilr_init <- function(input, output){
  ilr_load_ui(
    input,output,
    c("ilr_data"))
}

# render table for the pxk_man tab
render_ilr_data <- function(input){DT::renderDataTable({
  
  # get the table tthen display it using DTdatatable
  output_tbl <- get_aii_import_data()
  DT::datatable(output_tbl, options = list(pageLength = 10), 
                rownames=F)
  
})
}