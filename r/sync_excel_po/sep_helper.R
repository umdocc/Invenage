sep_load_ui <- function(input,output,ui_list){
  
  if ('sep_po_list' %in% ui_list){
    output$sep_po_list <- render_sep_po_list()
  }
  
  if ('sep_po_data_tbl' %in% ui_list){
    output$sep_po_data_tbl <- render_po_data_tbl(input)
  }
  return(output)
}
