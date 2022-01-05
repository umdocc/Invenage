sep_load_ui <- function(input,output,ui_list){
  if ('sep_po_name' %in% ui_list){
    output$sep_po_name <- render_sep_po_name(input)
  }
  if ('sep_po_data' %in% ui_list){
    output$sep_po_data <- render_sep_po_data(input)
  }
  return(output)
}