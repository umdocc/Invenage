sep_load_ui <- function(input,output,ui_list){
  
  if ('sep_po_list' %in% ui_list){
    output$sep_po_list <- render_sep_po_list()
  }
  
  if ('sep_po_data_tbl' %in% ui_list){
    output$sep_po_data_tbl <- render_po_data_tbl(input)
  }
  return(output)
}

# sep_add_po_import_price(input,output){
#   # po_name <- input$sep_po_list
#   # po_data <- read_po_data(po_name)
#   # vendor_id <- guess_vendor_id(po_name,mode='filepath')
#   # if (length(vendor_id)==1){
#   #   po_data$vendor_id <- vendor_id
#   #   po_data <- add_import_price(po_data)
#   #   
#   # }else{
#   #   show_alert("error","unknown_vendor","error")
#   # }
#   # 
# }