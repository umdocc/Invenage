# this function write the po_data price back to the po
write_po_price <- function(po_name,po_data){
  if(!('import_price' %in% names(po_data))){
    stop('import_price column not found')
  }
  # gather all data
  output_path <- get_po_filepath(po_name,config_dict)
  po_data_start_row <- get_excel_tbl_startrow(output_path) #auto guess
  po_data_start_col <- config_dict$value[config_dict$name=='po_data_start_col']
  po_labels <- unlist(
    strsplit(config_dict$value[config_dict$name=='po_data_cols'],';'))
  total_sum <- sum(po_data$import_price*po_data$qty)
  
  # write the data
  write_excel_data(
    output_path, po_data %>% select(import_price,prod_code),
    startcol = which(po_labels=='import_price')+as.numeric(po_data_start_col)-1,
    startrow = as.numeric(po_data_start_row)+1)
  # write 'Total' label
  write_excel_data(
    output_path, 'Total',
    startcol = which(po_labels=='comm_name')+as.numeric(po_data_start_col)-1,
    startrow = as.numeric(po_data_start_row)+nrow(po_data)+3)
  #write total_sum
  write_excel_data(
    output_path, total_sum,
    startcol = which(po_labels=='import_price')+as.numeric(po_data_start_col)-1,
    startrow = as.numeric(po_data_start_row)+nrow(po_data)+3)
}