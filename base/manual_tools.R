# read in data and check required columns
manual_po_read <- function(po_filepath,search_key="No",
                           remove_invalid=T){
  
  start_row <- 1
  while(length(start_row)>0){
    po_data <- read.xlsx(po_filepath, startRow = start_row)
    start_row <- start_row + which(po_data==search_key)
    }
  required_cols <- unlist(strsplit(config$po_data_colnames,";"))
  names(po_data)[1:8] <- required_cols
  
  # keep only required col and clean up
  po_data <- po_data[,required_cols]
  
  # add prod_code and check for invalid product
  po_data <- merge(po_data,product_info %>%
                     select(ref_smn,vendor_id,prod_code),
                   all.x=T)
  if(remove_invalid){
    po_data <- po_data[!is.na(po_data$prod_code),]
  }
}