# provide various tools

po_name <- 'MDS.Biolabo.PO.280720'
po_path <- get_po_filepath(po_name,config_dict)
compare_po_invoice(po_name,invoice_excel_name){
  po_data <- read_excel_po(get_po_filepath(po_name,config_dict))
  
  # fix for code still using vendor
  if (!('vendor_id' %in% names(po_data))){
    po_data <- merge(po_data,vendor_info %>% select(vendor,vendor_id))
    po_data$vendor <- NULL
  }
  
  # get the prod_code
  if (!('prod_code' %in% names(po_data))){
    po_data <- merge(po_data,
                     product_info %>% select(prod_code,vendor_id,ref_smn),
                     all.x=T)
    
  }
  po_data <- get_import_price(po_data)
}





