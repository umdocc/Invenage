# vfr data is similar to po data but with added tender data
vfr_get_data <- function(vendor_id){
  
  # create data based on pir
  report_data <- pir_get_po_report(vendor_id, keep_prod_code=T)
  tender_data <- vfr_get_tender_data()
  report_data <- merge(report_data, tender_data, all.x= T)
  
  return(report_data)
  
}

vfr_get_tender_data <- function(active_only=T){
  tender_data <- merge(tender_detail, tender_info)
  tender_data <- tender_data %>% filter(active==active_only)
  tender_data <- convert_to_pack(
    tender_data, packaging,"tender_qty","tender_pack_qty")
  
  return(tender_data)
}