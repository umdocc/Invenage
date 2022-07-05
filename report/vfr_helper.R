# vendor_id <- 2
# rollback_period <- 360
vfr_get_data <- function(vendor_id = 0, rollback_period = 360){
  # get current date and subtract rollback
  start_date <- Sys.Date() - rollback_period
  rbperiod_mth <- rollback_period/30
  
  # check recent import_log and sale and merge into initial report data
  import_data <- get_aii_import_data(
    from_date = start_date, for_display = F)
  import_data <- convert_to_pack(import_data, packaging, "qty","pack_qty")
  import_data <- import_data %>% group_by(prod_code) %>%
    summarise(total_import = sum(pack_qty))
  sale_data <- get_sale_data(from_date = start_date)
  sale_data <- convert_to_pack(sale_data, packaging, "qty","pack_qty")
  sale_data <- sale_data %>% group_by(prod_code) %>%
    summarise(total_sale = sum(pack_qty))
  report_data <- merge(sale_data, import_data, all=T)
  
  # create product filtering based on vendor_id, then filter by merge
  prod_filter <- product_info %>% select(prod_code, vendor_id)
  if(vendor_id!=0){
    prod_filter <- prod_filter[prod_filter$vendor_id==vendor_id,]
  }
  
  report_data <- merge(report_data, prod_filter)
  
  return(report_data)
  
}