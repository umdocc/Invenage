# create custom inventory report
# should be included in future version of creating report
# due to historical reason, including a from_date element will
# cause incompatibility with majority of report functions
# thus we choose to use a separate function for this task

# extract the total inventory from and to date
get_total_inventory <- function(
    from_date="1900-01-01", to_date=Sys.Date(), summarised = FALSE){

  # comment out on variables
  to_date<-Sys.Date(); from_date<-"2025-01-01"
  
  # process import_log into packs
  tmp <- import_log[
    import_log$delivery_date<(as.Date(to_date)+1) & 
      import_log$delivery_date>=(as.Date(from_date)),]
  tmp <- convert_to_pack(tmp,packaging,'qty','importQty')
  tmp <- tmp %>% group_by(prod_code,unit,lot,warehouse_id) %>% 
    summarise(totalImportQty = sum(importQty), .groups = 'drop')
  
  # process sale_log into packs
  tmp2 <- sale_log[
    sale_log$sale_datetime<(as.Date(to_date)+1) &
      sale_log$sale_datetime>=(as.Date(from_date)),]
  
  # for sale_log we need to merge with warehouse_id
  tmp2 <- convert_to_pack(tmp2,packaging,'qty','saleQty')
  tmp2 <- tmp2 %>% group_by(prod_code,unit,lot,warehouse_id) %>% 
    summarise(totalSaleQty = sum(saleQty), .groups = 'drop')
  totalInventory <- merge(tmp,tmp2,all=T,
                          by=c('prod_code','unit','lot','warehouse_id'))
  return(totalInventory)  
  
}

