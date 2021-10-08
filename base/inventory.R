get_sale_data <- function(vendor_id,from_date,to_date){
  return(db_read_query(paste0(
    "select sale_log.prod_code, sale_log.unit, sale_log.qty, 
      pxk_info.sale_datetime, product_info.ref_smn
      from sale_log inner join product_info 
      on sale_log.prod_code=product_info.prod_code
      inner join pxk_info
      on sale_log.pxk_num = pxk_info.pxk_num
      where vendor_id=",vendor_id," and
      pxk_info.sale_datetime between '",from_date,
      "' and '",to_date,"'"))
  )
}

update_inventory <- function(pos_item=TRUE, summarised = FALSE,
                             to_date = Sys.Date(), from_date="1900-01-01"){
  # read import_log
  tmp <- db_read_query(paste0(
    "select * from import_log where delivery_date between '",from_date,
    "' and '",to_date,"'"
  ))
  
  tmp <- convert_to_pack(tmp,packaging,'qty','importQty')
  tmp <- tmp %>% group_by(prod_code,unit,lot,warehouse_id) %>% 
    summarise(totalImportQty = sum(importQty), .groups = 'drop')
  
  # process sale_log as tmp2
  tmp2 <- db_read_query(paste0(
    "select * from sale_log inner join pxk_info 
    on sale_log.pxk_num = pxk_info.pxk_num 
    where pxk_info.sale_datetime between '",from_date,"' and '",
    to_date+1,"'"))
  # for sale_log we need to merge with warehouse_id
  tmp2 <- convert_to_pack(tmp2,packaging,'qty','saleQty')
  tmp2 <- tmp2 %>% group_by(prod_code,unit,lot,warehouse_id) %>% 
    summarise(totalSaleQty = sum(saleQty), .groups = 'drop')
  totalInventory <- merge(tmp,tmp2,all=T,
                          by=c('prod_code','unit','lot','warehouse_id'))
  totalInventory$totalSaleQty[is.na(totalInventory$totalSaleQty)] <- 0
  totalInventory$totalImportQty[is.na(totalInventory$totalImportQty)] <- 0
  totalInventory$remaining_qty <- totalInventory$totalImportQty - 
    totalInventory$totalSaleQty
  
  # keep only the available items
  if (pos_item){
    threshold <- 0.001
    totalInventory <- totalInventory[
      totalInventory$remaining_qty>threshold,] %>% distinct()
  }
  
  # recover the exp_date
  exp_dateData <- import_log[!duplicated(import_log[c('prod_code','lot')]),] %>% 
    select(prod_code,lot,exp_date) %>% distinct()
  
  # merge, distinct and remove NA
  totalInventory <- merge(totalInventory,exp_dateData,all.x=T) %>% distinct()
  inventory <- totalInventory[!is.na(totalInventory$prod_code),]
  
  # calculate the intexp_date, which is the exp_date in standard format
  inventory$exp_date <- gsub('/','-',inventory$exp_date)
  inventory$exp_date <- gsub(' .*$','',inventory$exp_date)
  inventory$intexp_date <- parse_date_time(
    inventory$exp_date,c('%Y-%m','%m-%Y','%d-%m-%Y','%Y-%m-%d'))
  
  # cleaning
  inventory <- inventory[!is.na(inventory$prod_code),] #remove NA
  
  # we will no longer need the unit, as everything should be in ordering_unit
  inventory$unit <- NULL
  
  return(inventory)
}