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

glb_update_inventory <- function(pos_item=TRUE, summarised = FALSE,
                             to_date = Sys.Date(), from_date="1900-01-01"){
  # read import and sale log

  tmp <- import_log[import_log$delivery_date<(as.Date(to_date)+1),]
  tmp2 <- sale_log[sale_log$sale_datetime<(as.Date(to_date)+1),]

  tmp <- convert_to_pack(tmp,packaging,'qty','importQty')
  tmp <- tmp %>% group_by(prod_code,unit,lot,warehouse_id) %>% 
    summarise(totalImportQty = sum(importQty), .groups = 'drop')
  
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
  exp_dateData <- get_exp_date()
  
  # merge, distinct and remove NA
  totalInventory <- merge(totalInventory,exp_dateData,all.x=T) %>% distinct()
  inventory <- totalInventory[!is.na(totalInventory$prod_code),]
  
  # cleaning
  inventory <- inventory[!is.na(inventory$prod_code),] #remove NA
  
  # we will no longer need the unit, as everything should be in ordering_unit
  inventory$unit <- NULL
  
  assign("inventory",inventory,envir = globalenv())
}

get_exp_date <- function(){
  import_total <- db_read_query("select * from import_log")
  # recover the exp_date
  exp_dateData <- import_total[
    !duplicated(import_total[c('prod_code','lot')]),] %>% 
    select(prod_code,lot,exp_date) %>% distinct()
  
  # calculate the intexp_date, which is the exp_date in standard format
  exp_dateData$exp_date <- gsub('/','-',exp_dateData$exp_date)
  exp_dateData$exp_date <- gsub(' .*$','',exp_dateData$exp_date)
  exp_dateData$intexp_date <- parse_date_time(
    exp_dateData$exp_date,c('%Y-%m','%m-%Y','%d-%m-%Y','%Y-%m-%d'))
  
  return(exp_dateData)
}

get_ordering_unit <- function(packaging){
  ordering_unit <- packaging[packaging$ordering_unit==1,]
  # we no longer need this after fixing database
  ordering_unit <- ordering_unit[!duplicated(ordering_unit$prod_code),]
  return(ordering_unit)
}