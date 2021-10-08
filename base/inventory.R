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

update_inventory <- function(){
  
}