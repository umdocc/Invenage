# debug_var
input_vendor_id <- 6
from_date <- '2020-01-01'
to_date <- "2020-12-31"


# generate the po_report used for placing a po
print_po_report <- function(input_vendor_id){
  query <- paste0(
    "select * from product_info where vendor_id=", input_vendor_id,
    " and active=1")
  product_list <- db_read_query(query)
  
}

# generate the report on sales
get_sales_report <- function(input_vendor_id, from_date, to_date){
  # extract the sales data
  query <- paste0(
    "select sale_log.prod_code, sale_log.unit, sale_log.qty,
    sale_log.pxk_num from sale_log inner join product_info 
    on sale_log.prod_code=product_info.prod_code
    inner join pxk_info on sale_log.pxk_num=pxk_info.pxk_num
    where product_info.vendor_id=",input_vendor_id
    ," and pxk_info.sale_datetime between '",from_date,"' and '",to_date,"'")
  sale_data <- db_read_query(query)
}