# stats.R contains all function to extract data from database for report/stats purpose
# it will mostly assume boot.R has been executed

# this function return the entire sale data, 
# with all id/codes reaplaced with names/str
get_sale_data <- function(vendor='all',from_date){
  sale_data <- merge(
    sale_log,pxk_info %>% select(pxk_num,customer_id,sale_datetime))
  sale_data <- merge(sale_data, product_info %>% select(prod_code,vendor_id))
  sale_data <- merge(sale_data, vendor_info %>% select(vendor,vendor_id))
  sale_data$total_price <- sale_data$unit_price*sale_data$qty
  # filter the vendor if needed
  if (vendor!='all'){
    sale_data <- sale_data[grepl(vendor,sale_data$vendor),]
  }
  
  # filter using from_date
  sale_data <- sale_data[sale_data$sale_datetime>as.POSIXct(from_date),]
  
  # get the customer name
  sale_data <- merge(sale_data,customer_info %>% select(customer_id,customer_name))
  
  # return
  return(sale_data)
}
