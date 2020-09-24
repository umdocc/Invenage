# stats.R contains all function used to summarise data

# this function return the entire sale data summarise the data by prod_code,
# providing all summary for each product
get_sale_summary <- function(
  vendor_id=0, from_date=Sys.Date(), group_cols = c('prod_code','customer_id')){
  sale_data <- merge(
    sale_log,pxk_info %>% select(pxk_num,customer_id,sale_datetime))
  sale_data <- merge(sale_data, product_info %>% select(prod_code,vendor_id))
  sale_data$total_price <- sale_data$unit_price*sale_data$qty
  
  # filter the vendor if needed
  if (vendor_id!=0){
    sale_data <- sale_data[sale_data$vendor_id==vendor_id,]
  }

  # filter using from_date
  sale_data <- sale_data[sale_data$sale_datetime>as.POSIXct(from_date),]

  # convert to pack
  sale_data <- convert_to_pack(sale_data,packaging,'qty','pack_qty')
  
  # by default we group by prod_code and customer_id
  sale_data <- sale_data %>% group_by_at(group_cols) %>%
    summarise(total_sale_pack = sum(pack_qty),.groups = 'drop')

  # return
  return(sale_data)
}
