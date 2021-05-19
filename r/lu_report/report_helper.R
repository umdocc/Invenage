create_po_report <- function(input){
  vendor_name <- input$por_vendor_select
  
  #debug var
  vendor_name <- 'Biolabo-Phap';to_date<-Sys.Date()
  from_date <- Sys.Date()-365
  
  vendor_id <- vendor_info$vendor_id[vendor_info$vendor==vendor_name]
  total_sales <- get_total_sales(
    vendor_id, from_date=(Sys.Date()-365),to_date=Sys.Date())
}


# get_total_sales summarise the total sales by pack, input:
# vendor_id: default to 0 to summarise all vendor
# from_date to_date: filtering data based on sale date range
# group_cols: how the summarise should be grouped
get_total_sales <- function(
  vendor_id=0, from_date=Sys.Date(), to_date = Sys.Date(),
  group_cols = c('prod_code','customer_id')){
  
  # read database
  sale_data <- db_read_query(
    'select * from sale_log inner join pxk_info
  on sale_log.pxk_num=pxk_info.pxk_num
  inner join product_info
  on sale_log.prod_code=product_info.prod_code')
  
  sale_data$total_price <- sale_data$unit_price*sale_data$qty
  
  # filter the vendor if needed
  if (vendor_id!=0){
    sale_data <- sale_data[sale_data$vendor_id==vendor_id,]
  }
  
  # filter using from_date & to_date
  sale_data <- sale_data[sale_data$sale_datetime>as.POSIXct(from_date),]
  sale_data <- sale_data[
    sale_data$sale_datetime<(as.POSIXct(to_date)+24*60*60),]
  
  # convert to pack
  sale_data <- convert_to_pack(sale_data,packaging,'qty','pack_qty')
  
  # by default we group by prod_code and customer_id
  sale_data <- sale_data %>% group_by_at(group_cols) %>%
    summarise(total_sale_pack = sum(pack_qty),.groups = 'drop')
  
  # return
  return(sale_data)
}