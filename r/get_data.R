# functions to read various data from database
get_sales_data <- function(
  from_date='2019-07-01',to_date='2020-06-30',vendor_id=0,prod_code='all',
  customer_id=0,sum_type='pack'){
  
  # create the base output table
  # first merge with other information tables
  output_tbl <- merge(sale_log,pxk_info,all.x=T)
  output_tbl <- merge(output_tbl,product_info %>% select(prod_code,vendor_id))
  #filter on from_date and to_date
  output_tbl <- output_tbl[output_tbl$sale_datetime>=as.Date(from_date)&
                             output_tbl$sale_datetime<=as.Date(to_date),]
  # filter on vendor
  if(vendor_id!=0 & nrow(output_tbl)>0){
    output_tbl <- output_tbl[output_tbl$vendor_id==vendor_id,]
  }
  # filter on customer_id
  if(customer_id!=0 & nrow(output_tbl)>0){
    output_tbl <- output_tbl[output_tbl$customer_id==customer_id,]
  }
  # filter on prod_code
  if(prod_code!='all' & nrow(output_tbl)>0){
    output_tbl <- output_tbl[output_tbl$prod_code==prod_code,]
  }
  
  # convert to pack if sum_type = 'pack'
  if(sum_type=='pack' & nrow(output_tbl)>0){
      output_tbl <- convert_to_pack(output_tbl,packaging,'qty','pack_qty')
  }
  
  # return
  return(output_tbl)
}