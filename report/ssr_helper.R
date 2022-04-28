# the sale_stats_report deals with all type of sale statistics

ssr_get_sale_report <- function(){
sale_report <- convert_to_pack(sale_log, packaging,"qty","pack_qty")

sale_report$month <- month(sale_report$sale_datetime)
sale_report$year <- year(sale_report$sale_datetime)

# full year or not
tmp1 <- sale_report %>% group_by(year,month) %>% summarise(total_row=1)
tmp1 <- tmp1 %>% group_by(year) %>% summarise(total_month=sum(total_row))
sale_report <- merge(sale_report,tmp1,all.x=T)

sale_report <- ssr_get_yoy_report(sale_report)
}

ssr_get_yoy_report <- function(sale_report){
  
  sale_report <- sale_report %>% filter(total_month==12)
  
  sale_report <- sale_report %>% group_by(year,prod_code) %>%
    summarise(total_pack=sum(pack_qty))
  
  # adding more data for filtering
  sale_report <- merge(sale_report,product_info %>% select(prod_code,vendor_id),
                       all.x=T)
  sale_report <- sale_report %>% filter(vendor_id==1) %>% 
    select(year,prod_code,total_pack)
  
  sale_report <- reshape(sale_report,idvar = "prod_code",timevar = "year",
                         direction = "wide")
  
  sale_report <- merge(sale_report,product_info %>% select(prod_code,comm_name),
                       all.x=T) %>% select(prod_code, comm_name, everything())
  
  return(sale_report)
}