# get sale price statistics for vendor_id
get_sale_price_stat <- function(vendor_id){
  tmp <- merge(sale_log, product_info %>% 
                 select(prod_code, vendor_id, comm_name, ref_smn))
  tmp <- tmp[tmp$vendor_id==vendor_id,]
  
  price_stat <- convert_to_pack(tmp, packaging, "qty", "pack_qty")
  price_stat$pack_price <- price_stat$unit_price*price_stat$units_per_pack
  price_stat <- price_stat[!is.na(price_stat$pack_price),]
  
  price_stat <- price_stat %>% group_by(prod_code, comm_name, ref_smn) %>%
    summarise(min_price = min(pack_price), 
              max_price = max(pack_price))
  
  return(price_stat)
}