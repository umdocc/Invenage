# build tender_track, use config_dict to directly read from database
create_tender_track <- function(sale_log,customer_id = 1){
  
  # build basic details from sale_log
  full_sale_log <- merge(sale_log,pxk_info)
  tmp <- convert_to_pack(full_sale_log,packaging,'qty','pack_qty')
  
  # read only the active tender
  tmp2 <- merge(tender_detail,tender_info %>% 
                  select(tender_id, customer_id, active, warehouse_id))
  tmp2 <- tmp2[tmp2$active==1,]
  tmp2 <- tmp2[tmp2$warehouse_id!=0,]
  tmp2 <- convert_to_pack(
    tmp2,packaging,'tender_qty','tender_pack_qty')
  
  #remove null tender_id
  tmp <- tmp[!is.na(tmp$tender_id),]
  tmp <- tmp %>% group_by(prod_code,tender_id,customer_id,warehouse_id) %>% 
    summarise(total_sale_pack = sum(pack_qty))
  
  tmp <- merge(tmp2, tmp,
               by = c('prod_code','tender_id','customer_id','warehouse_id'),
               all = T)
  
  # calculate final_rm_pack_qty
  tmp$final_rm_pack_qty <- tmp$final_rm_qty/tmp$units_per_pack
  
  # if total_sale_pack/tender_pack_qty is na we use 0
  tmp$total_sale_pack[is.na(tmp$total_sale_pack)] <- 0
  tmp$tender_pack_qty[is.na(tmp$tender_pack_qty)] <- 0
  tmp$tender_pack_remain <- tmp$tender_pack_qty-tmp$total_sale_pack
  
  return(tmp)
}


# update items with tender_id = 0 to correct tender_id using logic
update_tender_id <- function(config_dict, exclude_code=5){
  
  # rebuild sale_log, fill na with default tender_id = 0
  full_sale_log <- merge(sale_log,pxk_info)
  full_sale_log$tender_id[is.na(full_sale_log$tender_id)] <- 0
  full_sale_log <- convert_to_pack(
    full_sale_log, packaging, 'qty', 'sale_qty_pack')
  full_sale_log$new_tender_id <- full_sale_log$tender_id
  
  # we are not interested in items with payment_code = 5 as it is previously 
  # returned
  full_sale_log <- full_sale_log[full_sale_log$payment_code!=5,]
  
  # sort by sale_datetime
  full_sale_log <- full_sale_log[order(full_sale_log$sale_datetime),]
  
  # load the tender track, filter using tender customer only
  tender_track <- create_tender_track(sale_log)
  tender_track$active <- NULL
  tender_customer <- tender_info %>% select(customer_id,active) %>% 
    filter(customer_id!=0) %>% filter(!duplicated(customer_id))
  tmp <- merge(tender_track,tender_customer,by='customer_id',all.x=T)
  tmp <- tmp[!is.na(tmp$active),]
  
  # filter tender_track,
  tmp$final_rm_pack_qty[is.na(tmp$final_rm_pack_qty)] <- 0
  tmp <- tmp[
    (tmp$tender_pack_remain - tmp$final_rm_pack_qty) >0, ]
  tmp <- tmp[order(tmp$tender_id),]

  # run through the tender_track, then check items in sale log, tender_id
  for (i in 1:nrow(tmp)){
    prod_code_2edit <- tmp$prod_code[i]
    customer_2edit <- tmp$customer_id[i]
    amount_cap <- tmp$tender_pack_remain[i]-tmp$final_rm_pack_qty[i]
    test <- full_sale_log[
      full_sale_log$prod_code==prod_code_2edit & 
        full_sale_log$customer_id==customer_2edit & 
        full_sale_log$new_tender_id==0,]
    while ((nrow(test)>0) & (amount_cap>0)){
      print(i)
      print(paste('edit item',test$prod_code[1],'in pxk',test$pxk_num[1]))
      # edit the 1st item full_sale_log
      full_sale_log$new_tender_id[
        full_sale_log$prod_code==prod_code_2edit &
          full_sale_log$customer_id==customer_2edit &
          full_sale_log$new_tender_id==0][1] <- tmp$tender_id[i]
      test <- full_sale_log[ # rebuild the test data
        full_sale_log$prod_code==prod_code_2edit &
          full_sale_log$customer_id==customer_2edit &
          full_sale_log$new_tender_id==0,]
      amount_cap <- amount_cap - full_sale_log$sale_qty_pack[
        full_sale_log$prod_code==prod_code_2edit &
          full_sale_log$customer_id==customer_2edit &
          full_sale_log$new_tender_id==0][1]
    }
  }
  
  record_2edit <- full_sale_log[
    full_sale_log$new_tender_id!=full_sale_log$tender_id,]
  if (nrow(record_2edit)>0){
    # writing to database
    conn <- db_open(config_dict)
    for (i in 1:nrow(record_2edit)){
      query <- paste0(
        "update sale_log set tender_id = ", record_2edit$new_tender_id[i],
        " where pxk_num = ", record_2edit$pxk_num[i], " and prod_code like '",
        record_2edit$prod_code[i],"' and stt = ",record_2edit$stt[i])
      # print(query)
      dbExecute(conn,query)
    }
  }
  dbDisconnect(conn)

}

# this function will create a report for inventory order
create_inv_order_rp <- function(config_dict,tender_include=T){
  rp_data <- update_inventory(config_dict)
  rp_data <- rp_data %>% group_by(prod_code) %>%
    summarise(remaining_qty = sum(remaining_qty)) %>% ungroup
  
  # merge with prod_info so that we get zero items as well
  rp_data <- merge(rp_data,product_info %>% filter(active==T) %>%
                     select(prod_code,type),all.y=T)
  rp_data$remaining_qty[is.na(rp_data$remaining_qty)] <- 0
  
  # add sales_summary
  sales_summary <- get_sales_summary(config_dict)
  rp_data <- merge(rp_data,sales_summary %>% select(
    prod_code,ave_mth_sale), all.x=T)
  
  #add other info, and round up numbers
  rp_data <- add_inv_report_info(rp_data)
  rp_data <- round_report_col(
    rp_data, col_list = c('ave_mth_sale', 'remaining_qty'), decimal = 2)
  
  # include final tender remaining if tender_include = T
  if (tender_include){
  tender_track <- create_tender_track(sale_log)
  tender_track <- tender_track %>% group_by(prod_code) %>% 
    summarise(total_tender_pack_rem = sum(tender_pack_remain))
  rp_data <- merge(rp_data, tender_track, all.x = T)
  }else{rp_data$total_tender_rem <- NA}
  
  # finalise all the required columns
  rp_data <- rp_data %>%
    select(vendor, name, ref_smn, warehouse, remaining_qty,
           unit, ave_mth_sale,total_tender_pack_rem,prod_code)
  # write.xlsx(rp_data,'tongHopDatHang.xlsx')
}