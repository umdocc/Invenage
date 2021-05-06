

# function to build estimated import cost from import_log
get_est_import_cost <- function(import_log, algorithm='weighted_average'){
  if (algorithm=='weighted_average'){
    import_log <- convert_to_pack(import_log,packaging,stringSL='qty',
                               packString = 'pack_qty')
    import_log$pack_import_cost <-
      import_log$actual_unit_cost*import_log$units_per_pack
    import_log$total_import_cost <-
      import_log$pack_import_cost*import_log$pack_qty
    tmp <- import_log %>% group_by(prod_code,lot) %>%
      summarise(total_pack = sum(pack_qty),
                sum_import_cost = sum(total_import_cost), .groups = 'drop') %>%
      ungroup
    tmp$ave_pack_import_cost <- tmp$sum_import_cost/tmp$total_pack
    tmp <- tmp %>% select(prod_code,lot,ave_pack_import_cost)
    return(tmp)
  }
}

get_current_pricelist <- function(
  sale_log, pxk_info, customer_info, product_info){
  tmp <- sale_log[!is.na(sale_log$unit_price),]
  tmp <- tmp[tmp$unit_price>0,]
  tmp <- merge(tmp,pxk_info %>% select(pxk_num,customer_id,sale_datetime))
  tmp$sale_datetime <- strptime(tmp$sale_datetime,'%Y-%m-%d %H:%M:%S')
  tmp$sale_datetime <- as.POSIXct(tmp$sale_datetime)
  tmp <- merge(tmp,customer_info %>% select(customer_id,customer_name))
  tmp <- tmp %>% group_by(customer_id, prod_code,unit) %>% mutate(
    latest_date=max(sale_datetime))
  tmp$latest_price <- tmp$sale_datetime==tmp$latest_date
  tmp <- tmp[tmp$latest_price,]
  tmp <- merge(tmp,product_info %>% select(prod_code,name,vendor,ref_smn))
  tmp <- tmp %>% 
    select(customer_name,name,ref_smn,unit,unit_price,latest_date) %>%
    arrange(customer_name,name)
  # write.xlsx(tmp,'~/Downloads/price_list.xlsx')
  return(tmp)
}



# a function to replace duplicated line with NA
clean_duplicates <- function(
  data_df, col_list = c('customer_name','sale_date','pxk_num')){
  i <- nrow(data_df)
  while (i>=2){
    if (data_df[(i-1),col_list]==data_df[i,col_list]){
      data_df[i,col_list] <- NA
    }
    i <- i-1
  }
  return(data_df)
}

# translate column
translate_tbl_column <- function(input_df,ui_elem){
  for (i in 1:length(input_df)){
    if (length(ui_elem$actual[ui_elem$label==names(input_df)[i]])==1){
      names(input_df)[i] = ui_elem$actual[
        ui_elem$label==names(input_df)[i]]
    }
  }
  return(input_df)
}

# reverse the translated column
rev_trans_tbl_column <- function(input_df,ui_elem){
  for (i in 1:length(input_df)){
    if (length(ui_elem$label[ui_elem$actual==names(input_df)[i]])==1){
      names(input_df)[i] = ui_elem$label[
        ui_elem$actual==names(input_df)[i]]
    }
  }
  return(input_df)
}

get_pxk_entry_num <- function(selected_pxk_num,config_dict){
  conn <- db_open(config_dict)
  sale_log <- dbReadTable(conn,'sale_log')
  dbDisconnect(conn)
  output_pxk <- sale_log[sale_log$pxk_num==selected_pxk_num,]
  entry_list <- output_pxk$stt
  return(entry_list)
}

# create a list of ordering_unit based on packaging
get_ordering_unit <- function(packaging){
  ordering_unit <- packaging[
    packaging$units_per_pack==1, c('prod_code', 'unit')]
  ordering_unit <- ordering_unit[ordering_unit$unit!='pack',]
  ordering_unit <- ordering_unit[!duplicated(ordering_unit$prod_code),]
  return(ordering_unit)
}

# check the status of pxk_num
check_pxk_num <- function(selected_pxk_num,config_dict){
  conn <- db_open(config_dict)
  pxk_info <- dbReadTable(conn,"pxk_info")
  dbDisconnect(conn)
  pxk_status <- 'completed'
  complete_code <- pxk_info$completed[pxk_info$pxk_num==selected_pxk_num]
  if (length(complete_code)==0){
    pxk_status <- 'new'
  }else{
    if (complete_code==0){
      pxk_status <- 'in_progress'
    }
  }
  return(pxk_status)
}

get_latest_unit <- function(customer_id, prod_code, sale_log,pxk_info){
  sale_lookup <- merge(sale_log,pxk_info,on='pxk_num',all.x=T)
  # filter through sale_lookup to find price
  tmp <- sale_lookup[sale_lookup$prod_code == prod_code & 
                       sale_lookup$customer_id == customer_id,]
  tmp <- tmp[!is.na(tmp$sale_datetime),]
  # if we can find something, update priority unit
  if (nrow(tmp)>0){
    tmp <- tmp %>% select(pxk_num, unit, sale_datetime)
    if (class(tmp$sale_datetime) == "character"){
      tmp$sale_datetime <- strptime(tmp$sale_datetime,'%Y-%m-%d %H:%M:%S')
    }
    latest_unit <- tmp$unit[
      tmp$sale_datetime == max(tmp$sale_datetime)]
    return(latest_unit)
  }else{
    return(NULL)
  }
  
}
