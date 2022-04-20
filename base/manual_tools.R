# read in data and check required columns
manual_po_read <- function(po_filepath,search_key="No",
                           remove_invalid=T){
  
  start_row <- 1
  while(length(start_row)>0){
    po_data <- read.xlsx(po_filepath, startRow = start_row)
    start_row <- start_row + which(po_data==search_key)
    }
  required_cols <- unlist(strsplit(config$po_data_colnames,";"))
  names(po_data)[1:8] <- required_cols
  
  # keep only required col and clean up
  po_data <- po_data[,required_cols]
  
  # add prod_code and check for invalid product
  po_data <- merge(po_data,product_info %>%
                     select(ref_smn,vendor_id,prod_code),
                   all.x=T)
  if(remove_invalid){
    po_data <- po_data[!is.na(po_data$prod_code),]
  }
  return(po_data)
}

manual_add_unit_cost <- function(po_name,po_data){
  append_unit_cost <- db_read_query(
  paste0("select * from import_log where po_name='",po_name,"'"))
  append_unit_cost <- append_unit_cost %>% rename(old_unit_cost=actual_unit_cost)
  append_unit_cost <- merge(
    append_unit_cost, 
    po_data %>% select(prod_code, po_name, qty, lot, actual_unit_cost), 
                            all.x = T)
  
  # be defensive and filter out uneeded stuffs
  append_unit_cost <- append_unit_cost[!is.na(append_unit_cost$id),]
  append_unit_cost <- append_unit_cost[is.na(append_unit_cost$old_unit_cost),]
  append_unit_cost <- append_unit_cost[
    !is.na(append_unit_cost$actual_unit_cost),]
  
  
  for(i in 1:nrow(append_unit_cost)){
    query <- paste0("update import_log set actual_unit_cost=",append_unit_cost$actual_unit_cost[i],
                    " where id=",append_unit_cost$id[i])
    print(query)
    db_exec_query(query)
  }
}