# function to replace a pattern, default replacement to "", use with mapply
rep_vector <- function(pattern_col,target_col,replacement=""){
  gsub(pattern_col,replacement,target_col)
}

# check if a list of columns exist on a data frame
col_exist <- function(data_frame,col_list){
  col_exist <- F
  
  return(colname_not_exist)
}

# function to add import license data to a data frame of products
add_import_lic_info <- function(product_data, import_lic_data){
  required_col <- c("ref_smn","vendor_id")
}

# translate column
translate_tbl_column <- function(input_df,ui_elem=uielem){
  for (i in 1:length(input_df)){
    if (names(input_df)[i] %in% names(ui_elem)){
      names(input_df)[i] = ui_elem[names(input_df)[i]]
    }
  }
  return(input_df)
}

# write a single cell or a block to excel, using wb object, 
# and a starting_coordinate 
write_excel <- function(
  wb, written_data, start_cell_coor="1;1", sheet_num=1, split_char=";",
  include_colname=F){
  if(grepl(split_char, start_cell_coor)){
    start_cell_coor <- as.numeric(
      unlist(strsplit(start_cell_coor,split=split_char)))
  }
  writeData(wb,sheet=sheet_num,written_data, 
            startRow=start_cell_coor[1], 
            startCol=start_cell_coor[2], 
            colNames = include_colname)
  return(wb)
}

db_integrity_check <- function(){
  conn <- db_open()
  # check if all items in packaging has an ordering unit
  db_load_simple_tbl("packaging")
  ordering_unit <- get_ordering_unit(packaging)
  tmp <- packaging[!duplicated(packaging$prod_code),]
  test <- merge(tmp,ordering_unit %>% select(prod_code,ordering_unit),
                all.x = T)
  test[is.na(test$ordering_unit),]
  dbDisconnect(conn)
}

# generate per customer pack prices by aggregating sale_log
gen_customer_pricelist <- function(
  sale_data,group_vector=c("prod_code","customer_id"),
  display_mode="full"){
  
  data_df <- convert_to_pack(sale_data,packaging,"qty","pack_qty")
  data_df <- data_df[!is.na(data_df$unit_price)&
                       data_df$unit_price>=0,]
  data_df$pack_price <- data_df$unit_price*data_df$units_per_pack
  data_df <- data_df %>% group_by_at(group_vector) %>% 
    summarise(min_price=min(pack_price),mid_price=median(pack_price),
              max_price=max(pack_price))
  
  # add display info if needed
  if(display_mode=="full"){
    # translate prod_code to comm_name, vendor and ref
    data_df <- merge(
      data_df,product_info %>% select(prod_code, comm_name, ref_smn),
      all.x=T)
    
    # translate customer name if needed
    if("customer_id" %in% group_vector){
      data_df <- merge(
        data_df,customer_info %>% select(customer_id,customer_name))
    }
  }
  
  # label for consistency
  data_df$singular_price <- T
  data_df$singular_price[data_df$min_price!=data_df$max_price] <- F
  return(data_df)
  
}

open_location <- function(location_path, timeout=2){
  system2('open',location_path)
}

gbl_write_var <- function(var_name, var_data){
  assign(var_name,var_data,envir=globalenv())
}

show_error <- function(big_msg,small_msg){
  shinyalert(big_msg, small_msg, type = "error")
}