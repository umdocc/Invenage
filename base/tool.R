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
  # check if all items in packaging has an ordering unit
  # gbl_load_tbl("packaging")
  # ordering_unit <- get_ordering_unit(packaging)
  # tmp <- packaging[!duplicated(packaging$prod_code),]
  # test <- merge(tmp,ordering_unit %>% select(prod_code,ordering_unit),
  #               all.x = T)
  # test[is.na(test$ordering_unit),]
  
  # check product_info for duplications
  tmp <- product_info[duplicated(product_info %>% select(ref_smn, vendor_id, active)),]
  if(nrow(tmp)>0){
    stop("duplications found in product_info table!!!!!!!!!!!!!!!!!!!!")
    print(tmp)
  }
  # check import_price for duplications
  tmp <- import_price[
    duplicated(import_price %>% 
                 select(prod_code, min_order, source_name, vendor_id)),]
  if(nrow(tmp)>0){
    stop("duplications found in import_price table!!!!!!!!!!!!!!!!!!!!")
    print(tmp)
  }
}

# generate per customer pack prices by aggregating sale_log
gen_customer_pricelist <- function(
  sale_data,group_vector=c("prod_code","customer_id"),
  display_mode="full",extra_price="latest",add_info=NULL, vendor_id_list=0){
  
  data_df <- convert_to_pack(sale_data,packaging,"qty","pack_qty")
  data_df <- data_df[!is.na(data_df$unit_price)&
                       data_df$unit_price>=0,]
  data_df$pack_price <- data_df$unit_price*data_df$units_per_pack
  
  latest_price <- data_df %>% group_by_at(group_vector)%>% 
    filter(sale_datetime==max(sale_datetime)) 
  latest_price <- latest_price[,c(group_vector,"pack_price")] %>% 
    rename("latest_price"="pack_price")
  
  data_df <- data_df %>% group_by_at(group_vector) %>% 
    summarise(min_price=min(pack_price),mid_price=median(pack_price),
              max_price=max(pack_price))
  
  if("latest" %in% extra_price){
    data_df <- merge(data_df,latest_price,all.x=T)
  }
  
  
  
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
  
  # add vendor name if needed
  if("vendor" %in% add_info){
    data_df <- merge(
      data_df, product_info %>% select(prod_code,vendor_id))
    
    # if the vendor_id is not 0, filter to selected vendor only
    if(vendor_id_list!=0){
      data_df <- data_df[data_df$vendor_id %in% vendor_id_list,]
    }
    
    data_df <- merge(
      data_df, vendor_info %>% select(vendor_id,vendor))
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

# set error_free variable
gbl_set_error_free <- function(ef_value){
  gbl_write_var("error_free",ef_value)
}

check_required_col <- function(
    col_list, input_df,set_gbl_var=T, stop_app=F, warn_mode="shiny"){
  error_free <- T
  # check for required column
  for(col_name in col_list){
    print(col_name)
    if (!(col_name %in% names(input_df))){
      error_msg <- paste("Required column",col_name,"not found")
      if(warn_mode=="shiny"){
        shinyalert(uielem$error, error_msg, type = "error")
      }
      error_free <- F
      if(stop_app){
        stop(error_msg)
      }
    }
  }
  if(set_gbl_var){
    gbl_write_var("error_free",error_free)
  }
}

get_vendor_id <- function(vendor_name){
  vendor_id <- vendor_info$vendor_id[grepl(vendor_name,vendor_info$vendor)]
  if(length(vendor_id)!=1){
    stop("No/Multiple vendor id found")
  }
  return(vendor_id)
}

get_customer_id <- function(customer_name){
  customer_id <- customer_info$customer_id[
    grepl(customer_name,customer_info$customer_name)]
  if(length(customer_id)==0){
    stop("No customer id found")
  }
  if(length(customer_id)>1){
    stop("Multiple customer id found")
  }
  
  return(customer_id)
}

# this function will check the table in db specified in tbl_name and clean all 
# duplicated entries found
load_tbl_and_clean_duplicated <- function(tbl_name,column_list){
  gbl_load_tbl(tbl_name)
  tmp <- get(tbl_name)
  tmp <- tmp[duplicated(tmp[,column_list]),]
  if(nrow(tmp)>0 & ("id" %in% names(tmp))){
    conn <- db_open()
    for (i in 1:nrow(tmp)){
      query <- paste0("delete from ", tbl_name," where id=",tmp$id[i])
      dbExecute(conn,query)
    }
    dbDisconnect(conn)
    gbl_load_tbl(tbl_name)
  }
}

# generate a pxk file using only pxk_num and the sale_log in memory
# used on both cdn and slr
print_pxk <- function(pxk_num, open_file = T){
  pxk_data <- sale_log[sale_log$pxk_num == pxk_num,]
  
  form_path <- file.path(config$form_path,"pxk_form.xlsx")
  
  # check input output
  if(file.exists(form_path) & file.exists(config$pxk_out_path)){
    dest_path <- file.path(
      config$pxk_out_path,
      paste0(config$company_name,".",pxk_num,".xlsx"))
  }else{
    gbl_set_error_free(F)
    print("cannot find form path or dest path")
    show_error("path_notfound")
  }
  
  
  if(error_free){
    
    wb <- loadWorkbook(form_path)
    
    # get the expDate, if a Lot has 2 expDate, select only the 1st
    # need to get all items, not just positive ones
    tmp <- import_log
    exp_date <- tmp %>% select(prod_code,lot,exp_date) %>% unique()
    exp_date <- exp_date[!duplicated(exp_date$lot),] %>% 
      select(prod_code,lot,exp_date)
    
    # writing customer_name
    customer_name <- customer_info$customer_name[
      customer_info$customer_id == pxk_data$customer_id[1]]
    wb <- write_excel(
      wb, customer_name, config$pxk_customer_coor)
    
    # append the customer code if needed
    if(config$add_customer_code=="TRUE"){
      
      # read the customer code, then write it to the cell next to customer name
      wb <- write_excel(wb, customer_info$customer_code[
        customer_info$customer_name==customer_name], 
        config$pxk_customer_code_coor)
    }
    
    # writing pxkNum
    wb <- write_excel(wb, pxk_num, config$pxk_num_coor)
    
    # writing current date
    wb <- write_excel(wb, format(Sys.Date(),config$date_format), 
                      config$pxk_date_coor)
    
    # writing payment type
    payment_str <- payment_type$actual[
      payment_type$payment_code == pxk_data$payment_code[1]]
    wb <- write_excel(
      wb, payment_str, config$pxk_payment_coor)
    
    # writing data
    ## convert other info for display purpose
    pxk_data$dqty <- formatC(
      pxk_data$qty,format='f',big.mark=",",digits = 2)
    pxk_data$total_cost <- as.numeric(pxk_data$unit_price)*
      as.numeric(pxk_data$qty)
    
    # clean up big unit
    pxk_data$dqty <- gsub('\\.00','',pxk_data$dqty)
    pxk_data$dSL <- paste(pxk_data$dqty, pxk_data$unit)
    
    pxk_data$dunit_price <- paste(
      formatC(pxk_data$unit_price,format='f',
              big.mark=",",digits = 0),
      pxk_data$unit, sep='/')
    pxk_data$a_note <- ''
    
    # automatically note if unit is not ordering unit
    ordering_unit <- get_ordering_unit(packaging) %>% select(prod_code,unit)
    names(ordering_unit) <- c('prod_code','ordering_unit')
    pxk_data <- convert_to_pack(pxk_data,packaging,'qty','pack_qty')
    if(!all(pxk_data$units_per_pack==1)){
      # create converted display amount
      pxk_data <- merge(pxk_data,ordering_unit, all.x=T)
      pxk_data$a_note <- paste(pxk_data$pack_qty,
                               pxk_data$ordering_unit)
      pxk_data$a_note[pxk_data$units_per_pack==1] <- ''
      pxk_data$note <- paste(pxk_data$a_note,
                             pxk_data$note)
    }
    
    # arrange & select columns for writing
    pxk_data <- merge(pxk_data,product_info %>% 
                        select(prod_code,comm_name,ref_smn))
    
    # fix nolot not displaying
    pxk_nolot <- pxk_data %>% filter(lot == "nolot" | is.na(lot) | lot == "")
    
    pxk_data <- merge(pxk_data, exp_date)
    
    if(nrow(pxk_nolot)>0){# fix nolot not displaying
      pxk_nolot$exp_date <- NA
      pxk_data <- rbind(pxk_data, pxk_nolot)
    }
    pxk_data <- pxk_data[order(as.numeric(pxk_data$stt)),]
    pxk_data <- pxk_data[
      ,unlist(strsplit(config$pxk_display_col,split=";"))]
    
    
    # write data
    wb <- write_excel(wb,pxk_data,config$pxk_data_coor)
    
    # save the excel sheet
    saveWorkbook(wb,file=dest_path,overwrite = T)
    
    #open the file if open_file=T
    if(open_file){
      open_location(dest_path)
    }
  }
  
}
