writeout_missing_customer_id <- function(append_pxk_info, output_filename){
  
  # write missing customer id out
  missing_customer_id <- append_pxk_info[append_pxk_info$customer_id==0,]
  missing_customer_id <- missing_customer_id[
    !duplicated(missing_customer_id$customer_name),]
  missing_customer_id <- missing_customer_id %>% 
    select(customer_name, customer_id)
  
  write.xlsx(missing_customer_id, output_filename)
}

extract_pxk_info <- function(pxk_path, config){
  file_list <- list.files(pxk_path)
  file_list <- file_list[grepl(config$pxk_id_string,file_list)]
  
  for (i in 1:length(file_list)){ # loop
    # print(i)
    pxk_filename <- file_list[i]
    raw_data <- read.xlsx(file.path(pxk_path,pxk_filename))
    
    #extracr customer code
    c_code <- raw_data[config$pxk_cust_code_r, config$pxk_cust_code_c]
    c_id <- customer_info$customer_id[customer_info$customer_code==c_code]
    if(length(c_id)==0){c_id <- 0}

    
    
    # extract pxk_num num
    p_num <- raw_data[config$pxk_num_cell_r, config$pxk_num_cell_c]
    # extract sale_date
    sale_dt <- raw_data[config$pxk_sale_date_r, config$pxk_sale_date_c]
    sale_dt <- as.POSIXlt(sale_dt, format="%d-%m-%Y")
    
    #extract payment type
    pay_t <- raw_data[config$pxk_pay_type_r,config$pxk_pay_type_c]
    if(!is.na(pay_t)){
      pay_code <- payment_type$payment_code[payment_type$actual==pay_t]
      if(length(pay_code)==0){pay_code <- NA}
    }
    
    # gather all info
    tmp_info <- data.frame(
      pxk_num = as.integer(p_num), 
      customer_id = as.integer(c_id), 
      completed = 1, 
      sale_datetime = sale_dt,
      payment_code = as.integer(pay_code), 
      admin_id = 1
    )
    if(i==1){
      append_pxk_info <- tmp_info
    }else{
      append_pxk_info <- rbind(append_pxk_info, tmp_info)
    }
  }
  append_pxk_info <- append_pxk_info[!is.na(append_pxk_info$pxk_num),]
  append_pxk_info <- append_pxk_info[!duplicated(append_pxk_info$pxk_num),]
  
  return(append_pxk_info)
}

