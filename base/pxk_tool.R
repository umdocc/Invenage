# generate a pxk file using only pxk_num and the sale_log in memory
# used on both cdn and slr
print_pxk <- function(pxk_num, open_file = T){
  
  # check the required files for writing pxk
  form_path <- file.path(config$form_path,"pxk_form.xlsx")
  dest_path <- check_pxk_io(pxk_num, form_path)
  
  if(error_free){
    
    # prepare data
    pxk_data <- sale_log[sale_log$pxk_num == pxk_num,]
    wb <- loadWorkbook(form_path)
    
    # write pxk meta data
    wb <- write_pxk_meta(wb, pxk_data)
    
    # get the expDate, if a Lot has 2 expDate, select only the 1st
    # need to get all items, not just positive ones
    tmp <- import_log
    exp_date <- tmp %>% select(prod_code,lot,exp_date) %>% unique()
    exp_date <- exp_date[!duplicated(exp_date$lot),] %>% 
      select(prod_code,lot,exp_date)
    
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

check_pxk_io <- function(pxk_num, form_path){
  # check input output
  if(file.exists(form_path) & file.exists(config$pxk_out_path)){
    dest_path <- file.path(
      config$pxk_out_path,
      paste0(config$company_name,".",pxk_num,".xlsx"))
  }else{
    dest_path <- config$pxk_out_path
    gbl_set_error_free(F)
    print("cannot find form path or dest path")
    show_error("path_notfound")
  }
  
  return(dest_path)
  
}
write_pxk_meta <- function(wb, pxk_data){
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
  wb <- write_excel(wb, pxk_data$pxk_num[1], config$pxk_num_coor)
  
  # writing current date
  wb <- write_excel(wb, format(Sys.Date(),config$date_format), 
                    config$pxk_date_coor)
  
  # writing payment type
  payment_str <- payment_type$actual[
    payment_type$payment_code == pxk_data$payment_code[1]]
  wb <- write_excel(
    wb, payment_str, config$pxk_payment_coor)
  
  return(wb)
}