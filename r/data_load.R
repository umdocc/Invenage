# --------------------- functions to read excel files --------------------------
# this function give and format the po_data given the po_name
read_po_data <- function(po_name){
  po_filepath <- get_po_filepath(po_name,config_dict)
  out_data <- read_excel_po(po_filepath)
  # get the vendor_id
  out_data$vendor_id <- guess_vendor_id(po_filepath,mode='filepath')
  out_data$vendor <- NULL # remove legacy vendor
  out_data <- merge(out_data,product_info %>% 
                      select(vendor_id,ref_smn,prod_code))
  # check data
  if (nrow(out_data[is.na(out_data$prod_code),])>0){
    warning('not all product in po can be identifed')
  }
  return(out_data)
}

# ---------------------- functions to update database --------------------------
# this function take in the raw tender data, as read from excel,
# do all the cleaning, then upload it to the tender_detail table in the database
upload_tender_data <- function(tender_data){
  # checking data requirements
  required_col <- c('stt','comm_name','unit','unit_price',
                    'tender_qty','vendor','tender_id')
  check_required_col(tender_data,required_col)
  
  # use only required columns
  tender_data <- tender_data[,required_col]
  # get vendor_id
  tender_data <- guess_vendor_id(tender_data)
  # recover the prod_code
  tender_data <- merge(tender_data,tender_comm_name,all.x=T) %>%
    select(prod_code,unit,unit_price,tender_qty,tender_id)
  # remove rows with invalid tender_qty
  tender_data <- tender_data[!is.na(as.numeric(tender_data$tender_qty)),]
  
  # if at this stage there is missing prod_code, raise error
  test <- tender_data[is.na(tender_data$prod_code),]
  if (nrow(test)>0){
    stop(paste('prod_code for', paste(test$comm_name),'not found'))
  }
  
  # data cleaning
  tender_data$final_rm_qty <- 0
  tender_data$unit <- tolower(tender_data$unit)
  
  # check for existing entries and write to database
  tender_data <- check_exist(tender_data,tender_detail,
                             c('prod_code','unit','tender_qty','tender_id'))
  
  append_tender_detail <- tender_data[!(tender_data$exist),]
  append_tender_detail$exist <- NULL
  if (nrow(append_tender_detail)>0){
    append_tbl_rld(config_dict,'tender_detail',append_tender_detail)
  }else{
    print('nothing to upload')
  }
}
# --------------------- functions to create tables/dicts -----------------------
create_vendor_id_dict <- function(){
  # build the vendor_id dictionary
  vendor_dict <- guess_table[guess_table$guess_type=='vendor_dict',]
  vendor_dict <- vendor_dict %>% rename(vendor = output_str)
  vendor_dict <- merge(vendor_dict,vendor_info,all.x=T)
  vendor_dict$vendor <- NULL
  vendor_dict <- vendor_dict %>% rename(vendor = input_str)
  return(vendor_dict)
}