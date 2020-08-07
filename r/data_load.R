# functions to deal with reading and loading data

# this function take in the raw tender data, as read from excel,
# do all the cleaning, then upload it to the tender_detail table in the database
upload_tender_data <- function(tender_data){
  # checking data requirements
  required_col <- c('stt','comm_name','unit','unit_price',
                    'tender_qty','vendor','tender_id')
  for (col_name in required_col){
    if(!(col_name %in% names(tender_data))){
      stop(paste(col_name, 'column not found'))
    }
  }
  
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
