# all data cleaning functions
# this function use the guess_table to replace all vendor with database vendor_id
guess_vendor_id <- function(data_df){
  if (!('vendor' %in% names(data_df))){
    stop('cannot find vendor column in dataframe')
  }
  data_df$vendor <- gsub(' ','',data_df$vendor)
  vendor_dict <- guess_table[guess_table$guess_type=='vendor_dict',]
  vendor_dict <- vendor_dict %>% rename(vendor = output_str)
  vendor_dict <- merge(vendor_dict,vendor_info,all.x=T)
  vendor_dict$vendor <- NULL
  vendor_dict <- vendor_dict %>% rename(vendor = input_str)
  data_df <- data_df[!is.na(data_df$vendor),]
  
  #alternative to merge
  data_df$vendor_id <- 0
  for (i in 1:nrow(data_df)){
    if (length(vendor_dict$vendor_id[vendor_dict$vendor==data_df$vendor[i]])>0){
      data_df$vendor_id[i] <- 
        vendor_dict$vendor_id[vendor_dict$vendor==data_df$vendor[i]]
    }
  }
  # remove all irrelevant vendor
  return(data_df)
}