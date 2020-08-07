# all data cleaning functions
# this function use the guess_table to replace all vendor with database vendor_id
guess_vendor_id <- function(input_data,mode='dataframe'){
  vendor_dict <- create_vendor_id_dict()

  # if a filepath is provided, search through the vendor dict
  if(mode=='filepath'){
    for (i in 1:nrow(vendor_dict)){
      if(grepl(vendor_dict$vendor[i],input_data)){
        input_data <- vendor_dict$vendor_id[i]
      }
    }
  }
  if(mode=='dataframe'){
    if (!('vendor' %in% names(input_data))){
      stop('cannot find vendor column in dataframe')
    }
    input_data$vendor <- gsub(' ','',input_data$vendor)

    input_data <- input_data[!is.na(input_data$vendor),]
    
    #alternative to merge
    input_data$vendor_id <- 0
    for (i in 1:nrow(input_data)){
      if (length(vendor_dict$vendor_id[vendor_dict$vendor==input_data$vendor[i]])>0){
        input_data$vendor_id[i] <- 
          vendor_dict$vendor_id[vendor_dict$vendor==input_data$vendor[i]]
      }
    }
  }
  # remove all irrelevant vendor
  return(input_data)
}
