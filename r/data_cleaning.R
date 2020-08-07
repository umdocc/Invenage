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

# this function add importlic column to all products of a vendor using
# a 'raw' importlic data (read from excel with only needed columns)
# it will then write the name of the excel importlic file to the column 
# importlic if the ref_smn of the product can be found in the importlic data
add_importlic_info <- function(raw_importlic){
  # keep only required columns
  req_col <- c('stt','comm_name','ref_smn')
  check_required_col(raw_importlic,req_col)
  raw_importlic <- raw_importlic[,req_col]
  
  # filter product info to the specific vendor on the importlic
  tmp <- product_info[
    product_info$vendor_id==guess_vendor_id(filepath,mode='filepath'),]
  #combine all ref so that we can search easier
  ref_in_lic <- paste(raw_importlic$ref_smn,collapse ='/')
  
  # write the name of importlic file to importlic column
  tmp$importlic_name <- ''
  for (i in 1:nrow(tmp)){
    if(grepl(tmp$ref_smn[i],ref_in_lic)){
      tmp$importlic[i] <- basename(filepath)
    }
  }
  return(tmp)
}

# this funcion expect a table with prod_code, qty, vendor_id columns, it then
# add the exw import price
add_import_price <- function(po_data, stringQty = 'qty'){
  req_col <- c('prod_code','vendor_id',stringQty)
  for (i in req_col){
    if (!(i %in% names(po_data))){ stop(paste('error', i, 'not found'))}
  }
  
  # save the name of qty column, then rename it to qty
  oldname <- stringQty
  names(po_data)[names(po_data)==stringQty] <- 'qty'
  
  # load all import price
  po_data <- merge(
    po_data, import_price %>% 
      select(prod_code, import_price, vendor_id, currency_code, min_order),
    all.x=T)
  # if an item has min_order = NA, set it to 1
  po_data$min_order[is.na(po_data$min_order)] <- 1
  #calculate qty/min_order ratio, then choose one with 
  # this ratio >=1 and minimised
  po_data$order_ratio <- po_data$qty/po_data$min_order
  po_data <- po_data[po_data$order_ratio>=1,]
  po_data <- po_data %>%group_by(prod_code) %>% 
    mutate(min_ratio = min(order_ratio)) %>% ungroup
  po_data <- po_data[po_data$order_ratio==po_data$min_ratio,]
  
  # check for prod_code duplications before returning results
  if (nrow(po_data[duplicated(po_data %>% select(prod_code,qty)),])>0){
    stop('po_data contains duplicated line')
  }else{
    # restore the name
    names(po_data)[names(po_data)=='qty'] <- stringQty
    # remove all useless columns
    removed_col <- c('min_order','currency_code', 'vendor_id', 'vendor', 
                     'order_ratio','min_ratio','last_updated', 'lot','exp_date',
                     'actual_unit_cost','note')
    for (i in removed_col){ po_data[,i] <- NULL }
    if ('stt' %in% names(po_data)){
      po_data <- po_data[order(po_data$stt),]
    }
    return(po_data)
  }
}