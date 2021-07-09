update_vendor_from_udv <- function(input){
  input_vendor_name <- input$uv_vendor
  vendor_id <- vendor_info$vendor_id[vendor_info$vendor==input_vendor_name]
  
  # if nothing found, create new vendor, otherwise update
  if(length(vendor_id)==0){
    
    # translate orig_vendor and local
    trans_str <- unlist(strsplit(config$orig_vendor_noyes_str,split=';'))
    orig_vendor_noyes <- which(trans_str==input$uv_vendor_orig)-1
    trans_str <- unlist(strsplit(config$local_noyes_str,split=';'))
    local_noyes <- which(trans_str==input$uv_vendor_local)-1
    
    # append the new data
    append_vendor_info <- data.frame(
      vendor = input$uv_vendor,
      local = local_noyes,
      orig_vendor = orig_vendor_noyes
    )
    # print(append_vendor_info)
    append_tbl_rld(config_dict,'vendor_info',append_vendor_info)
    
    # build the vendor_code
    if(as.integer(config$add_vendor_code)){
      add_vendor_code(input_vendor_name)
      reload_tbl(config_dict,"vendor_info") # need to reload for display
    }
    
    show_alert("success","success","success")
  }else{
    show_alert("error","vendor_exist","error")
  }
}