# Functions to handle button presses

# -------------------------- update_db section ---------------------------------
add_prod_to_db <- function(input,output){
  # reload table
  vendor_info <- reload_tbl(config_dict,'vendor_info')
  # first add the vendor if not available
  current_vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor==input$add_orig_vendor]
  if (length(current_vendor_id)==0){
    append_vendor_info <- data.frame(vendor = input$add_orig_vendor, local=0)
    conn <- db_open(config_dict)
    dbWriteTable(conn,'vendor_info',append_vendor_info,append=T)
    dbDisconnect(conn)
    vendor_info <- reload_tbl(config_dict,'vendor_info')
  }
  # pulling information from ui
  prod_vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor==input$add_orig_vendor]
  # check if the added product exist
  error_free <- T
  tmp1 <- input$add_orig_vendor; tmp2 <- input$add_ref # cannot read from input
  test_df <- product_info[
    product_info$vendor==tmp1 & product_info$ref_smn==tmp2,]
  if (nrow(test_df)>0){
    error_free <- F
    big_msg <- ui_elem$actual[ui_elem$label=='error']
    small_msg <- ui_elem$actual[ui_elem$label=='prod_exist']
    shinyalert(title = big_msg, text = small_msg, type = "error")
  }
  
  # write to databas if error_free
  if (error_free){
    # compile the line to be added to product_info
    append_prod <- data.frame( 
      prod_code = input$add_prod_code, name = input$add_name, 
      comm_name = input$add_name,
      vendor = input$add_orig_vendor, ref_smn = input$add_ref,
      type = product_type$prod_type[product_type$actual == input$add_prod_type],
      packaging_str = '', updated_date = format(Sys.Date()), prod_group = '',
      warehouse_id = warehouse_info$warehouse_id[
        warehouse_info$warehouse==input$add_warehouse],
      active = 1, vendor_id = prod_vendor_id)
    # compose line to be added to packaging
    append_pkg <- data.frame(
      prod_code = input$add_prod_code, unit = tolower(input$add_ordering_unit),
      units_per_pack = 1, last_updated = format(Sys.Date())
    )
    
    conn <- db_open(config_dict)
    dbWriteTable(conn,'product_info',append_prod,append=T)
    dbWriteTable(conn,'packaging',append_pkg,append=T)
    dbDisconnect(conn)
    big_msg <- ui_elem$actual[ui_elem$label=='done']
    small_msg <- ui_elem$actual[ui_elem$label=='add_prod_success']
    shinyalert(title = big_msg, text = small_msg, type = "success")
  }
}

add_pkg_to_db <- function(input,output){
  # check the input for correct format
  error_free <- T
  product_info <- reload_tbl(config_dict,'product_info')
  packaging <- reload_tbl(config_dict, 'packaging')
  cur_prod_code <- product_info$prod_code[
    product_info$search_str==input$add_pkg_prod_name]
  added_unit <- tolower(input$add_pkg_unit) # prevent capital letters

  # if pakaging exists, show error
  test_df <- packaging
  test_df <- test_df[
    test_df$unit==added_unit & test_df$prod_code==cur_prod_code,]
  if(nrow(test_df)>0){
    error_free <- F
    big_msg <- ui_elem$actual[ui_elem$label=='error']
    small_msg <- ui_elem$actual[ui_elem$label=='pkg_exist']
    shinyalert(title = big_msg, text = small_msg, type = "error")
  }
  
  # if units_per_pack is not a number, do nothing
  if (is.na(as.numeric(input$add_unitspp))){error_free <- F}
  # if unit is blank, do nothing
  if (added_unit==''){error_free <- F}
  
  
  if(error_free){
    append_pkg <- data.frame(
      unit = added_unit, units_per_pack = input$add_unitspp,
      prod_code = cur_prod_code, last_updated = format(Sys.Date()))
    conn <- db_open(config_dict)
    dbWriteTable(conn,'packaging',append_pkg,append=T)
    dbDisconnect(conn)
    big_msg <- ui_elem$actual[ui_elem$label=='done']
    small_msg <- ui_elem$actual[ui_elem$label=='add_pkg_success']
    shinyalert(title = big_msg, text = small_msg, type = "success")
  }
}

add_customer_to_db <- function(input,output){
  error_free <- T
  added_name <- input$add_customer_name
  test_df <- reload_tbl(config_dict, 'customer_info')
  if(nrow(test_df[test_df$customer_name==added_name,])>0){
    error_free <- F
    big_msg <- ui_elem$actual[ui_elem$label=='error']
    small_msg <- ui_elem$actual[ui_elem$label=='customer_exist']
    shinyalert(title = big_msg, text = small_msg, type = "error")
  }
  if(error_free){
    append_customer <- data.frame(
      customer_name = input$add_customer_name,
      customer_email = input$add_customer_email,
      customer_address = input$add_customer_address,
      customer_phone = input$add_customer_phone,
      customer_tfn = input$add_customer_tfn)
    conn <- db_open(config_dict)
    dbWriteTable(conn,'customer_info',append_customer,append=T)
    dbDisconnect(conn)
    big_msg <- ui_elem$actual[ui_elem$label=='done']
    small_msg <- ui_elem$actual[ui_elem$label=='add_customer_success']
    shinyalert(title = big_msg, text = small_msg, type = "success")
  }
  }