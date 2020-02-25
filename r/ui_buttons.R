# Functions to handle button presses

# -------------------------- update_db section ---------------------------------
add_prod_to_db <- function(input,output){

  # first add the vendor if not available
  current_vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor==input$add_orig_vendor]
  if (length(current_vendor_id)==0){
    append_vendor_info <- data.frame(vendor = input$add_orig_vendor, local=0)
    conn <- db_open(config_dict)
    dbWriteTable(conn,'vendor_info',append_vendor_info,append=T)
    dbDisconnect(conn)
  }
  
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
      vendor = input$add_orig_vendor, ref_smn = input$add_ref,
      type = product_type$prod_type[product_type$actual == input$add_prod_type],
      packaging_str = '', updated_date = format(Sys.Date()), prod_group = '',
      warehouse_id = warehouse_info$warehouse_id[
        warehouse_info$warehouse==input$add_warehouse],
      active = 1 )
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