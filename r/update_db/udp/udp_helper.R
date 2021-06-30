# -------------------------------- UI Loader -----------------------------------
# function to reload additional ui
udp_load_ui <- function(input,output,ui_list){
  if ("udp_vendor" %in% ui_list){
    output$udp_vendor <- udp_render_vendor()
  }
  return(output)
}

udp_render_vendor <- function(){renderUI({
  vendor_list <- db_read_query("select * from vendor_info")$vendor
  
  selectizeInput(
    inputId = "udp_vendor", 
    label = uielem$orig_vendor,
    choices = vendor_list, 
    selected = vendor_list[1], 
    options = list(create = T))
})}

# add product button handler
udp_add_product <- function(input,output){
  # collect variables
  error_free <- T
  input_vendor_name <- input$udp_vendor
  input_ref_smn <- input$udp_ref_smn
  
  add_vendor_with_product(input_vendor_name)
  
  # pulling information from ui
  prod_vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor==input_vendor_name]
  
  # check if the added product exist
  test_df <- product_info[
    product_info$vendor_id==prod_vendor_id &
      product_info$ref_smn==input_ref_smn,]
  if (nrow(test_df)>0){
    error_free <- F
    big_msg <- ui_elem$actual[ui_elem$label=='error']
    small_msg <- ui_elem$actual[ui_elem$label=='prod_exist']
    shinyalert(title = big_msg, text = small_msg, type = "error")
  }
  
  # check if the prod_code exist
  test_df <- product_info[product_info$prod_code==input$udp_prod_code,]
  if (nrow(test_df)>0){
    error_free <- F
    big_msg <- ui_elem$actual[ui_elem$label=='error']
    small_msg <- ui_elem$actual[ui_elem$label=='prod_code_exist']
    shinyalert(title = big_msg, text = small_msg, type = "error")
  }

  # compile the line to be added to product_info
  append_prod <- data.frame( 
    prod_code = input$udp_prod_code,
    comm_name = input$udp_comm_name,
    vendor_id = prod_vendor_id, 
    ref_smn = input_ref_smn,
    type = product_type$prod_type[product_type$actual == input$add_prod_type],
    updated_date = format(Sys.Date()),
    warehouse_id = warehouse_info$warehouse_id[
      warehouse_info$warehouse==input$add_warehouse],
    active = 1)
    

  # compose line to be added to packaging
  append_pkg <- data.frame(
    prod_code = input$udp_prod_code, 
    unit = tolower(input$udp_ordering_unit),
    units_per_pack = 1, 
    last_updated = format(Sys.Date())
  )


  # check if anything missing
  if(any(append_prod=='') | any(append_pkg=='')){
    error_free <- F
    big_msg <- uielem$input_error
    small_msg <- uielem$missing_fields
    shinyalert(title = big_msg, text = small_msg, type = "error")
  }
  
  # write to database if error_free
  if (error_free){
  append_tbl_rld(config_dict, 'product_info',append_prod)
  append_tbl_rld(config_dict, 'packaging',append_pkg)

  big_msg <- ui_elem$actual[ui_elem$label=='done']
  small_msg <- ui_elem$actual[ui_elem$label=='add_prod_success']
  shinyalert(title = big_msg, text = small_msg, type = "success")
  }
}

# add a vendor when adding a new product, config all parameters automatically
add_vendor_with_product <- function(input_vendor_name){
  # check to see if the vendor is already in the database
  current_vendor_id <- db_read_query(paste0(
    "select vendor_id from vendor_info where vendor='",
    input_vendor_name,"'"))$vendor_id
  if (length(current_vendor_id)==0){
    append_vendor_info <- data.frame(
      vendor = input_vendor_name, 
      local=0,
      orig_vendor = 1)
    append_tbl_rld(config_dict,'vendor_info',append_vendor_info)
    if(as.integer(config$add_vendor_code)){
      new_vendor_id <- db_read_query(paste0(
        "select vendor_id from vendor_info where vendor='",
        input_vendor_name,"'"))$vendor_id
      new_vendor_code <- paste0(config$vendor_code_prefix,
                                formatC(as.integer(new_vendor_id), 
                                        width=4, flag="0"))
      db_exec_query(paste0(
        "update vendor_info info set vendor_code='",
        new_vendor_code,"' where vendor_id=",new_vendor_id))
    }
  }
  
}