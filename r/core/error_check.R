# functions to check various error during operation
# and display message instead of getting blank screen
# these return a boolean error_free variable

# check for all problems when we print the pxk
check_print_pxk <- function(input){
  error_free <- T #init
  
  # check if the pxk_form exists
  orig_form_path <- file.path(config$form_path, "pxk_form.xlsx")
  if(!file.exists(orig_form_path)){
    show_alert("error","pxk_form_notfound","error")
    error_free <- F
  }
  
  return(error_free) #return
}

check_vendor_exist <- function(vendor_name){
  vendor_exist <- T
  vendor_list <- db_read_query("select * from vendor_info")$vendor
  if(!(vendor_name %in% vendor_list)){
    vendor_exist <- F
  }
  return(vendor_exist)
}

# check if a product exists using ref_smn
check_product_exist <- function(prod_vendor_id,input_ref_smn){
  product_exist <- F
  
  # check if the added product exist
  test_df <- product_info[
    product_info$vendor_id==prod_vendor_id &
      product_info$ref_smn==input_ref_smn,]
  if (nrow(test_df)>0){
    product_exist <- T
    big_msg <- ui_elem$actual[ui_elem$label=='error']
    small_msg <- ui_elem$actual[ui_elem$label=='prod_exist']
    shinyalert(title = big_msg, text = small_msg, type = "error")
  }
  return(product_exist)
}

# check if a prod_code exist
check_prod_code_exist <- function(input_prod_code){
  prod_code_exist <- F
  
  test_df <- product_info[product_info$prod_code==input_prod_code,]
  if (nrow(test_df)>0){
    prod_code_exist <- T
    big_msg <- ui_elem$actual[ui_elem$label=='error']
    small_msg <- ui_elem$actual[ui_elem$label=='prod_code_exist']
    shinyalert(title = big_msg, text = small_msg, type = "error")
  }
  
  return(prod_code_exist)
}