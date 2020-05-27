# Functions to handle button presses
# ------------------------- inv_out buttons ------------------------------------
exec_inv_out <- function(input,output, config_dict){
  # custom display messa
  output$sys_msg <- render_sys_message('please wait....')
  
  # read info from database
  current_pxk <- get_current_pxk(config_dict)
  conn <- db_open(config_dict)
  sale_log <- dbReadTable(conn,"sale_log")
  pxk_info <- dbReadTable(conn,"pxk_info")
  payment_type <- dbReadTable(conn,"payment_type")
  warehouse_info <- dbReadTable(conn,"warehouse_info")
  dbDisconnect(conn)
  tender_info <- reload_tbl(config_dict, 'tender_info')
  
  payment_type <- merge(payment_type,ui_elem,
                        by.x='payment_label',by.y='label')
  
  # if this is a new pxk, write to database first
  if (nrow(pxk_info[pxk_info$pxk_num==current_pxk,])==0){
    appendPXKInfo <- data.frame(
      pxk_num = current_pxk,
      sale_datetime = format(Sys.time(),'%Y-%m-%d %H:%M:%S'),
      customer_id = customer_info[
        customer_info$customer_name==input$customer_name,'customer_id'],
      payment_code = payment_type$payment_code[
        payment_type$actual == input$payment_type],
      completed = 0,
      admin_id = admin_id
    )
    conn <- db_open(config_dict)
    dbWriteTable(conn,'pxk_info',appendPXKInfo,append=T)
    pxk_info <- dbReadTable(conn,"pxk_info")
    dbDisconnect(conn)
    # set current_stt also
    current_stt <- 1
  }else{ #otherwise, read the info from the sale_log
    conn <- db_open(config_dict)
    stt_list <- dbGetQuery(conn, "select stt from sale_log
                               where pxk_num = (
                               select pxk_num from pxk_info
                               where completed = 0)")
    dbDisconnect(conn)
    # if there is a result, determine the stt from list, otherwise set to 1
    if (nrow(stt_list)>0){
      for (i in 1:15){ # loop 20 times
        if (!any(i==stt_list$stt)){
          current_stt <- i
          break
        }
      }
    }else{
      current_stt <- 1
    }
  }
  # build base sale_log for testing first
  append_sale_log <- data.frame(
    stt = current_stt,
    prod_code = unique(
      product_info[product_info$search_str==input$prod_name_select,
                   "prod_code"]),
    unit = input$unit_selector,
    lot = input$lot_select,
    unit_price = as.integer(input$unit_price),
    qty = input$qty_selector,
    pxk_num = current_pxk,
    note = input$pxk_note
  )
  # check and write append_sale_log to database
  inv_out_ok <- check_inv_out(append_sale_log, config_dict)
  if (current_stt>10){ #limit the max stt to 10
    inv_out_ok <- F
  }
  if (inv_out_ok){
    # add warehouse_id and tender_id
    append_sale_log$warehouse_id <- warehouse_info$warehouse_id[
      warehouse_info$warehouse == input$warehouse_selector]
    append_sale_log$tender_id <- tender_info$tender_id[
      tender_info$customer_tender_name==input$tender_name]
    
    conn <- db_open(config_dict)
    dbWriteTable(conn,'sale_log',append_sale_log,append=T)
    dbDisconnect(conn)
    output$sys_msg <- render_sys_message(
      ui_elem$actual[ui_elem$label=='inv_out_success'])
  }else{
    # default reason
    output$sys_msg <- render_sys_message(
      ui_elem$actual[ui_elem$label=='inv_exceed'])
    if (current_stt>10){ # more than 10 lines
      output$sys_msg <- render_sys_message('exceeding 10 lines')
    }
  }
}

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