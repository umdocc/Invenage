# this tab handle all the update ui and will be big

# ----------------------- update_import_price_tab ------------------------------
if ('update_import_price' %in% hidden_tab){
  update_import_price_tab <- 
    tabPanel(ui_elem$actual[ui_elem$label=='update_import_price'])
}else{
  update_import_price_tab <- tabPanel(
    ui_elem$actual[ui_elem$label=='update_import_price'],
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      # add_product box
      box(width = 3, height = 400,
          h3(ui_elem$actual[ui_elem$label=='update_import_price']),
          htmlOutput('uip_prod_name'),
          div(style="display: inline-block;vertical-align:top;width: 150px",
              htmlOutput('uip_vendor')),
          div(style="display: inline-block;vertical-align:top;width: 100px",
              htmlOutput('uip_min_order')),
          div(style="display: inline-block;vertical-align:top;width: 150px",
              htmlOutput('uip_import_price')),
          div(style="display: inline-block;vertical-align:middle;width: 100px",
          htmlOutput('uip_currency')),
          textInput(
            inputId = 'uip_source',
            label = ui_elem$actual[ui_elem$label=='source']),
          actionButton(
            'uip_update_button',
            label = get_actual('update_import_price'))
      )
    )
  )
}

# ------------------------- update_vendor tab ----------------------------------
if ('update_vendor' %in% hidden_tab){
  update_vendor_tab <- tabPanel(get_actual('update_vendor'))
}else{
  update_vendor_tab <- tabPanel(get_actual('update_vendor'),
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      box(width = 3, height = 400, style = "background-color:#f5f5f5;",
          h4(get_actual("add_vendor")),
          htmlOutput('uv_vendor'),
          htmlOutput('uv_vendor_orig'),
          htmlOutput('uv_vendor_local'),
          actionButton("uv_update_vendor",get_actual('update_vendor'))
      ),
      box(
        width=9, height = 800,
        DT::dataTableOutput("vendor_info_tbl")
      )
    )
  )
}

# ----------------------- button functions -------------------------------------
# update_prod :: add product to db button
add_prod_to_db <- function(input,output){
  
  # first add the vendor if not available
  current_vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor==input$add_orig_vendor]
  if (length(current_vendor_id)==0){
    append_vendor_info <- data.frame(vendor = input$add_orig_vendor, local=0)
    append_tbl_rld(config_dict,'vendor_info',append_vendor_info)
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
    append_tbl_rld(config_dict, 'product_info',append_prod)
    append_tbl_rld(config_dict, 'packaging',append_pkg)
    
    big_msg <- ui_elem$actual[ui_elem$label=='done']
    small_msg <- ui_elem$actual[ui_elem$label=='add_prod_success']
    shinyalert(title = big_msg, text = small_msg, type = "success")
  }
}

# update_prod :: add packaging to db button
add_pkg_to_db <- function(input,output){
  # check the input for correct format
  error_free <- T
  
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
  
  # if units_per_pack is not a number, show alert then do nothing
  if (is.na(as.numeric(input$add_unitspp))){
    show_alert('error','units_per_pack_notnum',"error")
    error_free <- F
  }
  # if unit is blank, do nothing
  if (added_unit==''){error_free <- F}
  
  
  if(error_free){
    # append the table, then reload
    append_pkg <- data.frame(
      unit = added_unit, units_per_pack = input$add_unitspp,
      prod_code = cur_prod_code, last_updated = format(Sys.Date()))
    append_tbl_rld(config_dict,'packaging',append_pkg)
    
    # display messages
    big_msg <- ui_elem$actual[ui_elem$label=='done']
    small_msg <- ui_elem$actual[ui_elem$label=='add_pkg_success']
    shinyalert(title = big_msg, text = small_msg, type = "success")
  }
}

# update_customer: add customer to db button
add_customer_to_db <- function(input,output){
  error_free <- T
  added_name <- input$add_customer_name
  test_df <- customer_info
  if(nrow(test_df[test_df$customer_name==added_name,])>0){
    error_free <- F
    big_msg <- ui_elem$actual[ui_elem$label=='error']
    small_msg <- ui_elem$actual[ui_elem$label=='customer_exist']
    shinyalert(title = big_msg, text = small_msg, type = "error")
  }
  if(error_free){
    # append and reload table
    append_customer <- data.frame(
      customer_name = input$add_customer_name,
      customer_email = input$add_customer_email,
      customer_address = input$add_customer_address,
      customer_phone = input$add_customer_phone,
      customer_tfn = input$add_customer_tfn,
      active = 1)
    append_tbl_rld(config_dict,'customer_info',append_customer)
    
    # if the syste indicate customer_code, retrieve the id then
    # create new to be compatible with customer_id
    if (config_dict$value[config_dict$name=='add_customer_code']=="TRUE"){
      customer_id <- customer_info$customer_id[
        customer_info$customer_name==input$add_customer_name]
      customer_code <- paste0('KH',sprintf(paste0('%0',config_dict$value[
        config_dict$name=='customer_code_width'],'d'),customer_id))
      # writing to database
      db_exec_query(
        paste0("update customer_info set customer_code='",customer_code,
                    "' where customer_id=",customer_id))
      reload_tbl(config_dict,'customer_info')
    }
    
    # display message
    big_msg <- ui_elem$actual[ui_elem$label=='done']
    small_msg <- ui_elem$actual[ui_elem$label=='add_customer_success']
    shinyalert(title = big_msg, text = small_msg, type = "success")
  }
}

# update_import_price :: add/update import price
update_price_from_uip <- function(input,track_change=T){
  current_prod_code <- product_info$prod_code[
    product_info$search_str==input$uip_prod_name]
  current_vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor == input$uip_vendor]
  
  # also need current_min_order here
  current_min_order <- input$uip_min_order
  
  # get current data, defined by prod_code, vendor_id and min_order
  current_import_price <- import_price[
    import_price$prod_code==current_prod_code&
      import_price$vendor_id==current_vendor_id&
      import_price$min_order==current_min_order,]
  
  # get input data
  new_import_price <- input$uip_import_price
  # if there is something the new price is different, update database
  if(nrow(current_import_price)>0&
     current_import_price$import_price[1]!=new_import_price){
    
    # handle track change first
    if(track_change){
      current_import_price$id <- NULL
      append_tbl_rld(config_dict,'track_import_price',current_import_price)
    }
    query <- paste0("update import_price set import_price=",new_import_price,
                    ",source_name ='",input$uip_source,
                    "' where prod_code='",current_prod_code,
                    "' and vendor_id=",current_vendor_id,
                    " and min_order=",current_min_order)
    db_exec_query(query)
    show_alert(get_actual('success'),'',msg_type = 'success')
  }
  # if there is no current import price, write new to database
  if(nrow(current_import_price)==0){
    append_import_price <- data.frame(
      prod_code = current_prod_code,
      import_price = new_import_price,
      currency_code = currency$currency_code[
        currency$currency==input$uip_currency],
      min_order = input$uip_min_order,
      last_updated = Sys.Date(),
      vendor_id = vendor_info$vendor_id[
        vendor_info$vendor==input$uip_vendor],
      source_name = input$uip_source
    )
    # print(append_import_price)
    append_tbl_rld(config_dict,'import_price',append_import_price)
    show_alert('success','abcd',msg_type='success')
  }
}

update_vendor_from_uv <- function(input){
  vendor_id <- vendor_info$vendor_id[vendor_info$vendor==input$uv_vendor]
  

  
  
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
    if(config$add_vendor_code=='TRUE'){
      vid_2update <- vendor_info$vendor_id[vendor_info$vendor==input$uv_vendor]
      if(length(vid_2update)==1){
        vendor_code <- sprintf("NCC%04d",vid_2update)
        query <- paste0("update vendor_info set vendor_code='",vendor_code,
        "' where vendor_id=",vid_2update)
        db_exec_query(query)
        reload_tbl(config_dict,'vendor_info')
      }
    }
    show_alert("success","success","success")
  }
}