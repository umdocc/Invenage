cdn_load_ui <- function(input,output,ui_list){
  if ('cdn_customer' %in% ui_list){
    output$cdn_customer <- render_cdn_customer(input) # customer
  }
  if ('cdn_prod_name' %in% ui_list){
    output$cdn_prod_name <- render_cdn_prod_name(input)
  }
  if ('cdn_unit' %in% ui_list){
    output$cdn_unit <- render_cdn_unit(input)
  }
  if ('cdn_qty' %in% ui_list){
    output$cdn_qty <- render_cdn_qty(input)
  }
  if ('cdn_warehouse' %in% ui_list){
    output$cdn_warehouse <- render_cdn_warehouse(input)
  }
  if ('cdn_lot' %in% ui_list){
    output$cdn_lot <- render_cdn_lot(input)
  }
  if ('cdn_payment_type' %in% ui_list){
    output$cdn_payment_type <- render_cdn_payment_type(input)
  }
  if ('cdn_unit_price' %in% ui_list){
    output$cdn_unit_price <- render_cdn_unit_price(input)
  }
  if ('cdn_tender_name' %in% ui_list){
    output$cdn_tender_name <- render_cdn_tender_name(input)
  }
  if ('cdn_prod_info' %in% ui_list){
    output$cdn_prod_info <- render_cdn_prod_info(input)
  }
  if ('cdn_pxk_info' %in% ui_list){
    output$cdn_pxk_info <- render_cdn_pxk_info(input)
  }
  if ('cdn_pxk_data' %in% ui_list){
    output$cdn_pxk_data <- render_cdn_pxk_data(input)
  }
  return(output)
}

# used to load cdn data into memory
load_cdn_data <- function(input){
  assign("current_pxk",get_current_pxk(input),envir=globalenv())
  assign("current_pxk_data",get_pxk_data(current_pxk$pxk_num),
         envir=globalenv())
}

init_cdn <- function(input,output){
  
  output <- cdn_load_ui(
    input,output,
    c("cdn_customer", "cdn_prod_name", "cdn_qty", "cdn_unit",
      "cdn_warehouse","cdn_lot","cdn_payment_type","cdn_unit_price",
      "cdn_promo_price","cdn_tender_name","cdn_note",
      "cdn_prod_info","cdn_pxk_info","cdn_pxk_data"))
  load_cdn_data(input)
  
  return(output)
}

render_cdn_customer <- function(input){renderUI({
  if(current_pxk$status=="new"){
    cust_choices <- db_read_query(
      "select customer_name from customer_info")$customer_name
  }else{
    cust_choices <- current_pxk$customer_name
  }
  
  selectizeInput(
    inputId = "cdn_customer", label = uielem$customer_name, 
    choices = cust_choices, selected = cust_choices[1], 
    options = list(create = F))
})
}

render_cdn_prod_name <- function(input){renderUI({
  
  selectizeInput(
    inputId = "cdn_prod_name", label = uielem$comm_name, 
    choices = prod_choices$prod_search_str, 
    selected = prod_choices$prod_search_str[1], 
    options = list(create = F))
})
}

render_cdn_qty <- function(input){renderUI({
  
  selectizeInput(
    inputId = "cdn_qty", label = uielem$qty, 
    choices = 1:20000, selected = 1, 
    options = list(create = T))
})
}

render_cdn_unit <- function(input){renderUI({
  
  current_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$cdn_prod_name]
  
  unit_choices <- packaging$unit[packaging$prod_code==current_prod_code]
  
  selectizeInput(
    inputId = "cdn_unit", label = uielem$unit, 
    choices = unit_choices, selected = unit_choices[1], 
    options = list(create = F))
})
}

render_cdn_warehouse <- function(input){renderUI({
  
  # get the current prod_code and choose the warehouse
  current_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$cdn_prod_name]
  
  tmp <- inventory[inventory$lot==input$cdn_lot &
                     inventory$prod_code==current_prod_code,]
  
  warehouse_choice <- merge(
    warehouse_info, tmp %>% select(prod_code, warehouse_id))
  warehouse_choice <- warehouse_choice$warehouse

  #render ui
  selectizeInput(
    inputId = "cdn_warehouse", label = uielem$warehouse, 
    choices = warehouse_choice, selected = warehouse_choice[1], 
    options = list(create = F))
})
}

render_cdn_lot <- function(input){renderUI({
  current_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$cdn_prod_name]
  
  lot_select <- inventory[inventory$prod_code==current_prod_code,] %>%
    arrange(intexp_date)
  lot_choices <- lot_select$lot
  lot_select <- lot_select[
    lot_select$intexp_date==min(lot_select$intexp_date),]$lot
  #render ui
  selectizeInput(
    inputId = "cdn_lot", label = uielem$lot, 
    choices = lot_choices, selected = lot_select, 
    options = list(create = F))
})
}

render_cdn_payment_type <- function(input){renderUI({
  payment_type <- db_read_query(
    "select * from payment_type inner join uielem
    on payment_type.payment_label=uielem.label
    where uielem.type='payment_label'")
  ui_choices <- payment_type$actual
  ui_selected <- ui_choices[1]
  
  #render ui
  selectizeInput(
    inputId = "cdn_payment_type", label = uielem$payment_type, 
    choices = ui_choices, selected = ui_selected, 
    options = list(create = F))
})
}

render_cdn_unit_price <- function(input){renderUI({
  current_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$cdn_prod_name]
  
  current_customer_id <- customer_info$customer_id[
    customer_info$customer_name==input$cdn_customer]
  
  price_hist <- sale_log[sale_log$prod_code==current_prod_code,] %>% 
    filter((customer_id == current_customer_id) & (promotion_price == 0) & 
             (unit == input$cdn_unit))
  
  price_choices <- unique(price_hist$unit_price)
  price_selected <- price_hist$unit_price[
    price_hist$sale_datetime==max(price_hist$sale_datetime)]
  
  #render ui
  selectizeInput(
    inputId = "cdn_unit_price", label = uielem$unit_price, 
    choices = price_choices, selected = price_selected, 
    options = list(create = T))
})
}

render_cdn_tender_name <- function(input){renderUI({
  current_customer_id <- customer_info$customer_id[
    customer_info$customer_name==input$cdn_customer]
  tmp <- tender_info[
    tender_info$customer_id==current_customer_id|tender_info$tender_id==0,]
  
  tender_choices <- tmp$customer_tender_name
  tender_selected <- tender_choices[1]
  #render ui
  selectizeInput(
    inputId = "cdn_tender_name", label = uielem$tender_name, 
    choices = tender_choices, 
    selected = tender_selected, 
    options = list(create = F))
})
}

render_cdn_prod_info <- function(input){renderUI({
  current_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str == input$cdn_prod_name]
  current_lot <- input$cdn_lot
  current_selected_unit <- input$cdn_unit
  current_ref_smn <- product_info$ref_smn[
    product_info$prod_code == current_prod_code]
  current_vendor_id <- product_info$vendor_id[
    product_info$prod_code == current_prod_code]
  current_vendor <- vendor_info$vendor[
    vendor_info$vendor_id == current_vendor_id]
  
  selected_info <-inventory[
    inventory$prod_code == current_prod_code &
      inventory$lot == current_lot,]
  
  total_available <- selected_info$remaining_qty
  
  # also get all other available lot
  alllot_available <- inventory[
    inventory$prod_code == current_prod_code &
      inventory$lot != current_lot,]
  alllot_available$exp_date[is.na(alllot_available$exp_date)] <- 'nodate'
  
  if(length(total_available)==0){total_available <- 0}
  current_exp_date <- selected_info$exp_date
  
  packaging_str <- packaging[
    packaging$prod_code == current_prod_code &
      packaging$unit == current_selected_unit,]
  
  ordering_unit <- get_ordering_unit(packaging)
  current_order_unit <- ordering_unit$unit[
    ordering_unit$prod_code==current_prod_code]
  
  current_units_per_pack <- packaging$units_per_pack[
    packaging$prod_code==current_prod_code &
      packaging$unit==current_selected_unit]
  
  packaging_str <- paste0(packaging_str$units_per_pack[1],
                          packaging_str$unit[1],'/',current_order_unit)
  alllot_str <- ''
  alllot_available <- merge(alllot_available,ordering_unit,all.x=T)
  if (nrow(alllot_available)>0){
    for(i in 1:nrow(alllot_available)){
      alllot_str <- paste0(
        alllot_str,alllot_available$lot[i],' - ',alllot_available$exp_date[i],
        ' - ',alllot_available$remaining_qty[i],'',alllot_available$unit[i],
        '<br/>')
    }
  }
  
  product_info_str <- paste(
    uielem$information, ':<br/>',
    # uielem$prod_code,':',current_prod_code, '<br/>',
    uielem$vendor,':',current_vendor,'<br/>',
    uielem$ref_smn,":",current_ref_smn,'<br/>',
    uielem$exp_date,':',current_exp_date,'<br/>',
    uielem$remaining_qty,':',
    round(total_available*current_units_per_pack, digits=0),
    current_selected_unit,'(',
    round(total_available,digits=1), current_order_unit, ')<br/>',
    uielem$packaging_str,":",packaging_str, '<br/>',
    uielem$other_lot,':<br/>',
    alllot_str)
  HTML(product_info_str)
}) }

render_cdn_pxk_info <- function(input){renderUI({
  HTML(paste("<font size='+1'>",uielem$pxk_num,":",em(current_pxk$pxk_num),
             '&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp',
             uielem$customer_name,':',em(current_pxk$customer_name),
             '&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp',
             uielem$status,":",uielem[[current_pxk$status]],'</font><br/>')
  )
}) }

# render table for the pxk_man tab
render_cdn_pxk_data <- function(input){DT::renderDataTable({
  output_pxk <- render_output_pxk(current_pxk_data)
  DT::datatable(output_pxk, options = list(pageLength = 10),rownames=F,
                editable = 'cell')
})
}

get_current_pxk <- function(input){
  # query an incomplete pxk that match the admin_id
  current_pxk <- db_read_query(paste0(
    "select * from pxk_info where admin_id =",config$admin_id,
    " and sale_datetime = (select max(sale_datetime) from pxk_info 
    where admin_id=",config$admin_id,")"))
  
  if(nrow(current_pxk)>0){
    
    if (current_pxk$completed==0){
      current_pxk$status <- 'in_progress'
      current_pxk$customer_name <- customer_info$customer_name[
        customer_info$customer_id==current_pxk$customer_id]
    }else{
      admin_id_length <- nchar(as.character(config$admin_id))
      if(as.integer(Sys.Date()-as.Date(current_pxk$sale_datetime))>0){
        current_count <- 1
      }else{
        current_count <- as.integer(
          substring(as.character(current_pxk$pxk_num), admin_id_length+6+1))+1
      }
      current_pxk_num <- paste0(config$admin_id,strftime(Sys.Date(),"%d%m%y"),
                                sprintf("%02d", (current_count)))
      current_pxk <- data.frame(pxk_num=current_pxk_num,
                                status='new', customer_name = "")
    }
  }else{
    current_pxk_num <- paste0(config$admin_id,strftime(Sys.Date(),"%d%m%y"),
                              sprintf("%02d", 1))
    current_pxk <- data.frame(pxk_num=current_pxk_num,
                              status='new', customer_name = "")
  }
  return(current_pxk)
}

get_pxk_data <- function(pxk_num){
  
  pxk_data <- db_read_query(paste(
    "select * from sale_log where pxk_num=",pxk_num))
  
  return(pxk_data)
}

cdn_write_new_pxk <- function(input, current_pxk){
  
  append_pxk_info <- data.frame(
    pxk_num = current_pxk$pxk_num,
    # time variable needs to be in UTC
    sale_datetime = format(as.POSIXlt(Sys.time(),tz="UTC"),
                           '%Y-%m-%d %H:%M:%S'),
    customer_id = customer_info$customer_id[
      customer_info$customer_name==input$cdn_customer],
    payment_code = payment_type$payment_code[
      payment_type$actual == input$cdn_payment_type],
    completed = 0,
    admin_id = config$admin_id
  )
  
  # write to database
  conn <- db_open()
  dbWriteTable(conn,"pxk_info",append_pxk_info,append=T)
  dbDisconnect(conn)
  
  #update variable current_pxk
  current_pxk$status <- "in_progress"
  current_pxk$customer_name <- input$cdn_customer
  current_pxk$current_stt <- 1
  
  return(current_pxk)
  
}

cdn_build_append_sale_log <- function(input){
  
  # build base sale_log for testing first
  append_sale_log <- data.frame(
    stt = current_pxk$current_stt,
    prod_code = unique(
      prod_choices$prod_code[prod_choices$prod_search_str==input$cdn_prod_name]),
    unit = input$cdn_unit,
    lot = input$cdn_lot,
    unit_price = as.integer(input$cdn_unit_price),
    qty = input$cdn_qty,
    pxk_num = current_pxk$pxk_num,
    note = input$cdn_note
  )
  
  # add warehouse_id and tender_id
  current_warehouse_id <- warehouse_info$warehouse_id[
    warehouse_info$warehouse == input$cdn_warehouse]
  append_sale_log$warehouse_id <- current_warehouse_id
  current_tender_id <- tender_info$tender_id[
    tender_info$customer_tender_name==input$cdn_tender_name & 
      tender_info$warehouse_id==current_warehouse_id]
  
  # special handling of no tender
  tender_0_name <- tender_info$customer_tender_name[
    tender_info$tender_id==0]
  if(input$cdn_tender_name==tender_0_name){
    current_tender_id <- 0
  }
  append_sale_log$tender_id <- current_tender_id
  
  append_sale_log$promotion_price <- as.numeric(input$cdn_promo_price)
  
  return(append_sale_log)
}

cdn_check_append_sale_log <- function(append_sale_log){
  append_sale_log <- convert_to_pack(
    append_sale_log, packaging, "qty","pack_qty")
  tmp <- inventory[inventory$lot==append_sale_log$lot &
                     inventory$prod_code==append_sale_log$prod_code &
                     inventory$warehouse_id == append_sale_log$warehouse_id,]
  test_remain <- tmp$remaining_qty - append_sale_log$pack_qty
  thres <- -0.001
  if(test_remain < thres){
    gbl_write_var("error_free",F)
    show_error("qty_exceed_limit")
  }
}

cdn_add_entry <- function(input,output){
  gbl_set_error_free(T)
  # if this is a new pxk, write to database first
  if (current_pxk$status=="new"){
    current_pxk <- cdn_write_new_pxk(input, current_pxk)
  
  #otherwise, read the info from the sale_log, bump stt by 1 if found
  }else{
    current_stt_list <- sale_log$stt[
      sale_log$pxk_num==as.numeric(as.character(current_pxk$pxk_num))]
    if(length(current_stt_list)>0){
      current_pxk$current_stt <- max(current_stt_list)+1
    }else{
      current_pxk$current_stt <- 1
    }
  }
  # writing to global
  gbl_write_var("current_pxk",current_pxk)
  
  # build and check append_sale_log
  append_sale_log <- cdn_build_append_sale_log(input)
  
  gbl_write_var("append_sale_log",append_sale_log)
  cdn_check_append_sale_log(append_sale_log)
  
  # # writing to database
  if(error_free){
    db_append_tbl("sale_log",append_sale_log)
    
    #append to current_pxk_data
    current_pxk_data$id <- NULL
    current_pxk_data <- rbind(current_pxk_data,append_sale_log)
    gbl_write_var("current_pxk_data",current_pxk_data)
    
    # reload data and ui
    gbl_load_tbl("sale_log")
    cdn_clean_duplicated()
    gbl_update_inventory()
    cdn_load_ui(input,output,c("cdn_pxk_data","cdn_prod_info", "cdn_pxk_info",
                               "cdn_customer"))
  }
  
}

cdn_complete_pxk <- function(input,output){
  
  # check pxk for error then set the complete flag in database
  cdn_check_pxk()
  
  if(error_free){
    cdn_pxk_num <- current_pxk$pxk_num
    db_exec_query(paste0("update pxk_info set completed=1 where pxk_num=",
                         cdn_pxk_num))
    print(paste("completed pxk number",cdn_pxk_num,",printing....."))
    load_cdn_data(input)
    
    output <- cdn_load_ui(input,output,c("cdn_pxk_info","cdn_pxk_data",
                                         "cdn_customer"))
    show_success("complete",var_list = cdn_pxk_num)
    print_pxk(cdn_pxk_num)
  }
}


# this function is also shared with pxk_man tab
# in cdn, we use the current_pxk_data var, otherwise, reload the pxk_data var
render_output_pxk <- function(pxk_data){
  # directly use the current_pxk_data variable
  output_pxk <- merge(pxk_data,product_info %>% 
                        select(prod_code,comm_name))
  output_pxk <- output_pxk %>% 
    select(stt,comm_name,unit,qty,unit_price,lot,note)
  output_pxk <- output_pxk[order(output_pxk$stt),] # sort by stt
  
  for (i in 1:length(output_pxk)){
    if(length(uielem[[names(output_pxk)[i]]])==1){
      names(output_pxk)[i] = uielem[[names(output_pxk)[i]]]
    }
  }
  return(output_pxk)
}

cdn_check_pxk <- function(){
  # create new PXK file
  current_pxk$orig_path <- file.path(config$form_path,'pxk_form.xlsx')
  current_pxk$dest_path <- file.path(config$pxk_out_path,
                                     paste0(config$company_name,".PXK.",
                                            current_pxk$pxk_num,".xlsx"))
  
  if(!file.exists(current_pxk$orig_path)){
    gbl_write_var("error_free",F)
    show_error("file_notfound","pxk_form.xlsx")
  }
  
  if(!dir.exists(dirname(current_pxk$dest_path))){
    gbl_write_var("error_free",F)
    show_error("path_notfound",dirname(current_pxk$dest_path))
  }

  if(nrow(current_pxk_data)==0){
    gbl_write_var("error_free",F)
    show_error("no_data","")
  }
    
  gbl_write_var("current_pxk",current_pxk)
}

cdn_clean_duplicated <- function(){
  #sale log duplication
  tmp <- sale_log[
    duplicated(
      sale_log %>% 
        select(prod_code, unit, unit_price, qty, lot, pxk_num, 
               warehouse_id, tender_id, customer_id, payment_code, note)),]
  
  if(nrow(tmp)>0){
    # clean duplicated in db and reload global data
    conn <- db_open()
    for (i in 1:nrow(tmp)){
      query <- paste0("delete from sale_log where id=",tmp$id[i])
      # print(query)
      dbExecute(conn,query)
    }
    dbDisconnect(conn)
    
    assign("current_pxk_data",get_pxk_data(current_pxk$pxk_num),
           envir=globalenv())
    gbl_load_tbl("sale_log")

  }

}