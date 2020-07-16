# all reactive functions used to render shiny UI

# render list of tender
render_tender_list <- function(iid, config_dict, input){renderUI({
  current_cust_id <- customer_info$customer_id[
    customer_info$customer_name==input$customer_name] # get current customer
  current_wh_id <- warehouse_info$warehouse_id[
    warehouse_info$warehouse==input$warehouse_selector] # get current warehouse 
  # get the tender based on customer & warehouse first
  default_tender <- tender_info$customer_tender_name[
    tender_info$customer_id==current_cust_id & tender_info$warehouse_id==
      current_wh_id]
  tender_choices <- tender_info$customer_tender_name[
    tender_info$warehouse_id==current_wh_id | tender_info$tender_id==0
  ]
  # if there is nothing, or many thing default to 0
  if (length(default_tender)!=1){
    default_tender <- tender_info$customer_tender_name[
      tender_info$tender_id==0]
  }

  selectInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='tender_name'],
    choices = tender_choices, selected =  default_tender
  )
})}


# render_po_list
render_po_list <- function(iid, config_dict){renderUI({
  
  po_list_active <- po_info$po_name[po_info$completed==0] 
  
  selectizeInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='select_po'],
    choices = po_list_active, selected =  po_list_active[1]
  )
})}

render_in_vendor <- function(iid,input,config_dict){renderUI({

  current_prod_code <- product_info$prod_code[
    product_info$search_str == input$in_prodname_select]

  last_vendor_id <- import_log[
    import_log$prod_code==current_prod_code,]
  sel_vendor_id <- last_vendor_id$vendor_id[
    last_vendor_id$delivery_date==max(last_vendor_id$delivery_date)]
  sel_vendor_id <- sel_vendor_id[!is.na(sel_vendor_id)]
  if (length(sel_vendor_id)==0){
    sel_vendor_id <- 1
  }
  sel_vendor <- vendor_info$vendor[vendor_info$vendor_id==sel_vendor_id]
  selectizeInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='vendor'],
    choices = vendor_info$vendor, selected =  sel_vendor,
    options = list(create = F)
  )
})}

render_in_cost <- function(iid,input,config_dict){renderUI({

  current_prod_code <- product_info$prod_code[
    product_info$search_str == input$in_prodname_select]
  current_vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor==input$in_vendor]
  current_in_unit <- input$in_unit
  import_cost <- import_log[import_log$prod_code==current_prod_code & 
    import_log$vendor_id==current_vendor_id & 
      import_log$unit==current_in_unit,]
  sel_import_cost <- import_cost$actual_unit_cost[
    import_cost$delivery_date==max(import_cost$delivery_date)][1]
  import_cost_choice <- import_cost$actual_unit_cost
  
  if (length(import_cost_choice)==0){
    import_cost_choice <- 0
    sel_import_cost <- 0}
  selectizeInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='unit_import_cost'],
    choices = import_cost_choice, selected =  sel_import_cost,
    options = list(create = T) )
  
}) }

# render a list of pxk
render_pxk_list <- function(input,config_dict,iid){renderUI({
  conn <- db_open(config_dict)
  pxk_num_list <- dbGetQuery(conn,'select pxk_num from pxk_info')
  dbDisconnect(conn)
  selectizeInput( inputId = iid,
                  label = ui_elem$actual[ui_elem$label=='select_pxk'],
                  choices = as.character(pxk_num_list$pxk_num))
}) }



render_man_pxk_info <- function(input){renderUI({
  pxk_num <- input$man_pxk_list
  pxk_info_str <- get_pxk_info_str(pxk_num)
  HTML(pxk_info_str)
}) }



# render a list of active product
render_prod_name_list <- function(input,config_dict,iid){renderUI({
  active_prod <- product_info$search_str[product_info$active==1]
  selectizeInput(inputId = iid,
                 label = ui_elem$actual[ui_elem$label=='prod_name'],
                 choices=active_prod)
}) }

#render latest price
render_price <- function(input,iid){renderUI({
  current_customer <- input$customer_name
  customer_id <- customer_info$customer_id[
    customer_info$customer_name == current_customer]
  current_prod_name <- input$prod_name_select
  prod_code <- product_info$prod_code[
    product_info$search_str == current_prod_name]
  unit <- input$unit_selector
  # get latest price
  latest_price <- get_latest_price(customer_id, prod_code, unit, pxk_info)
  latest_price <- as.integer(latest_price)
  selectizeInput(inputId = iid,
                 label = ui_elem$actual[
                   ui_elem$label=='unit_price'],
                 choices=latest_price,options = list(create=T))
}) }

#render a list of stt, used for pxk_man
render_pxkman_stt_list <- function(input, config_dict, iid){renderUI({
  selected_pxk_num <- as.integer(input[['man_pxk_list']])
  entry_list <- as.character(get_pxk_entry_num(selected_pxk_num,config_dict))
  selectInput(inputId = iid, label = NULL,
              choices=c(entry_list,ui_elem$actual[ui_elem$label=='all']))
}) }

#render a list of stt for current pxk num only
render_invout_stt_list <- function(config_dict, iid){renderUI({
  current_pxk_num <- get_current_pxk(cofig_dict)
  entry_list <- as.character(get_pxk_entry_num(current_pxk_num,config_dict))
  selectInput(inputId = iid, label = NULL, choices=entry_list)
}) }


# render qty
render_qty <- function(iid){renderUI({
  selectizeInput(inputId = iid,
                 label = ui_elem$actual[ui_elem$label=='qty'],
                 choices=c(1:100),options = list(create=T))
}) }

render_unit <- function(input,iid,type='inv_out'){renderUI({
  if (type=='inv_out'){
    cur_prod_code<- product_info[
      product_info$search_str==input$prod_name_select, "prod_code"]}
  if (type=='inv_in'){
    cur_prod_code<- product_info[
      product_info$search_str==input$in_prodname_select, "prod_code"]}
  
  cur_customer_id <- customer_info$customer_id[
    customer_info$customer_name==input$customer_name]
  unitList <- packaging[packaging$prod_code == cur_prod_code, "unit"]
  unitList <- unique(unitList)
  unitList <- unitList[unitList!='pack']
  if (type=='inv_out'){
    latest_unit <- get_latest_unit(cur_customer_id, cur_prod_code,
                                 sale_log, pxk_info)
    # if there is nothing, default to first unit
    if (length(latest_unit)==0){ 
      latest_unit <- unitList[1]
    }
  }else{ # for other type, default to first unit
    latest_unit <- unitList[1]
    }

  # if there is still nothing, it is problem loading unitList, default to 'wait'
  if (length(latest_unit)==0){
    latest_unit <- 'wait'
  }  
  selectInput(
    inputId = iid,
    label = ui_elem$actual[ui_elem$label=='unit'],
    choices = unitList, selected = latest_unit
  )
}) }

render_lot <- function(input,iid){renderUI({
  current_prod_code <- product_info[
    product_info$search_str==input$prod_name_select, "prod_code"]
  avaiLot <- get_avail_lot(current_prod_code,config_dict)
  selectizeInput(
    inputId = iid, label = "Lot",
    choices = unique(avaiLot), options = list(create = TRUE)
  )
})
}

render_note <- function(iid){renderUI({
  textInput(inputId = iid,
            label = ui_elem$actual[ui_elem$label=='note'],
            value = '')
}) }

# render product info default to inv_out but things can change
render_prod_info <- function(input,type='inv_out'){renderUI({
  product_info_str <- build_prod_info(config_dict,input,type)
  HTML(product_info_str)
}) }

# function to rebuild the productInfo HTML string
build_prod_info <- function(config_dict,input,type){
  inventory <- update_inventory(config_dict)
  if (type=='inv_out'){
    current_select <- product_info[
    product_info$search_str==input$prod_name_select,]
  }
  total_available <-inventory[
    inventory$prod_code == current_select$prod_code &
      inventory$lot == input$lot_select,
    'remaining_qty']

  if(length(total_available)==0){total_available <- 0}
  current_exp_date <- inventory[
    inventory$prod_code == current_select$prod_code &
      inventory$lot == input$lot_select, 'exp_date']
  packaging_str <- packaging[
    packaging$prod_code == current_select$prod_code &
      packaging$unit == input$unit_selector,]
  ordering_unit <- get_ordering_unit(packaging)
  current_order_unit <- ordering_unit$unit[
    ordering_unit$prod_code==current_select$prod_code]
  current_selected_unit <- packaging[
    packaging$unit==input$unit_selector & 
      packaging$prod_code==current_select$prod_code,]
  packaging_str <- paste0(packaging_str$units_per_pack[1],
                          packaging_str$unit[1],'/',current_order_unit)

  return(paste(
    "Information:", '<br/>',
    ui_elem$actual[ui_elem$label=='prod_code'],':',
    current_select$prod_code, '<br/>',
    ui_elem$actual[ui_elem$label=='vendor'],':',
    current_select$vendor,'<br/>',
    "REF: ",current_select$ref_smn,'<br/>',
    ui_elem$actual[ui_elem$label=='exp_date'],':',
    current_exp_date, '<br/>',
    ui_elem$actual[ui_elem$label=='total_available'],':',
    round(total_available*current_selected_unit$units_per_pack,digits=0),
    current_selected_unit$unit,'(',
    round(total_available,digits=1), current_order_unit, ')<br/>',
    ui_elem$actual[ui_elem$label=='packaging_str'],
    ':',packaging_str)
  )
}





build_pxk_status_str <- function(pxk_num,config_dict){
  conn <- db_open(config_dict)
  pxk_status_code <- dbGetQuery(
    conn,paste('select completed from pxk_info where pxk_num =', pxk_num))
  if (nrow(pxk_status_code)<=0){
    status <- ui_elem$actual[ui_elem$label=='new']
  }else{
    if (pxk_status_code == 1){
      status <- 'completed'
    }else{
      status <- 'in_progress'
    }
  }
  dbDisconnect(conn)
  tmp1 <- ui_elem$actual[ui_elem$label=='pxkNum']
  tmp2 <- ui_elem$actual[ui_elem$label=='Status']
  current_status <- check_pxk_num(pxk_num,config_dict)
  current_status <- ui_elem$actual[ui_elem$label==current_status]
  return(paste("<font size='+1'>",tmp1,": ",pxk_num,tmp2,':',
               current_status,'</font><br/>')
  )
}

# render table for the pxk_man tab
render_man_pxktable <- function(input){DT::renderDataTable({
  selected_pxk_num <- as.integer(input$man_pxk_list)
  output <- render_selected_pxk(selected_pxk_num,config_dict)
  DT::datatable(output, options = list(pageLength = 10),rownames=F,
                editable = 'cell')
})
}





render_sys_message <- function(sys_msg){renderUI({
  HTML(sys_msg)
}) }

render_customer_list <- function(iid,type='inv_out',input){renderUI({
  cust_choices <- get_cust_list(config_dict,type)
  # set the label and default customer
  if (type=='add_customer'){
    clabel <- ui_elem$actual[ui_elem$label=='customer_name']
    default_customer <- cust_choices[1]
    allow_add <- T
  }
  if (type=='inv_out'){
    clabel <- ui_elem$actual[ui_elem$label=='customer_name']
    default_customer <- cust_choices[1]
    allow_add <- F
  }
  if (type=='customer_change'){
    clabel <- ui_elem$actual[ui_elem$label=='customer_change']
    man_selected_pxk <- input$man_pxk_list
    conn <- db_open(config_dict)
    man_selected_pxk_info <- dbGetQuery(
      conn, paste('select * from pxk_info where pxk_num =', man_selected_pxk))
    dbDisconnect(conn)
    man_selected_pxk_info <- merge(man_selected_pxk_info, customer_info)
    # print(man_selected_pxk_info)
    default_customer <- man_selected_pxk_info$customer_name[1]
    allow_add <- F
  }
  selectizeInput(
    inputId = iid, label = clabel, choices = cust_choices, 
    selected = default_customer, options = list(create = allow_add))
}) }

render_payment_type <- function(input,iid,ui_type){renderUI({
  conn <- db_open(config_dict)
  payment_type <- dbReadTable(conn,'payment_type')
  payment_type <- merge(payment_type,ui_elem %>% select(label,actual),
                        by.x='payment_label',by.y='label',all.x=T)
  dbDisconnect(conn)
  paymentChoices <- payment_type$actual
  if (ui_type=='inv_out'){
    defaultPay <- payment_type$actual[payment_type$payment_code==0]
    ui_label <- ui_elem$actual[ui_elem$label=='payment_type']
  }
  if (ui_type=='man_pxk'){
    pxk_num <- input$man_pxk_list
    pxk_info <- get_pxk_info(pxk_num, translate = F)
    # print(pxk_info)
    defaultPay <- pxk_info$payment_type[1]
    ui_label <- ui_elem$actual[ui_elem$label=='change_payment_type']
  }
  selectInput(inputId = iid,
              label = ui_label,
              choices = paymentChoices,selected = defaultPay)
}) }

render_warehouse <- function(input,iid,warehouse_info){renderUI({
  warehouseChoices <- warehouse_info$warehouse
  # get default warehouse based on current product
  tmp <- update_inventory(config_dict)
  current_prod_code <- product_info[
    product_info$search_str==input$prod_name_select, "prod_code"]
  default_warehouse_id <- tmp$warehouse_id[
    tmp$prod_code==current_prod_code & 
      tmp$lot==input$lot_select]
  if (length(default_warehouse_id)==0){ # set default if we cannot find sth
    default_warehouse <- warehouseChoices[1]
  }else{
    default_warehouse <- warehouse_info$warehouse[
    warehouse_info$warehouse_id == default_warehouse_id]
  }
  selectInput(inputId = iid,
              label = ui_elem$actual[ui_elem$label=='warehouse'],
              choices = warehouseChoices, selected = default_warehouse)
}) }

# this render the current prod_code list, allow adding prod_code
render_prod_code_list <- function(iid, allow_add = T){renderUI({
  prodcode_list <- product_info$prod_code
  selectizeInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='prod_code'],
    choices = unique(prodcode_list), options = list(create = allow_add)
  )
}) }

render_name_list <- function(input, iid, allow_add = T){renderUI({
  current_prod_code <- input$add_prod_code
  current_name <- product_info$name[product_info$prod_code==current_prod_code]
  if (length(current_name)==0){
    current_name <- ''
  }
  selectizeInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='name'],
    choices = current_name, options = list(create = allow_add)
  )
}) }

render_ref_list <- function(input, iid, allow_add = T){renderUI({
  current_prod_code <- input$add_prod_code
  current_ref <- product_info$ref_smn[product_info$prod_code==current_prod_code]
  if (length(current_ref)==0){
    current_ref <- ''
  }
  selectizeInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='ref_smn'],
    choices = current_ref, options = list(create = allow_add)
  )
}) }

render_add_order_unit <- function(input, iid, allow_add = T){renderUI({
  current_prod_code <- input$add_prod_code
  current_unit <- packaging$unit[
    packaging$prod_code==current_prod_code & packaging$units_per_pack==1]
  if (length(current_unit)==0){
    current_unit <- ''
  }
  selectizeInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='ordering_unit'],
    choices = current_unit, options = list(create = allow_add)
  )
}) }

render_vendor_list <- function(input, iid, allow_add = T,tab='update_db'){
  renderUI({
  if (tab=='update_db'){
    current_vendor <- product_info$vendor[
      product_info$prod_code == input$add_prod_code]
    if (length(current_vendor)==0){
     current_vendor <- vendor_info$vendor
    }
  }
  if (tab=='invoice_update'){
    current_vendor <- vendor_info$vendor[vendor_info$local==0]
    }
  selectizeInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='orig_vendor'],
    choices = current_vendor, options = list(create = allow_add))
  }) 
}

render_add_warehouse <- function(input, iid, allow_add = T){renderUI({
  current_warehouse_id <- product_info$warehouse_id[
    product_info$prod_code == input$add_prod_code]
  if (length(current_warehouse_id)==0){
    warehouse_choice <- warehouse_info$warehouse
  }else{
    warehouse_choice <- warehouse_info$warehouse[
      warehouse_info$warehouse_id==current_warehouse_id]
  }
  selectizeInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='warehouse'],
    choices = warehouse_choice, options = list(create = allow_add)
  )
}) }

render_add_prod_type <- function(input, iid){renderUI({
  selectInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='prod_type'],
    choices = product_type$actual )
}) }

