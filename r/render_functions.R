# all reactive functions used to render shiny UI

# render a list of pxk
render_pxk_list <- function(input,config_dict,iid){renderUI({
  conn <- db_open(config_dict)
  pxk_num_list <- dbGetQuery(conn,'select pxk_num from pxk_info')
  dbDisconnect(conn)
  selectizeInput( inputId = iid,
                  label = ui_elem$actual[ui_elem$label=='select_pxk'],
                  choices = as.character(pxk_num_list$pxk_num))
}) }

get_pxk_info_str <- function(pxk_num){
  man_pxk_info <- get_pxk_info(pxk_num)
  pxk_info_str <- '<font size=+1>'
  for (i in 1:length(man_pxk_info)){
    pxk_info_str <- paste0(
      pxk_info_str, names(man_pxk_info)[i],':', em(man_pxk_info[1,i]),
      '&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp')
  }
  pxk_info_str <- paste(pxk_info_str,'<font><br/>')
  return(pxk_info_str)
}

render_man_pxk_info <- function(input){renderUI({
  pxk_num <- input$man_pxk_list
  pxk_info_str <- get_pxk_info_str(pxk_num)
  HTML(pxk_info_str)
}) }

get_pxk_info <- function(pxk_num,translate=TRUE){
  # read info for pxk_num
  # print(pxk_num)
  conn <- db_open(config_dict)
  query <- paste('select * from pxk_info where pxk_num =',pxk_num)
  current_pxk_info <- dbGetQuery(conn, query)
  dbDisconnect(conn)
  if (nrow(current_pxk_info)==0){
    current_pxk_info[1,1] <- 1
    current_pxk_info$pxk_num[1] <- pxk_num
  }
  # recover information
  current_pxk_info <- merge(current_pxk_info,customer_info,all.x=T)
  current_pxk_info <- merge(current_pxk_info,payment_type,all.x=T)
  current_pxk_info <- merge(
    current_pxk_info, ui_elem, by.x = 'payment_label', by.y = 'label', 
    all.x=T) %>% rename(payment_type = actual)
  
  # translate completed column
  current_pxk_info$label <- ifelse(
    is.na(current_pxk_info$completed), 'new', ifelse(
           current_pxk_info$completed==1, 'completed', 'in_progress'))
  current_pxk_info <- merge(
    current_pxk_info, ui_elem %>% select(label,actual))
  current_pxk_info <- current_pxk_info %>% rename(status = actual)
  # select relevant column and format output
  current_pxk_info <- current_pxk_info %>% 
    select(pxk_num,customer_name, payment_type, status)
  current_pxk_info$pxk_num <- as.character(current_pxk_info$pxk_num)
  # translate the output
  if (translate){
  	current_pxk_info <- translate_tbl_column(current_pxk_info,ui_elem)
  }
  return(current_pxk_info)
}

# render a list of active product
render_prod_name_list <- function(input,product_info,iid){renderUI({
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

render_unit <- function(input,iid){renderUI({
  cur_prod_code<- product_info[product_info$search_str==input$prod_name_select,
                               "prod_code"]
  cur_customer_id <- customer_info$customer_id[
    customer_info$customer_name==input$customer_name]
  unitList <- packaging[packaging$prod_code == cur_prod_code, "unit"]
  unitList <- unique(unitList)
  unitList <- unitList[unitList!='pack']
  latest_unit <- get_latest_unit(cur_customer_id, cur_prod_code,
                                 sale_log, pxk_info)
  # if there is nothing, default to first unit
  if (length(latest_unit)==0){ 
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
            label = ui_elem$actual[ui_elem$label=='Note'],
            value = '')
}) }

render_prod_info <- function(input){renderUI({
  product_info_str <- build_prod_info(config_dict,input)
  HTML(product_info_str)
}) }

# function to rebuild the productInfo HTML string
build_prod_info <- function(config_dict,input){
  conn <- db_open(config_dict)
  product_info <- dbReadTable(conn,"product_info")
  dbDisconnect(conn)
  inventory <- update_inventory(config_dict)
  
  current_select <- product_info[product_info$name==input$prod_name_select,]
  
  total_available <- round(inventory[
    inventory$prod_code == current_select$prod_code &
      inventory$lot == input$lot_select,
    'remaining_qty'],digits=2)
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
  packaging_str <- paste0(packaging_str$units_per_pack[1],
                          packaging_str$unit[1],'/',current_order_unit)
  return(paste(
    "Information:", '<br/>',
    ui_elem$actual[ui_elem$label=='prod_code'],':',
    current_select$prod_code, '<br/>',
    ui_elem$actual[ui_elem$label=='vendor'],':',
    current_select$vendor,
    "REF: ",current_select$ref_smn,'<br/>',
    ui_elem$actual[ui_elem$label=='exp_date'],':',
    current_exp_date, '<br/>',
    ui_elem$actual[ui_elem$label=='total_available'],':',
    total_available, current_order_unit, '<br/>',
    ui_elem$actual[ui_elem$label=='packaging_str'],
    ':',packaging_str)
  )
}

render_current_pxk_infostr <- function(config_dict){renderUI({
  pxk_num <- get_current_pxk(config_dict) # get the current pxk_num
  current_pxk_str <- get_pxk_info_str(pxk_num)
  HTML(current_pxk_str)
}) }



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

# render table for the invout tab
render_invout_pxktable <- function(){DT::renderDataTable({
  current_pxk <- get_current_pxk(config_dict)
  output <- render_selected_pxk(current_pxk,config_dict)
  
  DT::datatable(output, options = list(pageLength = 10),rownames=F)
})
}

render_sys_message <- function(sys_msg){renderUI({
  HTML(sys_msg)
}) }

render_customer_list <- function(iid,type='inv_out',input){renderUI({
  cust_choices <- get_cust_list(config_dict,type)
  # set the label and default customer
  if (type=='inv_out'){
    clabel <- ui_elem$actual[ui_elem$label=='customer_name']
    default_customer <- cust_choices[1]
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
  }
  selectizeInput(
    inputId = iid, label = clabel, choices = cust_choices, 
    selected = default_customer)
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
              choices = default_warehouse)
}) }