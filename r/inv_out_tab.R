# do everything when press inv_out button
exec_inv_out <- function(input,output){
  # custom display message
  output$sys_msg <- render_sys_message('please wait....')
  
  # read info from database
  current_pxk <- get_current_pxk(config_dict)
  
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
    dbDisconnect(conn)
    reload_tbl(config_dict, 'pxk_info')
    
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
    current_warehouse_id <- warehouse_info$warehouse_id[
      warehouse_info$warehouse == input$warehouse_selector]
    append_sale_log$warehouse_id <- current_warehouse_id
    current_tender_id <- tender_info$tender_id[
      tender_info$customer_tender_name==input$tender_name & 
        tender_info$warehouse_id==current_warehouse_id]
    
    # special handling of no tender
    tender_0_name <- tender_info$customer_tender_name[
      tender_info$tender_id==0]
    if(input$tender_name==tender_0_name){
      current_tender_id <- 0
    }
    append_sale_log$tender_id <- current_tender_id
    
    # writing to database
    append_tbl_rld(config_dict,'sale_log',append_sale_log)
    # update sys_msg
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

# get the current pxk
get_current_pxk <- function(cofig_dict){
  admin_id <- config_dict$value[config_dict$name=='admin_id']
  if (length(admin_id)!=1){
    stop('Critical Error! admin_id not found')
  }
  conn <- db_open(config_dict)
  pxk_num_list <- dbGetQuery(conn,'select pxk_num from pxk_info')
  current_pxk <- dbGetQuery(
    conn,paste0("select pxk_num from pxk_info where completed = 0 and ",
                "admin_id = ",admin_id))
  dbDisconnect(conn)
  if (nrow(current_pxk)>0){
    newPXK = current_pxk$pxk_num[1]
  }else{
    currentDate <- strftime(Sys.time(),'%d%m%y')
    i <- 1;newPXKNum <- F
    while (!newPXKNum){
      tmp_num <- as.numeric(paste0(admin_id, currentDate,
                                   sprintf("%02d",i)))
      if (length(pxk_num_list[pxk_num_list$pxk_num==tmp_num,'pxk_num'])==0){
        newPXK <- tmp_num
        newPXKNum <- T
      }else{
        i <- i+1
      }
    }
  }
  return(newPXK)
}

render_current_pxk_infostr <- function(config_dict){renderUI({
  pxk_num <- get_current_pxk(config_dict) # get the current pxk_num
  current_pxk_str <- get_pxk_info_str(pxk_num)
  HTML(current_pxk_str)
}) }

# an ui line that display pxk information
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

# function to render pxk information for get_pxk_info_str
get_pxk_info <- function(pxk_num,translate=TRUE){
  # read info for pxk_num
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
  current_pxk_info <- merge(current_pxk_info,payment_type,all.x=T) %>% 
    rename(payment_type = actual)
  
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

# the get_avail_lot function get a list of available lot, it returns a vector
# if sortType ='fifo', the earliest exp_date will be on top
get_avail_lot <- function(current_prod_code,config_dict,sort_type='fifo'){
  inventory <- update_inventory(config_dict)
  if (sort_type == 'fifo'){
    avail_lot <- inventory[inventory$prod_code==current_prod_code,]
    avail_lot <- avail_lot[order(avail_lot$intexp_date,
                                 na.last = F, # put NA lot first
                                 decreasing = F),] #lowest exp_date first
  }
  avail_lot <- avail_lot$lot
  return(avail_lot)
}