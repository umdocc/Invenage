# Functions to handle button presses
# ------------------------- inv_out buttons ------------------------------------
exec_inv_out <- function(input,output, config_dict){
  # custom display messa
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
    
    conn <- db_open(config_dict)
    dbWriteTable(conn,'sale_log',append_sale_log,append=T)
    dbDisconnect(conn)
    reload_tbl(config_dict, 'sale_log')
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

