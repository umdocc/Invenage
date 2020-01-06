# Invenage server.R
source("global.R",local = F)
require(dplyr)
require(DT)
shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp) #make shiny like a normal app
  # -------------------------------inv_out UI ----------------------------------
  # sidebar
  output$customer_selector <- render_customer_list('customer_name') # customer
  output$prod_name_select <- render_prod_name_list(
    input,product_info,'prod_name_select') # prod_name
  output$qty_selector <- render_qty(iid='qty_selector') #Qty
  output$unit_selector <- render_unit(input,iid='unit_selector') #Unit
  output$lot_select <- render_lot(input, iid='lot_select') # Lot
  output$warehouse_selector <- render_warehouse(
    input, 'warehouse_selector',warehouse_info) # warehouse
  output$unit_price <- render_price(input,iid='unit_price')
  output$payment_selector <- render_payment_type('payment_type') # payment
  output$pxk_note <- render_note(iid='pxk_note') #Note
  output$prod_info_str <- render_prod_info(input) #product Info pane
  output$sys_msg <- render_sys_message('ready')
  # side
  output$current_pxk_info <- render_current_pxk_infostr(
    config_dict) #current pxk info
  output$current_pxk_tbl <- render_invout_pxktable()
  output$invout_stt_list <- render_invout_stt_list(
    config_dict, 'invout_stt_list')
  
  # inv_out UI buttons handlers
  observeEvent(input$inventory_out, { # inv_out button
    output$sys_msg <- render_sys_message('please wait....')
    # read info from database
    current_pxk <- get_current_pxk(config_dict)
    conn <- db_open(config_dict)
    sale_log <- dbReadTable(conn,"sale_log")
    pxk_info <- dbReadTable(conn,"pxk_info")
    payment_type <- dbReadTable(conn,"payment_type")
    warehouse_info <- dbReadTable(conn,"warehouse_info")
    dbDisconnect(conn)
    payment_type <- merge(payment_type,ui_elem,
                          by.x='payment_label',by.y='label')

    # if this PXK is not in the database yet, create new with completed 0
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
      prod_code = unique(product_info[product_info$name==input$prod_name_select,
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
      append_sale_log$warehouse_id <- warehouse_info$warehouse_id[
        warehouse_info$warehouse == input$warehouse_selector]
      conn <- db_open(config_dict)
      dbWriteTable(conn,'sale_log',append_sale_log,append=T)
      sale_log <- dbReadTable(conn,'sale_log')
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
    # refresh the UI after sucessfull inventory_out
    output$customer_selector <- render_customer_list('customer_name')
    output$prod_name_selector <- render_prod_name_list(
      input,product_info,'prod_name_select') # prod_name
    output$qty_selector <- render_qty(iid='qty_selector') #Qty
    output$lot_select <- render_lot(input, iid='lot_select') # Lot
    output$prod_info_str <- render_prod_info(input) #product Info pane
    output$pxk_note <- render_note(iid='pxk_note') #Note
    output$current_pxk_info <- render_current_pxk_infostr(
      config_dict) #current pxk info
    output$current_pxk_tbl <- render_invout_pxktable() # reload the PXK table
    output$invout_stt_list <- render_invout_stt_list( # reload invout_stt_list
      config_dict, 'invout_stt_list')
    output$man_pxk_list <- render_pxk_list(
      input,config_dict,'man_pxk_list') #reload pxk_list in pxk_man as well
    # output$sys_msg <- render_sys_message('ready') # reload sys message
    
  })
  
  observeEvent(input$del_invout_stt,{
    current_pxk <- get_current_pxk(config_dict)
    current_stt_list <- get_pxk_entry_num(current_pxk,config_dict)
    # if there is record in current pxk, allow delete
    if (length(current_stt_list)>0){
      stt_to_proc <- as.character(input$invout_stt_list)
      delete_pxk(current_pxk,stt_to_proc,config_dict)
    }
    # reload UI
    output$invout_stt_list <- render_invout_stt_list(
      config_dict, 'invout_stt_list')
    output$prod_name_selector <- render_prod_name_list(
      input,product_info,'prod_name_select') # prod_name, required for prod_info
    output$qty_selector <- render_qty(iid='qty_selector') #Qty
    output$lot_select <- render_lot(input, iid='lot_select') # Lot
    output$current_pxk_tbl <- render_invout_pxktable()
    output$prod_info_str <- render_prod_info(input)
  })
  
  observeEvent(input$complete_form,{
    # getting data to write to excel
    # finalised_pxk_warehouse <- input$warehouse_selector
    finalised_pxk_num <- get_current_pxk(config_dict)
    
    # update completed field in databse
    conn <- db_open(config_dict)
    query <- paste0("update pxk_info set completed = 1
                    where pxk_num = ",finalised_pxk_num)

    dbExecute(conn,query)

    dbDisconnect(conn)
    
    # UI refresh
    output$customer_selector <- render_customer_list('customer_name') # customer
    output$current_pxk_tbl <- render_invout_pxktable() # current_pxk_tbl
    output$current_pxk_info <- render_current_pxk_infostr(
      config_dict) #current pxk info
    output$prod_name_selector <- render_prod_name_list(
      input,product_info,'prod_name_select') # prod_name
    output$qty_selector <- render_qty(iid='qty_selector') #Qty
    output$lot_select <- render_lot(input, iid='lot_select') # Lot
    output$prod_info_str <- render_prod_info(input) #product Info pane
    output$pxk_note <- render_note(iid='pxk_note') #Note
    
    # create the excel for current pxk
    dest_path <- create_pxk_file(finalised_pxk_num)
    # open the file
    system(paste0('open ','"',dest_path,'"'))
  })

  # ------------------------------- lookup UI ----------------------------------
  output$lookup_tbl_output <- DT::renderDataTable({
    table_name <- ui_elem$label[
      ui_elem$actual==input$lu_tbl_selector]
    create_lookup_tbl(table_name,config_dict)
  },rownames=F)
  observeEvent(input$print_lu_tbl,{
    table_name <- ui_elem$label[
      ui_elem$actual==input$lu_tbl_selector]
    lu_tbl_out <- create_lookup_tbl(table_name,config_dict)
    dest_path <- file.path(app_path,'lu_tbl.xlsx')
    write.xlsx(lu_tbl_out, dest_path,row.names=F)
    system(paste0('open ','"',dest_path,'"'))
  })
  # ----------------------------- report UI ------------------------------------
  
  output$report_tbl_ouput <- DT::renderDataTable({
    report_type <- ui_elem$label[ui_elem$actual==input$report_type]
    rp_filename <- get_rp_filename(report_type, config_dict)
    create_report(report_type,rp_filename,config_dict,input)
  },rownames=F)
  
  # create the report and open it
  observeEvent(input$printReport, {
    # gather all data
    report_type <- ui_elem$label[ui_elem$actual==input$report_type]
    rp_data <- build_rp_data(report_type,input)
    to_date <- input$to_date
    
    #debug
    print(report_type);print(rp_data)
    
    #from_date and to_date depends on rp type
    if (report_type == 'sale_profit_report'){
      from_date <- input$from_date 
    }else{
      from_date <- strftime(Sys.Date())
    }
    if (report_type == 'inv_value_report'){
      rp_filename <- write_inv_value_rp()
    }else{
      rp_filename <- write_report_data(report_type, rp_data, from_date, to_date)
    }
    system(paste0('open ','"',rp_filename,'"'))
  })

  # ---------------------------- pxk_man UI ------------------------------------
  # sidebar
  output$man_pxk_list <- render_pxk_list(
    input,config_dict,'man_pxk_list') #pxk_list
  output$man_pxk_cust_select <- render_customer_list(
    'customer_change',type='customer_change')
  
  #main
  output$pxk_detail <- render_man_pxktable(input)
  output$stt_select <- render_pxkman_stt_list(
    input,config_dict, iid='stt_select') #select_stt
  
  # button handlers
  observeEvent(input$delete_stt_man,{
    selected_pxk_num <- as.integer(input$man_pxk_list)
    full_stt_list <- as.character(
      get_pxk_entry_num(selected_pxk_num,config_dict))
    trans_list <- data.frame(label=c(full_stt_list,'all'),
                             localised=c(full_stt_list,
                                 ui_elem$actual[ui_elem$label=='all']))
    stt_to_proc <- as.character(
      trans_list$label[
      as.character(trans_list$localised)==as.character(input$stt_select)]
    )
    delete_pxk(selected_pxk_num,stt_to_proc,config_dict)
    # refresh the UI
    output$pxk_detail <- render_man_pxktable(input) # reload the pxk_man table
    output$stt_select <- render_pxkman_stt_list(
    input,config_dict, iid='stt_select')
  })
  
  observeEvent(input$edit_pxk_info,{
    selected_pxk_num <- as.integer(input$man_pxk_list)
    new_customer <- input$customer_change
    new_customer_id <- customer_info$customer_id[
      customer_info$customer_name == new_customer]
    query <- paste(
      'update pxk_info set customer_id =',new_customer_id,"where pxk_num =",
      selected_pxk_num)
    # print(query)
    # writing to database
    conn <- db_open(config_dict)
    dbExecute(conn,query)
    dbDisconnect(conn)
    # refresh the UI
    output$pxk_detail <- render_man_pxktable(input) # reload the pxk_man table
    output$stt_select <- render_pxkman_stt_list(
      input, config_dict, iid='stt_select')
  })
  # table edit handler
  observeEvent(input$pxk_detail_cell_edit,{
    cell <- input$pxk_detail_cell_edit
    current_pxk_num <- input$man_pxk_list
    errorsFree <- edit_db_pxk(cell,current_pxk_num)
    output$pxk_detail <- render_man_pxktable(input) #refresh the pxk
  })
  
  observeEvent(input$print_pxk_man,{
    man_pxk_num <- input$man_pxk_list
    dest_path <- create_pxk_file(man_pxk_num) # create the pxk
    system(paste0('open ','"',dest_path,'"')) #open the file
  })
  # -------------------------- About tab ---------------------------------------
  output$error_text <- renderText(error_text)
})