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
    input, 'warehouse_selector') # warehouse
  output$unit_price <- render_price(input,iid='unit_price')
  output$payment_selector <- render_payment_type('payment_type') # payment
  output$pxk_note <- render_note(iid='pxk_note') #Note
  output$prod_info_str <- render_prod_info(input) #product Info pane
  # side
  output$current_pxk_info <- render_current_pxk_infostr(
    config_dict) #current pxk info
  output$current_pxk_tbl <- render_invout_pxktable()
  output$invout_stt_list <- render_invout_stt_list(
    config_dict, 'invout_stt_list')
  
  # inv_out UI buttons handlers
  observeEvent(input$inventory_out, { # inv_out button
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
        completed = 0
      )
      conn <- db_open(config_dict)
      dbWriteTable(conn,'pxk_info',appendPXKInfo,append=T)
      pxk_info <- dbReadTable(conn,"pxk_info")
      dbDisconnect(conn)
      # set current_stt also
      current_stt <- 1
      #otherwise, read the info from the sale_log
    }else{
      conn <- db_open(config_dict)
      current_stt <- dbGetQuery(conn, "select max(stt) from sale_log
                               where pxk_num = (
                               select pxk_num from pxk_info
                               where completed = 0)")[1,1]
      dbDisconnect(conn)
      # if there is a result, increase by 1, otherwise set to 1
      if (is.na(current_stt)){
        current_stt <- 1
      }else{
        current_stt <- current_stt+1
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
      output$sys_msg <- render_sys_message(
        ui_elem$actual[ui_elem$label=='inv_exceed'])
    }
    # refresh the UI after sucessfull inventory_out
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
    finalised_pxk_warehouse <- input$warehouse_selector
    finalised_pxk_num <- get_current_pxk(config_dict)
    
    conn <- db_open(config_dict)
    query <- paste0("update pxk_info set completed = 1
                    where pxk_num = '",finalised_pxk_num,"'")
    res <- dbSendStatement(conn,query)
    dbClearResult(res)
    dbDisconnect(conn)
    new_pxk <- get_current_pxk(cofig_dict)
    
    
    # create new PXK file
    orig_path <- config_dict$value[config_dict$name=='pxk_form']
    dest_path <- file.path(config_dict$value[config_dict$name=='pxk_out_path'],
                           paste0(finalised_pxk_warehouse,".PXK.",
                                  finalised_pxk_num,".xlsx"))
    # file.copy(orig_path,dest_path)
    wb <- loadWorkbook(orig_path)
    
    # get the expDate, if a Lot has 2 expDate, select only the 1st
    # need to get all items, not just positive ones
    tmp <- update_inventory(config_dict,pos_item=FALSE)
    exp_date <- tmp %>% select(prod_code,lot,exp_date) %>% unique()
    exp_date <- exp_date[!duplicated(exp_date$lot),]
    
    # read the data
    conn <- db_open(config_dict)
    query <- paste("SELECT sale_log.stt, product_info.name, product_info.ref_smn,
                   sale_log.unit, sale_log.unit_price,
                   sale_log.qty,sale_log.lot
                   FROM   sale_log INNER JOIN product_info
                   ON     sale_log.prod_code = product_info.prod_code
                   WHERE  sale_log.pxk_num =",finalised_pxk_num)
    
    form_data <- dbGetQuery(conn,query)
    form_data <- merge(form_data,exp_date,all.x=T)
    
    # calculate total price
    form_data$total_price <- form_data$unit_price*form_data$qty
    
    # get customer data
    query <- paste("SELECT DISTINCT customer_info.customer_name
                    FROM pxk_info INNER JOIN customer_info
                    ON pxk_info.customer_id = customer_info.customer_id
                    WHERE pxk_info.PXK_num =", finalised_pxk_num)
    printingCustomerName <- dbGetQuery(conn,query)
    printingCustomerName <- printingCustomerName$customer_name[1]
    
    output_info <- dbGetQuery(
      conn,'select * from output_info where type = "pxk_output"')
    dbDisconnect(conn)
    
    # writing customer_name
    customerNameRow <- as.numeric(
      output_info$value[output_info$name=='customerNameRow'])
    customerNameCol <- as.numeric(
      output_info$value[output_info$name=='customerNameCol'])
    
    writeData(wb,sheet=1,printingCustomerName, startRow=customerNameRow, 
              startCol=customerNameCol, colNames = F)
    
    # writing pxkNum
    pxkNumRow <- as.numeric(output_info$value[output_info$name=='pxkNumRow'])
    pxkNumCol <- as.numeric(output_info$value[output_info$name=='pxkNumCol'])
    writeData(wb,sheet=1,finalised_pxk_num,startRow=pxkNumRow, 
              startCol=pxkNumCol, colNames = F)
    
    # writing current date
    date_row <- as.numeric(output_info$value[output_info$name=='date_row'])
    date_col <- as.numeric(output_info$value[output_info$name=='date_col'])
    writeData(wb, sheet=1, Sys.Date(), startRow=date_row, startCol=date_col, 
              colNames = F)
    
    # writing payment type
    out_payment_type <- input$payment_type
    payment_type_row <- as.numeric(
      output_info$value[output_info$name == 'payment_type_row'])
    payment_type_col <- as.numeric(
      output_info$value[output_info$name == 'payment_type_col'])
    writeData(wb, sheet=1, out_payment_type, startRow=payment_type_row, 
              startCol=payment_type_col, colNames = F)    
    
    # get pxkDataHeaders
    pxkDataHeaders <-  data.frame(matrix(unlist(strsplit(
      output_info$value[output_info$name=='dataHeaders'],';')),nrow=1))
    
    # rearrange Data and write
    form_data <- form_data[order(as.numeric(form_data$stt)),]
    dataColumns <- unlist(strsplit(
      output_info$value[output_info$name=='dataToWrite'],';'))
    
    form_data <- form_data[,dataColumns]
    
    
    # write both data and headers
    dataStartRow <- as.numeric(
      output_info$value[output_info$name=='dataStartRow'])
    dataStartCol <- as.numeric(
      output_info$value[output_info$name=='dataStartCol'])
    #write headers first
    writeData(wb,sheet=1,pxkDataHeaders, startRow=dataStartRow,
              startCol=dataStartCol, colNames=F)
    # data is one row below
    writeData(wb,sheet=1,form_data,startRow=dataStartRow+1,
              startCol=dataStartCol, colNames=F)
    # save the excel sheet
    saveWorkbook(wb,dest_path,overwrite = T)
    
    # open the file
    system(paste0('open ','"',dest_path,'"'))
    
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
  })

  # ------------------------------- lookup UI ----------------------------------
  output$lookup_tbl_output <- DT::renderDataTable({
    table_name <- ui_elem$label[
      ui_elem$actual==input$lu_tbl_selector]
    # print(table_name)
    create_lookup_tbl(table_name,config_dict)
  },rownames=F)
  
  # ----------------------------- report UI ------------------------------------
  
  output$report_tbl_ouput <- DT::renderDataTable({
    report_type <- ui_elem$label[ui_elem$actual==input$report_type]
    print(report_type)
    rp_filename <- get_rp_filename(report_type, config_dict)
    create_report(report_type,rp_filename,config_dict,input)
  },rownames=F)
  
  # create the report and open it
  observeEvent(input$printReport, {
    # gather all data
    report_type <- ui_elem$label[ui_elem$actual==input$report_type]
    print(report_type)
    rp_data <- build_rp_data(report_type,input)
    if (report_type == 'sale_profit_report'){
      from_date <- input$from_date # from_date and to_date depends on rp type
      to_date <- input$to_date
    }else{
      from_date <- strftime(Sys.Date())
      to_date <- from_date
    }
    if (report_type == 'inv_value_report'){
      rp_filename <- write_inv_value_rp()
    }else{
      rp_filename <- write_report_data(report_type, rp_data, from_date, to_date)
    }
    system(paste0('open ','"',rp_filename,'"'))
  })

  # ---------------------------- pxk_man UI ------------------------------------
  # renderers
  output$man_pxk_list <- render_pxk_list(
    input,config_dict,'man_pxk_list') #pxk_list
  output$stt_select <- render_pxkman_stt_list(
    input,config_dict, iid='stt_select') #select_stt
  output$sys_msg <- render_sys_message('ready')
  output$pxk_detail <- render_man_pxktable(input)
  
  # button handlers
  observeEvent(input$delete_stt_man,{
    # print(input$pxk_list)
    selected_pxk_num <- as.integer(input$man_pxk_list)
    full_stt_list <- get_pxk_entry_num(selected_pxk_num,config_dict)
    trans_list <- data.frame(label=c(full_stt_list,'all'),
                             localised=c(full_stt_list,
                                         ui_elem$actual[ui_elem$label=='all'])
                             )
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
  
  
})