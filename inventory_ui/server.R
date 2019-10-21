# Invenage server.R
source("global.R",local = F)
require(dplyr)
shinyServer(function(input, output,session) {
  # # --------------------- custom render functions ------------------------------
  renderPXK <- function(){renderUI({
    current_pxk <- get_current_pxk(config_dict)
    selectizeInput( inputId = "pxk_selector",
                    label = ui_elem$actual[ui_elem$label=='pxkNum'],
                    choices = current_pxk, options = list(create = T))
  }) }
  
  renderProdName <- function(){renderUI({
    selectizeInput(inputId = "prod_name_selector",
                label = ui_elem$actual[ui_elem$label=='prod_name'],
                choices=product_info$name)
  }) }
  
  renderQty <- function(){renderUI({
    selectizeInput(inputId = "qty_selector",
                   label = ui_elem$actual[ui_elem$label=='qty'],
                   choices=c(1:100),options = list(create=T))
  }) }
  
  renderUnit <- function(){renderUI({
    current_prod_code<- product_info[product_info$name==input$prod_name_selector,
                                     "prod_code"]
    unitList <- packaging[packaging$prod_code == current_prod_code,"unit"]
    unitList <- unique(unitList)
    unitList <- unitList[unitList!='pack']
    selectInput(
      inputId = "unit_selector",
      label = ui_elem$actual[ui_elem$label=='unit'],
      choices = unitList
    )
  }) }
  
  renderLot <- function(){renderUI({
    current_prod_code <- product_info[product_info$name==input$prod_name_selector,
                                      "prod_code"]
    avaiLot <- get_avail_lot(current_prod_code,config_dict)
    selectizeInput(
      inputId = "lot_selector", label = "Lot",
      choices = unique(avaiLot), options = list(create = TRUE)
    )
  })
  }
  renderNote <- function(){renderUI({
    textInput(inputId = 'pxk_note',
              label = ui_elem$actual[ui_elem$label=='Note'],
              value = '')
  }) }
  renderProdInfo <- function(){renderUI({
    product_info_str <- build_prod_info(config_dict,input)
    HTML(product_info_str)
  }) }
  renderCustomer <- function(){renderUI({
    custChoices <- get_cust_list(config_dict)
    selectizeInput(inputId = 'customer_name',
                label = ui_elem$actual[ui_elem$label=='customer_name'],
                choices = custChoices)
  }) }
  renderPaymentType <- function(){renderUI({
    conn <- db_open(config_dict)
    payment_type <- dbReadTable(conn,'payment_type')
    payment_type <- merge(payment_type,ui_elem %>% select(label,actual),
                          by.x='payment_label',by.y='label',all.x=T)
    dbDisconnect(conn)
    paymentChoices <- payment_type$actual
    defaultPay <- payment_type$actual[payment_type$payment_code==0]
    selectInput(inputId = 'payment_type',
                label = ui_elem$actual[ui_elem$label=='payment_type'],
                choices = paymentChoices,selected = defaultPay)
  }) }
  renderWarehouse <- function(){renderUI({
    conn <- db_open(config_dict)
    warehouse_info <- dbReadTable(conn,'warehouse_info')
    dbDisconnect(conn)
    warehouseChoices <- warehouse_info$warehouse
    # get default warehouse based on current product
    tmp <- update_inventory(config_dict)
    current_prod_code <- product_info[
      product_info$name==input$prod_name_selector, "prod_code"]
    default_warehouse_id <- tmp$warehouse_id[
      tmp$prod_code==current_prod_code & 
        tmp$lot==input$lot_selector]
    default_warehouse <- warehouse_info$warehouse[
      warehouse_info$warehouse_id == default_warehouse_id]
    selectInput(inputId = 'warehouse_selector',
                label = ui_elem$actual[ui_elem$label=='warehouse'],
                choices = default_warehouse)
  }) }
  
  renderCurrentPXK <- function(){renderTable({
    query <- paste("select sale_log.stt, product_info.name, sale_log.unit,
                sale_log.unit_price,sale_log.qty, sale_log.lot,
                sale_log.pxk_num, sale_log.note
                from sale_log inner join product_info
                 on sale_log.prod_code = product_info.prod_code
                 where sale_log.pxk_num =",
                   input$pxk_selector)
    conn <- db_open(config_dict)
    outTable <- dbGetQuery(conn,query)
    dbDisconnect(conn)
    outTable
  }) }
  
  # ---------------------------- Inventory Out Tab -------------------------------
  # Inputs
  output$pxk_selector <- renderPXK() # PXK
  output$lot_selector <- renderLot() # Lot
  output$prod_name_selector <- renderProdName() # prod_name
  output$qty_selector <- renderQty() #Qty
  output$unit_selector <- renderUnit() #Unit
  output$pxk_note <- renderNote() #Note
  output$prod_info_str <- renderProdInfo() #product Info pane
  output$customer_selector <- renderCustomer() # customer
  output$payment_selector <- renderPaymentType() # customer
  output$warehouse_selector <- renderWarehouse() # customer
  
  
  # Buttons
  # Inventory Out will write transaction to database
  observeEvent(input$inventory_out, {
    # Write the event to transaction table to keep track of the transaction
    # connect to database, also re-read pxk_info
    conn <- db_open(config_dict)
    sale_log <- dbReadTable(conn,"sale_log")
    pxk_info <- dbReadTable(conn,"pxk_info")
    payment_type <- dbReadTable(conn,"payment_type")
    warehouse_info <- dbReadTable(conn,"warehouse_info")
    dbDisconnect(conn)
    payment_type <- merge(payment_type,ui_elem,
                          by.x='payment_label',by.y='label')
    
    
    # if this PXK is not in the database yet, create new with completed 0
    # print(nrow(pxk_info[pxk_info$pxk_num==input$pxk_selector,])==0)
    if (nrow(pxk_info[pxk_info$pxk_num==input$pxk_selector,])==0){
      
      appendPXKInfo <- data.frame(
        pxk_num = input$pxk_selector,
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
      current_stt <- dbGetQuery(conn, "select max(Stt) from sale_log
                               where pxk_num = (
                               select pxk_num from pxk_info
                               where completed = 0)")[1,1]
      dbDisconnect(conn)
      is.na(current_stt)
      # if there is a result, increase by 1, otherwise set to 1
      if (is.na(current_stt)){
        current_stt <- 1
      }else{
        current_stt <- current_stt+1
      }
    }
    
    # append the data from input
    append_sale_log <- data.frame(
      stt = current_stt,
      prod_code = unique(product_info[product_info$name==input$prod_name_selector,
                                      "prod_code"]),
      unit = input$unit_selector,
      unit_price = as.integer(input$unit_price),
      qty = input$qty_selector,
      lot = input$lot_selector,
      pxk_num = input$pxk_selector,
      note = input$pxk_note,
      warehouse_id = warehouse_info$warehouse_id[
        warehouse_info$warehouse == input$warehouse_selector]
    )
    print('debug ok')
    # writing sale_log to database
    conn <- db_open(config_dict)
    dbWriteTable(conn,'sale_log',append_sale_log,append=T)
    sale_log <- dbReadTable(conn,'sale_log')
    dbDisconnect(conn)
    
    # refresh the UI after sucessfull inventory_out
    output$prod_name_selector <- renderProdName() # prod_name
    output$qty_selector <- renderQty() #Qty
    output$lot_selector <- renderLot() # Lot
    output$prod_info_str <- renderProdInfo() #product Info pane
    output$pxk_note <- renderNote() #Note
    output$pxk_selector <- renderPXK() # PXK
    output$current_pxk_tbl <- renderCurrentPXK() # reload the PXK table
  })
  
  observeEvent(input$reload_pxk, {
    output$current_pxk_tbl <- renderCurrentPXK()
  })
  
  observeEvent(input$del_last_entry,{
    conn <- db_open(config_dict)
    query <- paste("delete from sale_log where pxk_num =",input$pxk_selector,
                   "and stt = (select max(stt) from sale_log where pxk_num =",
                   input$pxk_selector,")")
    res <- dbSendStatement(conn,query)
    dbClearResult(res)
    dbDisconnect(conn)
    
    #refresh UI
    output$prod_info_str <- renderProdInfo() #product Info pane
    output$current_pxk_tbl <- renderCurrentPXK() # current PXK table
  })
  
  observeEvent(input$complete_form,{
    conn <- db_open(config_dict)
    query <- paste0("update pxk_info set completed = 1
                    where pxk_num = '",input$pxk_selector,"'")
    res <- dbSendStatement(conn,query)
    dbClearResult(res)
    dbDisconnect(conn)
    new_pxk <- get_current_pxk(cofig_dict)
    
    # getting data to write to excel
    finalised_pxk_num <- input$pxk_selector
    finalised_pxk_warehouse <- input$warehouse_selector
    
    # create new PXK file
    orig_path <- config_dict$value[config_dict$name=='pxk_form']
    dest_path <- file.path(config_dict$value[config_dict$name=='pxk_out_path'],
                           paste0(finalised_pxk_warehouse,".PXK.",
                                  finalised_pxk_num,".xlsx"))
    # file.copy(orig_path,dest_path)
    wb <- loadWorkbook(orig_path)
    
    # get the expDate, if a Lot has 2 expDate, select only the 1st
    # need to get all items, not just positive ones
    tmp <- update_inventory(config_dict,pos_item=TRUE)
    exp_date <- tmp %>% select(prod_code,lot,exp_date) %>% unique()
    exp_date <- exp_date[!duplicated(exp_date$lot),]
    
    # read the data
    conn <- db_open(config_dict)
    query <- paste("SELECT sale_log.stt, product_info.name, product_info.ref_smn,
                   sale_log.unit, sale_log.unit_price,
                   sale_log.qty,sale_log.lot
                   FROM   sale_log INNER JOIN product_info
                   ON     sale_log.prod_code = product_info.prod_code
                   WHERE  sale_log.pxk_num =",input$pxk_selector)
    
    form_data <- dbGetQuery(conn,query)
    form_data <- merge(form_data,exp_date,all.x=T)
    
    # calculate total price
    form_data$total_price <- form_data$unit_price*form_data$qty
    
    # get customer data
    query <- paste("SELECT DISTINCT customer_info.customer_name
                    FROM pxk_info INNER JOIN customer_info
                    ON pxk_info.customer_id = customer_info.customer_id
                    WHERE pxk_info.PXK_num =", input$pxk_selector)
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
    
    # ------------- completeForm UI refresh ------------------------------------
    output$pxk_selector <- renderPXK() # PXK
    output$customer_selector <- renderCustomer() # customer
    output$prod_name_selector <- renderProdName() # prod_name
    output$qty_selector <- renderQty() #Qty
    output$lot_selector <- renderLot() # Lot
    output$prod_info_str <- renderProdInfo() #product Info pane
    output$pxk_note <- renderNote() #Note
  })
  #   
  # ------------------------ UI for the Lookup Tab -----------------------------
  output$lookup_tbl_output <- renderDataTable({
    tableName <- ui_elem$label[
      ui_elem$actual==input$lu_tbl_selector]
    if (tableName=='inventory'){
      lookup_tbl_output <- update_inventory(config_dict)
      lookup_tbl_output <- merge(
        lookup_tbl_output, product_info %>% select(prod_code,name,ref_smn),
        all.x = T) %>%
        select(name,ref_smn,lot,exp_date,remaining_qty)
    }else{
      # query on simple table
      if (tableName=='product_info'){
        query <- paste("SELECT prod_code,name,vendor,ref_smn from product_info")
      }
      if (tableName=='import_price'){
        query <- paste("SELECT product_info.name, product_info.vendor,
                        product_info.ref_smn, import_price.import_price,
                        import_price.currency_code,
                        import_price.min_order, import_price.last_updated
                        FROM import_price INNER JOIN product_info
                        ON import_price.prod_code = product_info.prod_code")
      }
      # if (tableName=='comingList'){
      #   query <- paste0("SELECT comingList.Name, comingList.ref_smn, 
      #                   comingList.NSX, comingList.Quantity, comingList.poName,
      #                   poInfo.Note, ui_elem.actual as Status
      #                   FROM comingList INNER JOIN poInfo
      #                   ON poInfo.poName = comingList.poName
      #                   INNER JOIN poStatusCode ON
      #                   poInfo.poStatusCode = poStatusCode.poStatusCode
      #                   INNER JOIN ui_elem ON 
      #                   poStatusCode.label = ui_elem.label
      #                   where ui_elem.appLang like '",appLang,"'")
      # }
      # if (tableName=='poInfo'){
      #   query <- paste0("SELECT poInfo.poName as POName, poInfo.Note, 
      #                   ui_elem.actual as Status from poInfo 
      #                   inner join poStatusCode on 
      #                   poInfo.poStatusCode = poStatusCode.poStatusCode 
      #                   inner join ui_elem on 
      #                   poStatusCode.label = ui_elem.label 
      #                   where ui_elem.appLang like '",appLang,"' and
      #                   poInfo.poStatusCode < 9 order by POName asc")
      # }
      if (tableName=='sale_log'){
        query <- paste("SELECT product_info.name, product_info.vendor,
                        product_info.ref_smn, sale_log.unit,
                        sale_log.unit_price, sale_log.qty,
                        sale_log.lot, sale_log.pxk_num, customer_info.customer_name
                        FROM sale_log INNER JOIN product_info
                        ON sale_log.prod_code = product_info.prod_code
                        INNER JOIN pxk_info
                        ON sale_log.pxk_num = pxk_info.pxk_num
                        INNER JOIN customer_info
                        ON pxk_info.customer_id = customer_info.customer_id"
        )
      }
      if (tableName=='import_log'){
        query <- paste("SELECT product_info.name, product_info.vendor,
                        product_info.ref_smn, import_log.unit,
                        import_log.qty, import_log.po_name,
                        import_log.lot, import_log.exp_date, 
                        import_log.delivery_date
                        FROM import_log INNER JOIN product_info
                        ON import_log.prod_code = product_info.prod_code"
        )
      }
      conn <- db_open(config_dict)
      lookup_tbl_output <- dbGetQuery(conn,query)
      dbDisconnect(conn)
    }
    lookup_tbl_output
  })
  
  # --------------------- UI for the Reports tab -------------------------------

  # printReport action
  observeEvent(input$printReport, {
    report_type <- ui_elem$label[ui_elem$actual==input$report_type]
    print(report_type)
    if (report_type == 'inventoryValueReport'){
      rp_file_name <- file.path(
        config_dict$value[config_dict$name=='report_out_path'],
        paste0(ui_elem$actual[ui_elem$label=='inventory'],
               '.',Sys.Date(), '.xlsx')
        )

      summary_sheet_name <- ui_elem$actual[ui_elem$label=='summary']
      missing_price_sheet_name <- ui_elem$actual[ui_elem$label=='missing_price']
      totalNSXcostName <- ui_elem$actual[ui_elem$label=='total_inv_value']
      removeCountry <- TRUE # format the vendor
      
      # refresh information
      inventory <- update_inventory(config_dict)

      if (removeCountry){
        inventory$vendor <- gsub('-.*$','',inventory$vendor)
      }
      val_by_vendor <- inventory %>% group_by(vendor) %>% summarise(
        total_inv_value = sum(total_inv_value,na.rm=T)) %>% ungroup
      val_by_vendor <- val_by_vendor[!is.na(val_by_vendor$vendor),]
      vendor_list <- gsub('-.*$','',val_by_vendor$vendor)

      # add total cost, and format the ouput
      tmp <- val_by_vendor[1:2,]
      tmp[1,] <- ''
      tmp$vendor[2] <- ui_elem$actual[ui_elem$label=='total_inv_value']
      tmp$total_inv_value[2] <- sum(val_by_vendor$total_inv_value,na.rm=T)
      val_by_vendor <- rbind(val_by_vendor,tmp)
      val_by_vendor$total_inv_value <- format(
        as.numeric(val_by_vendor$total_inv_value), big.mark=",")
            
      # this report use a new excel for now
      wb <- createWorkbook()
      addWorksheet(wb, summary_sheet_name)
      addWorksheet(wb, missing_price_sheet_name)

      # write missing_price
      missing_price <- inventory[is.na(inventory$ave_pack_import_cost),] %>%
        select(prod_code,name,ref_smn)
      missing_price <- rename_table(missing_price,ui_elem)
      writeData(wb, sheet=missing_price_sheet_name, missing_price)
      # write summary sheet
      val_by_vendor <- rename_table(val_by_vendor,ui_elem)
      writeData(wb, sheet=summary_sheet_name, val_by_vendor)

      for (i in 1:length(vendor_list)){
        addWorksheet(wb, vendor_list[i])
        tmp_df <- inventory[grepl(vendor_list[i],inventory$vendor),] %>%
        select(name,ref_smn,lot,exp_date,remaining_qty,ave_pack_import_cost,
               total_inv_value)
        tmp_df <- rename_table(tmp_df,ui_elem)
        writeData(wb, sheet=vendor_list[i], tmp_df)
      }
      saveWorkbook(wb,rp_file_name,overwrite = T)
      system(paste0('open ','"',rp_file_name,'"'))
    }
    if (report_type == 'inventoryAuditReport'|
        report_type == 'inventoryOrderReport'){
      # read the form
      orig_file <- config_dict$value[config_dict$name=='report_form_path']
      wb <- loadWorkbook(orig_file)
      
      # read the inventory
      inventoryReport <- update_inventory(config_dict)
      # set all negative number to 0
      inventoryReport <- inventoryReport[inventoryReport$remaining_qty>0,]
      
      # if this is ordering report, group and sum
      if (report_type == 'inventoryOrderReport'){
        inventoryReport <- inventoryReport %>% group_by(prod_code) %>% 
          summarise(total_remaining_qty = sum(remaining_qty)) %>% ungroup
        # merge with prod_info so that we get zero items as well
        inventoryReport <- merge(inventoryReport,product_info %>% 
                                   select(prod_code,type),all.y=T)
      }
      #recover human-readble info
      inventoryReport <- merge(
        inventoryReport, product_info %>% select(
          prod_code,name,vendor,ref_smn,warehouse_id))
      inventoryReport <- merge(
        inventoryReport,warehouse_info %>% select(warehouse_id,warehouse))
      # select the appropriate column
      if (report_type == 'inventoryOrderReport'){
        inventoryReport <- inventoryReport %>%
          select(name,vendor,ref_smn,total_remaining_qty,warehouse)
      }else{
        inventoryReport <- inventoryReport %>%
          select(name,vendor,ref_smn,remaining_qty,
                 lot,exp_date,warehouse)
      }
      
      # write data to destination file then open file
      writeData(wb, 1, inventoryReport, startRow=5, startCol=1)
      file_name <- paste(input$report_type,format(Sys.Date(),'%d%m%y'),'xlsx',
                         sep='.')
      dest_file <- file.path(
        config_dict$value[config_dict$name=='report_out_path'],file_name)
      print(dest_file)
      saveWorkbook(wb,dest_file,overwrite = T)
      system(paste0('open ','"',dest_file,'"'))
    }
  })
})