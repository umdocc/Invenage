# Invenage server.R
source("global.R",local = F)
require(dplyr)
require(DT)
shinyServer(function(input, output,session) {
  # # --------------------- custom render functions ----------------------------
  render_pxk_list <- function(){renderUI({
    conn <- db_open(config_dict)
    pxk_num_list <- dbGetQuery(conn,'select pxk_num from pxk_info')
    dbDisconnect
    selectizeInput( inputId = "pxk_list",
                    label = ui_elem$actual[ui_elem$label=='select_pxk'],
                    choices = pxk_num_list)
  }) }
  # renderPXK <- function(){renderUI({
  #   current_pxk <- get_current_pxk(config_dict)
  #   selectizeInput( inputId = "pxk_selector",
  #                   label = ui_elem$actual[ui_elem$label=='pxkNum'],
  #                   choices = current_pxk, options = list(create = T))
  # }) }
  
  renderProdName <- function(){renderUI({
    selectizeInput(inputId = "prod_name_selector",
                label = ui_elem$actual[ui_elem$label=='prod_name'],
                choices=product_info$name)
  }) }
  render_entry_list <- function(){renderUI({
    selected_pxk_num <- as.integer(input$pxk_list)
    entry_list <- get_pxk_entry_num(selected_pxk_num,config_dict)
    selectInput(inputId = "stt_select",
                   label = ui_elem$actual[ui_elem$label=='select_stt'],
                   choices=c(entry_list,ui_elem$actual[ui_elem$label=='all']))
  }) }
  render_price <- function(){renderUI({
    current_customer <- input$customer_name
    current_prod <- input$prod_name_selector
    current_unit <- input$unit_selector
    sale_lookup <- create_lookup_tbl('sale_log',config_dict,local_name = F)
    conn <- db_open(config_dict)
    pxk_info <- dbReadTable(conn,'pxk_info')
    dbDisconnect(conn)
    # get latest price
    latest_price <- get_latest_price(current_customer,current_prod,current_unit,
                                     sale_lookup,pxk_info)
    latest_price <- as.integer(latest_price)
    selectizeInput(inputId = "unit_price",
                   label = ui_elem$actual[
                     ui_elem$label=='unit_price'],
                   choices=latest_price,options = list(create=T))
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
  
  render_current_pxk_info <- function(){renderUI({
    current_pxk <- get_current_pxk(config_dict)
    current_pxk_str <- render_current_pxk_str(current_pxk,config_dict)
    HTML(current_pxk_str)
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
  # output$pxk_selector <- renderPXK() # PXK
  output$lot_selector <- renderLot() # Lot
  output$prod_name_selector <- renderProdName() # prod_name
  output$qty_selector <- renderQty() #Qty
  output$unit_selector <- renderUnit() #Unit
  output$unit_price <- render_price()
  output$pxk_note <- renderNote() #Note
  output$prod_info_str <- renderProdInfo() #product Info pane
  output$current_pxk_info <- render_current_pxk_info() #current pxk info
  output$customer_selector <- renderCustomer() # customer
  output$payment_selector <- renderPaymentType() # payment
  output$warehouse_selector <- renderWarehouse() # warehouse
  output$pxk_list <- render_pxk_list() #pxk_list
  output$stt_select <- render_entry_list() #select_stt
  
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
  output$lookup_tbl_output <- DT::renderDataTable({
    table_name <- ui_elem$label[
      ui_elem$actual==input$lu_tbl_selector]
    print(table_name)
    create_lookup_tbl(table_name,config_dict)
  },rownames=F)
  
  # --------------------- UI for the Reports tab -------------------------------

  # create the report and open it
  observeEvent(input$printReport, {
    report_type <- ui_elem$label[ui_elem$actual==input$report_type]
    rp_filename <- create_report(report_type,config_dict,input)
    system(paste0('open ','"',rp_filename,'"'))
  })
  # ------------------------ ui for pxk_man tab ----------------------------
  render_current_pxktable <- function(){DT::renderDataTable({
      selected_pxk_num <- as.integer(input$pxk_list)
      output_pxk <- render_selected_pxk(selected_pxk_num,config_dict)
      output_pxk
    }, rownames=F)
  }
  output$pxk_detail <- render_current_pxktable()
  
  observeEvent(input$delete_stt,{
    # print(input$pxk_list)
    selected_pxk_num <- as.integer(input$pxk_list)
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
    output$pxk_detail <- render_current_pxktable()
    output$stt_select <- render_entry_list()
  })
})