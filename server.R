# Invenage server.R
source("global.R",local = F)
require(dplyr)
shinyServer(function(input, output,session) {
# # ------------------- UI Elements for Xuat Kho tab -----------------------------
#   # Info line, use htmlOutput for more controls
  output$pxkSelector <- renderUI({
    currentPXK <- get_current_pxk(config_dict)
    selectizeInput(
      inputId = "pxkSelector",
      label = ui_elem$actual[ui_elem$label=='pxkNum'],
      choices = currentPXK, options = list(create = T))
  })
  
  output$prodNameSelector <- renderUI({
    selectInput(inputId = "prodNameSelector",
                label = ui_elem$actual[ui_elem$label=='prod_name'],
                choices=product_info$name)
  })

  output$qtySelector <- renderUI({
    selectizeInput(inputId = "qtySelector",
                   label = ui_elem$actual[ui_elem$label=='qty'],
                   choices=c(1:100),options = list(create=T))
  })

  output$unitSelector <- renderUI({
    current_prod_code<- product_info[product_info$name==input$prodNameSelector,
                                  "prod_code"]
    unitList <- packaging[packaging$prod_code == current_prod_code,"unit"]
    unitList <- unique(unitList)
    unitList <- unitList[unitList!='pack']
    selectInput(
      inputId = "unitSelector",
      label = ui_elem$actual[ui_elem$label=='unit'],
      choices = unitList
    )
  })

  output$lotSelector <- renderUI({
    current_prod_code <- product_info[product_info$name==input$prodNameSelector,
                                      "prod_code"]
    avaiLot <- get_avail_lot(current_prod_code,config_dict)
    selectizeInput(
      inputId = "lotSelector", label = "Lot",
      choices = unique(avaiLot), options = list(create = TRUE)
    )
  })
  
  output$prod_info_str <- renderUI({
    product_info_str <- build_prod_info(config_dict,input)
    HTML(product_info_str)
  })
  
#   # selector UIs
  output$customerSelector <- renderUI({
    custChoices <- get_cust_list(config_dict)
    selectInput(inputId = 'customer_name',
                 label = ui_elem$actual[ui_elem$label=='customer_name'],
                 choices = custChoices)
  })


  

  


  output$warehouseSelector <- renderUI({
    warehouseList <- warehouseInfo$Warehouse
    selectInput(
      inputId = "warehouseSelector",
      label = ui_elem$actual[ui_elem$label=='Warehouse'],
      choices = warehouseList,
      selected = 'MDS' # dynamic selection later on
    )
  })
  
  output$pxkNote <- renderUI({
    textInput(inputId = 'pxkNote',
              label = ui_elem$actual[ui_elem$label=='Note'],
              value = '')
  })

#   # Buttons
  observeEvent(input$reloadPXK, {
    output$currentPXKTable <- renderTable({
      renderPXK(input$pxkSelector)
    })
  })
  
  observeEvent(input$inventoryOut, {

    # Write the event to transaction table to keep track of the transaction
    # connect to database, also re-read pxk_info
    conn <- db_open(config_dict)
    saleLog <- dbReadTable(conn,"saleLog")
    pxk_info <- dbReadTable(conn,"pxk_info")
    dbDisconnect(conn)


    # if this PXK is not in the database yet, create new with completionCode 0
    # print(nrow(pxk_info[pxk_info$PXKNum==input$pxkSelector,])==0)
    if (nrow(pxk_info[pxk_info$PXKNum==input$pxkSelector,])==0){
      
      appendPXKInfo <- data.frame(
        PXKNum = input$pxkSelector,
        saleDate = format(Sys.Date(),'%d%m%y'),
        customerID = customer_info[
          customer_info$customer_name==input$customer_name,'customerID'],
        PXKType = 'I',
        Warehouse = 'MDS',
        completionCode = 0
      )
      # print(appendPXKInfo)
      # write this to database, then reload pxk_info
      conn <- db_open(config_dict)
      dbWriteTable(conn,'pxk_info',appendPXKInfo,append=T)
      pxk_info <- dbReadTable(conn,"pxk_info")
      dbDisconnect(conn)
      # set currentStt also
      currentStt <- 1
      #otherwise, read the info from the saleLog
    }else{
      conn <- db_open(config_dict)
      currentStt <- dbGetQuery(conn, "select max(Stt) from saleLog
                               where PXKNum = (
                               select PXKNum from pxk_info
                               where completionCode = 0)")[1,1]
      dbDisconnect(conn)
      is.na(currentStt)
      # if there is a result, increase by 1, otherwise set to 1
      if (is.na(currentStt)){
        currentStt <- 1
      }else{
        currentStt <- currentStt+1
      }
    }

    # print(currentStt)
    # append the data from input
    appendSaleLog <- data.frame(
      Stt = currentStt,
      prodCode = unique(product_info[product_info$Name==input$prodNameSelector,
                                    "prodCode"]),
      Unit = input$unitSelector,
      unit_price = as.integer(input$unit_price),
      qty = input$amountSelector,
      Lot = input$lotSelector,
      PXKNum = input$pxkSelector,
      Note = input$pxkNote,
      invoiceTypeCode = 1 # default to 1
    )

    # print(appendSaleLog)

    # writing saleLog to database
    conn <- db_open(config_dict)
    dbWriteTable(conn,'saleLog',appendSaleLog,append=T)
    saleLog <- dbReadTable(conn,'saleLog')
    dbDisconnect(conn)
    
    # rebuild the Inventory table
    Inventory <- updateInventory(importLog,saleLog)
    # ------------------- inventoryOut UI refresh ------------------------------
    # prodName refresh
    output$prodNameSelector <- renderUI({
      selectInput(inputId = "prodNameSelector",
                  label = ui_elem$actual[ui_elem$label=='prodName'],
                  choices=product_info$Name)
    })
    # amount refresh
    output$amountSelector <- renderUI({
      selectizeInput(inputId = "amountSelector",
                     label = ui_elem$actual[ui_elem$label=='qty'],
                     choices=c(1:100),options = list(create=T))
    })
    #lot refresh
    output$lotSelector <- renderUI({
      current_prod_code <- product_info[product_info$Name==input$prodNameSelector, "prodCode"]
      avaiLot <- get_avail_lot(current_prod_code,config_dict)
      selectizeInput(
        inputId = "lotSelector", label = "Lot",
        choices = unique(avaiLot), options = list(create = TRUE)
      )
    })
    # product_infoPane refresh
    output$product_infoPane <- renderUI({
      product_infoPaneStr <- buildProductInfoPane(Inventory,product_info,Packaging,
                                                 input)
      HTML(product_infoPaneStr)
    })
    # Note refresh
    output$pxkNote <- renderUI({
      textInput(inputId = 'pxkNote',
                label = ui_elem$actual[ui_elem$label=='Note'],
                value = '')
    })
    #current PXK refresh
    output$currentPXKTable <- renderTable({
      renderPXK(input$pxkSelector)
    })
    
  })

  observeEvent(input$delLastEntry,{
    conn <- db_open(config_dict)
    query <- paste("delete from saleLog where PXKNum =",input$pxkSelector,
                   "and Stt = (select max(Stt) from saleLog where PXKNum =",
                   input$pxkSelector,")")
    res <- dbSendStatement(conn,query)
    dbClearResult(res)
    # reload saleLog
    saleLog <- dbReadTable(conn,'saleLog')
    dbDisconnect(conn)
    
    # rebuild the Inventory table
    Inventory <- updateInventory(importLog,saleLog)
    #refresh
    # refresh the product_infoPane
    output$product_infoPane <- renderUI({
      product_infoPaneStr <- buildProductInfoPane(Inventory,product_info,
                                                 Packaging,input)
      HTML(product_infoPaneStr)
    })
    
    
    # render current PXK table
    output$currentPXKTable <- renderTable({
      query <- paste("select * from saleLog where PXKNum =",
                     input$pxkSelector)
      conn <- db_open(config_dict)
      outTable <- dbGetQuery(conn,query)
      dbDisconnect(conn)
      outTable
    })
  })
  
  observeEvent(input$completeForm,{
    conn <- db_open(config_dict)
    query <- paste0("update pxk_info set completionCode = 1
                    where PXKNum = '",input$pxkSelector,"'")
    # print(query)
    res <- dbSendStatement(conn,query)
    dbClearResult(res)
    newPXK <- get_new_pxk(cofig_dict)
    dbDisconnect(conn)

    # getting data to write to excel
    printingPXKNum <- input$pxkSelector
    # printingWarehouse <- input$warehouseSelector

    # create new PXK file
    printingWarehouse <- 'MDS'
    origPath <- config_dict$value[config_dict$name=='pxk_form']
    destPath <- file.path(config_dict$value[config_dict$name=='pxk_out_path'],
                          paste0(printingWarehouse,".PXK.",
                                 printingPXKNum,".xlsx"))
    file.copy(origPath,destPath)
    print(origPath)
    wb <- loadWorkbook(origPath)
    # setStyleAction(wb,XLC$"STYLE_ACTION.NONE")

    conn <- db_open(config_dict)
    query <- paste("SELECT saleLog.Stt, product_info.Name, product_info.mfgCode,
                   saleLog.Unit, saleLog.unit_price,
                   saleLog.qty,saleLog.Lot
                   FROM   saleLog INNER JOIN product_info
                   ON     saleLog.prodCode = product_info.prodCode
                   WHERE  saleLog.PXKNum =",printingPXKNum)
    # get the expDate, if a Lot has 2 expDate, select only the 1st
    expDate <- Inventory %>% select(prodCode,Lot,expDate) %>% unique()
    expDate <- expDate[!duplicated(expDate$Lot),]
    Data <- dbGetQuery(conn,query)
    Data <- merge(Data,expDate,all.x=T)
    # calculate total price
    Data$totalPrice <- Data$unit_price*Data$qty
    query <- paste("SELECT DISTINCT customer_info.customer_name
                    FROM pxk_info INNER JOIN customer_info
                    ON pxk_info.customerID = customer_info.customerID
                    WHERE pxk_info.PXKNum =", printingPXKNum)
    printingCustomerName <- dbGetQuery(conn,query)
    printingCustomerName <- printingCustomerName$customer_name[1]
    outputInfo <- dbGetQuery(conn,'select * from outputInfo where Type = "pxkOutput"')
    dbDisconnect(conn)

    # writing customer_name
    customerNameRow <- as.numeric(
      outputInfo$Value[outputInfo$Name=='customerNameRow'])
    customerNameCol <- as.numeric(
      outputInfo$Value[outputInfo$Name=='customerNameCol'])
    writeData(wb,sheet=1,printingCustomerName, startRow=customerNameRow, 
                   startCol=customerNameCol, colNames = F)
    # writing pxkNum
    pxkNumRow <- as.numeric(outputInfo$Value[outputInfo$Name=='pxkNumRow'])
    pxkNumCol <- as.numeric(outputInfo$Value[outputInfo$Name=='pxkNumCol'])
    writeData(wb,sheet=1,printingPXKNum,startRow=pxkNumRow, 
                   startCol=pxkNumCol, colNames = F)
    # print(printingPXKNum)
    # get pxkDataHeaders
    pxkDataHeaders <-  data.frame(matrix(unlist(strsplit(
        outputInfo$Value[outputInfo$Name=='dataHeaders'],',')),nrow=1))
    # print(pxkDataHeaders)
    # rearrange Data and write
    Data <- Data[order(as.numeric(Data$Stt)),]
    dataColumns <- unlist(strsplit(
      outputInfo$Value[outputInfo$Name=='dataToWrite'],','))
    # print(dataColumns)
    pxkData <- Data[,dataColumns]
    
    
    # write both data and headers
    dataStartRow <- as.numeric(
      outputInfo$Value[outputInfo$Name=='dataStartRow'])
    dataStartCol <- as.numeric(
      outputInfo$Value[outputInfo$Name=='dataStartCol'])
    #write headers first
    writeData(wb,sheet=1,pxkDataHeaders, startRow=dataStartRow,
                   startCol=dataStartCol, colNames=F)
    # data is one row below
    writeData(wb,sheet=1,pxkData,startRow=dataStartRow+1,
                   startCol=dataStartCol, colNames=F)
    # save the excel sheet
    saveWorkbook(wb,destPath,overwrite = T)
    
    # open the file
    system(paste0('open ','"',destPath,'"'))
    
    # ------------- completeForm UI refresh ------------------------------------
    # update PXK
    output$pxkSelector <- renderUI({
      newPXK <- get_new_pxk(config_dict)
      selectInput(
        inputId = "pxkSelector",
        label = ui_elem$actual[ui_elem$label=='pxkNum'],
        choices = newPXK
      )
    })
    #refresh customer selector
    output$customerSelector <- renderUI({
      custChoices <- get_cust_list(config_dict)
      selectInput(inputId = 'customer_name',
                  label = ui_elem$actual[ui_elem$label=='customer_name'],
                  choices = custChoices)
    })
    # prodName refresh
    output$prodNameSelector <- renderUI({
      selectInput(inputId = "prodNameSelector",
                  label = ui_elem$actual[ui_elem$label=='prodName'],
                  choices=product_info$Name)
    })
    # amount refresh
    output$amountSelector <- renderUI({
      selectizeInput(inputId = "amountSelector",
                     label = ui_elem$actual[ui_elem$label=='qty'],
                     choices=c(1:100),options = list(create=T))
    })
    #lot refresh
    output$lotSelector <- renderUI({
      prodCodeCurrent <- product_info[product_info$Name==input$prodNameSelector, "prodCode"]
      avaiLot <- get_avail_lot(current_prod_code,config_dict)
      selectizeInput(
        inputId = "lotSelector", label = "Lot",
        choices = unique(avaiLot), options = list(create = TRUE)
      )
    })
    # product_infoPane refresh
    output$product_infoPane <- renderUI({
      product_infoPaneStr <- buildProductInfoPane(Inventory,product_info,Packaging,
                                                 input)
      HTML(product_infoPaneStr)
    })
    # Note refresh
    output$pxkNote <- renderUI({
      textInput(inputId = 'pxkNote',
                label = ui_elem$actual[ui_elem$label=='Note'],
                value = '')
    })
  })
#   
# # ------------------------ UI for the Lookup Tab -------------------------------
  output$lookup_tbl_output <- renderDataTable({
    tableName <- ui_elem$label[
      ui_elem$actual==input$lookupTableSelector]
    # complex tables that cannot run on query (yet)
    if (tableName=='Inventory'){
      lookup_tbl_output <- updateInventory(importLog,saleLog)
      lookup_tbl_output <- merge(lookup_tbl_output,
                                 product_info %>% select(prodCode,Name,mfgCode),
                                 all.x = T) %>%
        select(Name,mfgCode,Lot,expDate,remaining_qty)
    }else{
      # query on simple table
      if (tableName=='product_info'){
        query <- paste("SELECT prodCode,Name,NSX,mfgCode from product_info")
      }
      if (tableName=='importPrice'){
        query <- paste("SELECT product_info.Name, product_info.NSX,
                        product_info.mfgCode, importPrice.importPrice,
                        importPrice.Currency, importPrice.Vendor,
                        importPrice.priceType, importPrice.lastUpdated
                        FROM importPrice INNER JOIN product_info
                        ON importPrice.prodCode = product_info.prodCode")
      }
      if (tableName=='comingList'){
        query <- paste0("SELECT comingList.Name, comingList.mfgCode, 
                        comingList.NSX, comingList.Quantity, comingList.poName,
                        poInfo.Note, ui_elem.actual as Status
                        FROM comingList INNER JOIN poInfo
                        ON poInfo.poName = comingList.poName
                        INNER JOIN poStatusCode ON
                        poInfo.poStatusCode = poStatusCode.poStatusCode
                        INNER JOIN ui_elem ON 
                        poStatusCode.label = ui_elem.label
                        where ui_elem.appLang like '",appLang,"'")
      }
      if (tableName=='poInfo'){
        query <- paste0("SELECT poInfo.poName as POName, poInfo.Note, 
                        ui_elem.actual as Status from poInfo 
                        inner join poStatusCode on 
                        poInfo.poStatusCode = poStatusCode.poStatusCode 
                        inner join ui_elem on 
                        poStatusCode.label = ui_elem.label 
                        where ui_elem.appLang like '",appLang,"' and
                        poInfo.poStatusCode < 9 order by POName asc")
      }
      if (tableName=='saleLog'){
        query <- paste("SELECT product_info.Name, product_info.NSX,
                        product_info.mfgCode, saleLog.Unit,
                        saleLog.unit_price, saleLog.qty,
                        saleLog.Lot, saleLog.PXKNum, customer_info.customer_name
                        FROM saleLog INNER JOIN product_info
                        ON saleLog.prodCode = product_info.prodCode
                        INNER JOIN pxk_info
                        ON saleLog.PXKNum = pxk_info.PXKNum
                        INNER JOIN customer_info
                        ON pxk_info.customerID = customer_info.customerID"
        )
      }
      if (tableName=='importLog'){
        query <- paste("SELECT product_info.Name, product_info.NSX,
                        product_info.mfgCode, importLog.Unit,
                        importLog.Quantity, importLog.POName,
                        importLog.Lot, importLog.expDate, 
                        importLog.deliveryDate
                        FROM importLog INNER JOIN product_info
                        ON importLog.prodCode = product_info.prodCode"
        )
      }
      conn <- db_open(config_dict)
    lookup_tbl_output <- dbGetQuery(conn,query)
    dbDisconnect(conn)
    }
    # lookup_tbl_output <- setnames(lookup_tbl_output,colNameLabel,colNameActual)
    lookup_tbl_output
  })

# --------------------- UI for the Tools tab -----------------------------------
  # addCustomer button action
  observeEvent(input$addCustomer, {
    conn <- db_open(config_dict)
    customer_info <- dbReadTable(conn,"customer_info")
    dbDisconnect(conn)
    currentCustomerID = max(customer_info$customerID)+1
    appendCustomerInfo <- data.frame(
      customerCode = '',
      customer_name = input$addCustomerName,
      customerID = currentCustomerID,
      Email = input$addCustomerEmail,
      Address = '',
      Phone = ''
    )
    customer_info <- rbind(customer_info,appendCustomerInfo)
    customer_info <- customer_info[!duplicated(customer_info$customer_name),]
    conn <- db_open(config_dict)
    dbWriteTable(conn,'customer_info',customer_info,overwrite=T)
    customer_info <- dbReadTable(conn,"customer_info")
    dbDisconnect(conn)
    output$addCustomerSuccess <- renderUI({
      HTML(ui_elem$actual[ui_elem$label=='addSuccess'])
    })
  })
  
  # addPackagingName & Unit
  output$addPackagingName <- renderUI({
    productList <- unique(product_info$Name)
    selectInput(
      inputId = "addPackagingName",
      label = ui_elem$actual[ui_elem$label=='prodName'],
      choices = productList
    )
  })
  output$addPackagingUnit <- renderUI({
    unitList <- unique(Packaging$Unit)
    selectizeInput(
      inputId = "addPackagingUnit",
      label = ui_elem$actual[ui_elem$label=='Unit'],
      choices = unitList,
      options = list(create=T)
    )
  })

  # printReport action
  observeEvent(input$printReport, {
    reportType <- ui_elem$label[ui_elem$actual==input$reportType]
    # print(reportType)
    if (reportType == 'inventoryAuditReport'|
        reportType == 'inventoryOrderReport'){
      # read the form
      orig_file <- config_dict$value[config_dict$name=='report_form_path']
      wb <- loadWorkbook(orig_file)
      
      # read the inventory
      inventoryReport <- updateInventory(importLog,saleLog,moreThanZero=F)
      # set all negative number to 0
      inventoryReport <- inventoryReport[inventoryReport$remaining_qty>0,]
      
      # if this is ordering report, group and sum
      if (reportType == 'inventoryOrderReport'){
        inventoryReport <- inventoryReport %>% group_by(prodCode) %>% 
          summarise(totalremaining_qty = sum(remaining_qty)) %>% ungroup
        # merge with prod_info so that we get zero items as well
        inventoryReport <- merge(inventoryReport,product_info %>% 
                                   select(prod_code,type),all.y=T)
      }
      #recover human-readble info
      inventoryReport <- merge(
        inventoryReport, product_info %>% select(
          prodCode,Name,NSX,mfgCode,warehouseID))
      inventoryReport <- merge(
        inventoryReport,warehouseInfo %>% select(warehouseID,Warehouse))
      # select the appropriate column
      if (reportType == 'inventoryOrderReport'){
        inventoryReport <- inventoryReport %>%
        select(Name,NSX,mfgCode,totalremaining_qty,Warehouse)
      }else{
        inventoryReport <- inventoryReport %>%
          select(Name,NSX,mfgCode,remaining_qty,
                 Lot,expDate,Warehouse)
      }
      
      # write data to destination file then open file
      writeData(wb, 1, inventoryReport, startRow=5, startCol=1)
      dest_file <- config_dict$value[config_dict$name=='report_out_path']
      saveWorkbook(wb,dest_file,overwrite = T)
      system(paste0('open ','"',dest_file,'"'))
    }

  })

    # addPackaging button action
  observeEvent(input$addPackaging, {
    # reload Packaging table first
    conn <- db_open(config_dict)
    Packaging <- dbReadTable(conn,'Packaging')
    dbDisconnect(conn)
    
    # create the dataFrame to be appended
    appendPackaging <- data.frame(
      prodCode = product_info$prodCode[
        product_info$Name==input$addPackagingName],
      Unit = input$addPackagingUnit,
      unitsPerPack = input$addPackagingNum,
      lastUpdated = format(Sys.Date(),'%d%m%y')
    )
    # append and check for duplicates
    Packaging <- rbind(Packaging,appendPackaging)
    if (nrow(Packaging[duplicated(Packaging[,c('Unit','prodCode')]),])>0){
      stop(ui_elem$actual[ui_elem$label=='duplicatedPackaging'])
    }else{ # else writing to database
      conn <- db_open(config_dict)
      dbWriteTable(conn,'Packaging', appendPackaging, row.names=F, append=T)
      dbDisconnect(conn)
      output$addPackagingSuccess <- renderUI({
        HTML(ui_elem$actual[ui_elem$label=='addSuccess'])
      })  
    }
  })
})