# Invenage server.R
source("global.R",local = F)
require(dplyr)
shinyServer(function(input, output,session) {
# # ------------------- UI Elements for Xuat Kho tab -----------------------------
#   # Info line, use htmlOutput for more controls
  output$productInfoPane <- renderUI({
    productInfoPaneStr <- buildProductInfoPane(Inventory,productInfo,Packaging,
                                               input)
    HTML(productInfoPaneStr)
  })
  
#   # selector UIs
  output$customerSelector <- renderUI({
    conn <- dbOpen(configDict)
    custChoices <- getCustomerSelection(conn)
    dbDisconnect(conn)
    selectInput(inputId = 'customerName',
                 label = localisation$Actual[localisation$Label=='customerName'],
                 choices = custChoices)
  })
  output$prodNameSelector <- renderUI({
    selectInput(inputId = "prodNameSelector",
                label = localisation$Actual[localisation$Label=='prodName'],
                choices=productInfo$Name)
  })
  output$amountSelector <- renderUI({
  selectizeInput(inputId = "amountSelector",
                 label = localisation$Actual[localisation$Label=='Amount'],
                 choices=c(1:100),options = list(create=T))
  })
  
  output$lotSelector <- renderUI({
    prodCodeCurrent <- productInfo[productInfo$Name==input$prodNameSelector, "prodCode"]
    avaiLot <- getAvailableLot(prodCodeCurrent,Inventory,sortType='fifo')
    selectizeInput(
      inputId = "lotSelector", label = "Lot",
      choices = unique(avaiLot), options = list(create = TRUE)
    )
  })
  
  output$unitSelector <- renderUI({
    currentProdCode<- productInfo[productInfo$Name==input$prodNameSelector,
                                   "prodCode"]
    unitList <- Packaging[Packaging$prodCode == currentProdCode,"Unit"]
    unitList <- unique(unitList)
    unitList <- unitList[unitList!='pack']
    selectizeInput(
      inputId = "unitSelector",
      label = localisation$Actual[localisation$Label=='Unit'],
      choices = unitList,
      options = list(create=T)
    )
  })
  
  output$pxkSelector <- renderUI({
    # get current PXK
    conn <- dbOpen(configDict)
    currentPXK <- getCurrentPXK(conn)
    dbDisconnect(conn)

    selectizeInput(
      inputId = "pxkSelector",
      label = localisation$Actual[localisation$Label=='pxkNum'],
      choices = currentPXK,
      options = list(create = T)
    )
  })
  output$warehouseSelector <- renderUI({
    warehouseList <- warehouseInfo$Warehouse
    selectInput(
      inputId = "warehouseSelector",
      label = localisation$Actual[localisation$Label=='Warehouse'],
      choices = warehouseList,
      selected = 'MDS' # dynamic selection later on
    )
  })
  
  output$pxkNote <- renderUI({
    textInput(inputId = 'pxkNote',
              label = localisation$Actual[localisation$Label=='Note'],
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
    # connect to database, also re-read PXKInfo
    conn <- dbOpen(configDict)
    saleLog <- dbReadTable(conn,"saleLog")
    PXKInfo <- dbReadTable(conn,"PXKInfo")
    dbDisconnect(conn)


    # if this PXK is not in the database yet, create new with completionCode 0
    # print(nrow(PXKInfo[PXKInfo$PXKNum==input$pxkSelector,])==0)
    if (nrow(PXKInfo[PXKInfo$PXKNum==input$pxkSelector,])==0){
      
      appendPXKInfo <- data.frame(
        PXKNum = input$pxkSelector,
        saleDate = format(Sys.Date(),'%d%m%y'),
        customerID = customerInfo[
          customerInfo$customerName==input$customerName,'customerID'],
        PXKType = 'I',
        Warehouse = 'MDS',
        completionCode = 0
      )
      # print(appendPXKInfo)
      # write this to database, then reload PXKInfo
      conn <- dbOpen(configDict)
      dbWriteTable(conn,'PXKInfo',appendPXKInfo,append=T)
      PXKInfo <- dbReadTable(conn,"PXKInfo")
      dbDisconnect(conn)
      # set currentStt also
      currentStt <- 1
      #otherwise, read the info from the saleLog
    }else{
      conn <- dbOpen(configDict)
      currentStt <- dbGetQuery(conn, "select max(Stt) from saleLog
                               where PXKNum = (
                               select PXKNum from PXKInfo
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
      prodCode = unique(productInfo[productInfo$Name==input$prodNameSelector,
                                    "prodCode"]),
      Unit = input$unitSelector,
      unitPrice = as.integer(input$unitPrice),
      Amount = input$amountSelector,
      Lot = input$lotSelector,
      PXKNum = input$pxkSelector,
      Note = input$pxkNote,
      invoiceTypeCode = 1 # default to 1
    )

    # print(appendSaleLog)

    # writing saleLog to database
    conn <- dbOpen(configDict)
    dbWriteTable(conn,'saleLog',appendSaleLog,append=T)
    saleLog <- dbReadTable(conn,'saleLog')
    dbDisconnect(conn)
    
    # rebuild the Inventory table
    Inventory <- updateInventory(importLog,saleLog)
    # ------------------- inventoryOut UI refresh ------------------------------
    # prodName refresh
    output$prodNameSelector <- renderUI({
      selectInput(inputId = "prodNameSelector",
                  label = localisation$Actual[localisation$Label=='prodName'],
                  choices=productInfo$Name)
    })
    # amount refresh
    output$amountSelector <- renderUI({
      selectizeInput(inputId = "amountSelector",
                     label = localisation$Actual[localisation$Label=='Amount'],
                     choices=c(1:100),options = list(create=T))
    })
    #lot refresh
    output$lotSelector <- renderUI({
      prodCodeCurrent <- productInfo[productInfo$Name==input$prodNameSelector, "prodCode"]
      avaiLot <- getAvailableLot(prodCodeCurrent,Inventory,sortType='fifo')
      selectizeInput(
        inputId = "lotSelector", label = "Lot",
        choices = unique(avaiLot), options = list(create = TRUE)
      )
    })
    # productInfoPane refresh
    output$productInfoPane <- renderUI({
      productInfoPaneStr <- buildProductInfoPane(Inventory,productInfo,Packaging,
                                                 input)
      HTML(productInfoPaneStr)
    })
    # Note refresh
    output$pxkNote <- renderUI({
      textInput(inputId = 'pxkNote',
                label = localisation$Actual[localisation$Label=='Note'],
                value = '')
    })
    #current PXK refresh
    output$currentPXKTable <- renderTable({
      renderPXK(input$pxkSelector)
    })
    
  })

  observeEvent(input$delLastEntry,{
    conn <- dbOpen(configDict)
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
    # refresh the productInfoPane
    output$productInfoPane <- renderUI({
      productInfoPaneStr <- buildProductInfoPane(Inventory,productInfo,
                                                 Packaging,input)
      HTML(productInfoPaneStr)
    })
    
    
    # render current PXK table
    output$currentPXKTable <- renderTable({
      query <- paste("select * from saleLog where PXKNum =",
                     input$pxkSelector)
      conn <- dbOpen(configDict)
      outTable <- dbGetQuery(conn,query)
      dbDisconnect(conn)
      outTable
    })
  })
  
  observeEvent(input$completeForm,{
    conn <- dbOpen(configDict)
    query <- paste0("update PXKInfo set completionCode = 1
                    where PXKNum = '",input$pxkSelector,"'")
    # print(query)
    res <- dbSendStatement(conn,query)
    dbClearResult(res)
    newPXK <- getNewPXK(conn)
    dbDisconnect(conn)

    # getting data to write to excel
    printingPXKNum <- input$pxkSelector
    # printingWarehouse <- input$warehouseSelector

    # create new PXK file
    printingWarehouse <- 'MDS'
    origPath <- configDict$value[configDict$name=='pxk_form']
    destPath <- file.path(configDict$value[configDict$name=='pxk_out_path'],
                          paste0(printingWarehouse,".PXK.",
                                 printingPXKNum,".xlsx"))
    file.copy(origPath,destPath)
    print(origPath)
    wb <- loadWorkbook(origPath)
    # setStyleAction(wb,XLC$"STYLE_ACTION.NONE")

    conn <- dbOpen(configDict)
    query <- paste("SELECT saleLog.Stt, productInfo.Name, productInfo.mfgCode,
                   saleLog.Unit, saleLog.unitPrice,
                   saleLog.Amount,saleLog.Lot
                   FROM   saleLog INNER JOIN productInfo
                   ON     saleLog.prodCode = productInfo.prodCode
                   WHERE  saleLog.PXKNum =",printingPXKNum)
    # get the expDate, if a Lot has 2 expDate, select only the 1st
    expDate <- Inventory %>% select(prodCode,Lot,expDate) %>% unique()
    expDate <- expDate[!duplicated(expDate$Lot),]
    Data <- dbGetQuery(conn,query)
    Data <- merge(Data,expDate,all.x=T)
    # calculate total price
    Data$totalPrice <- Data$unitPrice*Data$Amount
    query <- paste("SELECT DISTINCT customerInfo.customerName
                    FROM PXKInfo INNER JOIN customerInfo
                    ON PXKInfo.customerID = customerInfo.customerID
                    WHERE PXKInfo.PXKNum =", printingPXKNum)
    printingCustomerName <- dbGetQuery(conn,query)
    printingCustomerName <- printingCustomerName$customerName[1]
    outputInfo <- dbGetQuery(conn,'select * from outputInfo where Type = "pxkOutput"')
    dbDisconnect(conn)

    # writing customerName
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
      conn <- dbOpen(configDict)
      newPXK <- getNewPXK(conn)
      dbDisconnect(conn)
      selectInput(
        inputId = "pxkSelector",
        label = localisation$Actual[localisation$Label=='pxkNum'],
        choices = newPXK
      )
    })
    #refresh customer selector
    output$customerSelector <- renderUI({
      conn <- dbOpen(configDict)
      custChoices <- getCustomerSelection(conn)
      dbDisconnect(conn)
      selectInput(inputId = 'customerName',
                  label = localisation$Actual[localisation$Label=='customerName'],
                  choices = custChoices)
    })
    # prodName refresh
    output$prodNameSelector <- renderUI({
      selectInput(inputId = "prodNameSelector",
                  label = localisation$Actual[localisation$Label=='prodName'],
                  choices=productInfo$Name)
    })
    # amount refresh
    output$amountSelector <- renderUI({
      selectizeInput(inputId = "amountSelector",
                     label = localisation$Actual[localisation$Label=='Amount'],
                     choices=c(1:100),options = list(create=T))
    })
    #lot refresh
    output$lotSelector <- renderUI({
      prodCodeCurrent <- productInfo[productInfo$Name==input$prodNameSelector, "prodCode"]
      avaiLot <- getAvailableLot(prodCodeCurrent,Inventory,sortType='fifo')
      selectizeInput(
        inputId = "lotSelector", label = "Lot",
        choices = unique(avaiLot), options = list(create = TRUE)
      )
    })
    # productInfoPane refresh
    output$productInfoPane <- renderUI({
      productInfoPaneStr <- buildProductInfoPane(Inventory,productInfo,Packaging,
                                                 input)
      HTML(productInfoPaneStr)
    })
    # Note refresh
    output$pxkNote <- renderUI({
      textInput(inputId = 'pxkNote',
                label = localisation$Actual[localisation$Label=='Note'],
                value = '')
    })
  })
#   
# # ------------------------ UI for the Lookup Tab -------------------------------
  output$lookupTableOutput <- renderDataTable({
    tableName <- localisation$Label[
      localisation$Actual==input$lookupTableSelector]
    # complex tables that cannot run on query (yet)
    if (tableName=='Inventory'){
      lookupTableOutput <- updateInventory(importLog,saleLog)
      lookupTableOutput <- merge(lookupTableOutput,
                                 productInfo %>% select(prodCode,Name,mfgCode),
                                 all.x = T) %>%
        select(Name,mfgCode,Lot,expDate,remainingQty)
    }else{
      # query on simple table
      if (tableName=='productInfo'){
        query <- paste("SELECT prodCode,Name,NSX,mfgCode from productInfo")
      }
      if (tableName=='importPrice'){
        query <- paste("SELECT productInfo.Name, productInfo.NSX,
                        productInfo.mfgCode, importPrice.importPrice,
                        importPrice.Currency, importPrice.Vendor,
                        importPrice.priceType, importPrice.lastUpdated
                        FROM importPrice INNER JOIN productInfo
                        ON importPrice.prodCode = productInfo.prodCode")
      }
      if (tableName=='comingList'){
        query <- paste0("SELECT comingList.Name, comingList.mfgCode, 
                        comingList.NSX, comingList.Quantity, comingList.poName,
                        poInfo.Note, localisation.Actual as Status
                        FROM comingList INNER JOIN poInfo
                        ON poInfo.poName = comingList.poName
                        INNER JOIN poStatusCode ON
                        poInfo.poStatusCode = poStatusCode.poStatusCode
                        INNER JOIN localisation ON 
                        poStatusCode.Label = localisation.Label
                        where localisation.appLang like '",appLang,"'")
      }
      if (tableName=='poInfo'){
        query <- paste0("SELECT poInfo.poName as POName, poInfo.Note, 
                        localisation.Actual as Status from poInfo 
                        inner join poStatusCode on 
                        poInfo.poStatusCode = poStatusCode.poStatusCode 
                        inner join localisation on 
                        poStatusCode.Label = localisation.Label 
                        where localisation.appLang like '",appLang,"' and
                        poInfo.poStatusCode < 9 order by POName asc")
      }
      if (tableName=='saleLog'){
        query <- paste("SELECT productInfo.Name, productInfo.NSX,
                        productInfo.mfgCode, saleLog.Unit,
                        saleLog.unitPrice, saleLog.Amount,
                        saleLog.Lot, saleLog.PXKNum, customerInfo.customerName
                        FROM saleLog INNER JOIN productInfo
                        ON saleLog.prodCode = productInfo.prodCode
                        INNER JOIN PXKInfo
                        ON saleLog.PXKNum = PXKInfo.PXKNum
                        INNER JOIN customerInfo
                        ON PXKInfo.customerID = customerInfo.customerID"
        )
      }
      if (tableName=='importLog'){
        query <- paste("SELECT productInfo.Name, productInfo.NSX,
                        productInfo.mfgCode, importLog.Unit,
                        importLog.Quantity, importLog.POName,
                        importLog.Lot, importLog.expDate, 
                        importLog.deliveryDate
                        FROM importLog INNER JOIN productInfo
                        ON importLog.prodCode = productInfo.prodCode"
        )
      }
      conn <- dbOpen(configDict)
    lookupTableOutput <- dbGetQuery(conn,query)
    dbDisconnect(conn)
    }
    # lookupTableOutput <- setnames(lookupTableOutput,colNameLabel,colNameActual)
    lookupTableOutput
  })

# --------------------- UI for the Tools tab -----------------------------------
  # addCustomer button action
  observeEvent(input$addCustomer, {
    conn <- dbOpen(configDict)
    customerInfo <- dbReadTable(conn,"customerInfo")
    dbDisconnect(conn)
    currentCustomerID = max(customerInfo$customerID)+1
    appendCustomerInfo <- data.frame(
      customerCode = '',
      customerName = input$addCustomerName,
      customerID = currentCustomerID,
      Email = input$addCustomerEmail,
      Address = '',
      Phone = ''
    )
    customerInfo <- rbind(customerInfo,appendCustomerInfo)
    customerInfo <- customerInfo[!duplicated(customerInfo$customerName),]
    conn <- dbOpen(configDict)
    dbWriteTable(conn,'customerInfo',customerInfo,overwrite=T)
    customerInfo <- dbReadTable(conn,"customerInfo")
    dbDisconnect(conn)
    output$addCustomerSuccess <- renderUI({
      HTML(localisation$Actual[localisation$Label=='addSuccess'])
    })
  })
  
  # addPackagingName & Unit
  output$addPackagingName <- renderUI({
    productList <- unique(productInfo$Name)
    selectInput(
      inputId = "addPackagingName",
      label = localisation$Actual[localisation$Label=='prodName'],
      choices = productList
    )
  })
  output$addPackagingUnit <- renderUI({
    unitList <- unique(Packaging$Unit)
    selectizeInput(
      inputId = "addPackagingUnit",
      label = localisation$Actual[localisation$Label=='Unit'],
      choices = unitList,
      options = list(create=T)
    )
  })

  # printReport action
  observeEvent(input$printReport, {
    reportType <- localisation$Label[localisation$Actual==input$reportType]
    # print(reportType)
    if (reportType == 'inventoryAuditReport'|
        reportType == 'inventoryOrderReport'){
      # read the form
      orig_file <- configDict$value[configDict$name=='report_form_path']
      wb <- loadWorkbook(orig_file)
      
      # read the inventory
      inventoryReport <- updateInventory(importLog,saleLog,moreThanZero=F)
      # set all negative number to 0
      inventoryReport <- inventoryReport[inventoryReport$remainingQty>0,]
      
      # if this is ordering report, group and sum
      if (reportType == 'inventoryOrderReport'){
        inventoryReport <- inventoryReport %>% group_by(prodCode) %>% 
          summarise(totalRemainingQty = sum(remainingQty)) %>% ungroup
      }
      #recover human-readble info
      inventoryReport <- merge(
        inventoryReport, productInfo %>% select(
          prodCode,Name,NSX,mfgCode,warehouseID))
      inventoryReport <- merge(
        inventoryReport,warehouseInfo %>% select(warehouseID,Warehouse))
      # select the appropriate column
      if (reportType == 'inventoryOrderReport'){
        inventoryReport <- inventoryReport %>%
        select(Name,NSX,mfgCode,totalRemainingQty,Warehouse)
      }else{
        inventoryReport <- inventoryReport %>%
          select(Name,NSX,mfgCode,remainingQty,
                 Lot,expDate,Warehouse)
      }
      
      # write data to destination file then open file
      writeData(wb, 1, inventoryReport, startRow=5, startCol=1)
      dest_file <- configDict$value[configDict$name=='report_out_path']
      saveWorkbook(wb,dest_file,overwrite = T)
      system(paste0('open ','"',dest_file,'"'))
    }

  })

    # addPackaging button action
  observeEvent(input$addPackaging, {
    # reload Packaging table first
    conn <- dbOpen(configDict)
    Packaging <- dbReadTable(conn,'Packaging')
    dbDisconnect(conn)
    
    # create the dataFrame to be appended
    appendPackaging <- data.frame(
      prodCode = productInfo$prodCode[
        productInfo$Name==input$addPackagingName],
      Unit = input$addPackagingUnit,
      unitsPerPack = input$addPackagingNum,
      lastUpdated = format(Sys.Date(),'%d%m%y')
    )
    # append and check for duplicates
    Packaging <- rbind(Packaging,appendPackaging)
    if (nrow(Packaging[duplicated(Packaging[,c('Unit','prodCode')]),])>0){
      stop(localisation$Actual[localisation$Label=='duplicatedPackaging'])
    }else{ # else writing to database
      conn <- dbOpen(configDict)
      dbWriteTable(conn,'Packaging', appendPackaging, row.names=F, append=T)
      dbDisconnect(conn)
      output$addPackagingSuccess <- renderUI({
        HTML(localisation$Actual[localisation$Label=='addSuccess'])
      })  
    }
  })
})