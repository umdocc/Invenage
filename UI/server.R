# Invenage server.R
source("global.R",local = F)

shinyServer(function(input, output,session) {

# ------------------- UI Elements for Xuat Kho tab -----------------------------
  # Info line, use htmlOutput for more controls
  output$thongTinSP <- renderUI({
    mfgCode <- productInfo[productInfo$Name==input$prodName, "mfgCode"]
    prodCode <- productInfo[productInfo$Name==input$prodName &
                               productInfo$NSX==input$nsxSelector, "prodCode"]
    totalAvail <- Inventory[Inventory$prodCode == prodCode &
                              Inventory$Lot == input$lotSelector, 'SLKho']
    ExpDate <- Inventory[Inventory$prodCode == prodCode &
                           Inventory$Lot == input$lotSelector, 'ExpDate']
    HTML(paste("REF: ",mfgCode,'<br/>',
          localisation$actual[localisation$label=='prodCode'],':',prodCode,
          '<br/>',
          localisation$actual[localisation$label=='ExpDate'],':',ExpDate,
          '<br/>',
          localisation$actual[localisation$label=='totalAvail'],':',totalAvail)
    )
  })

  output$nsxSelector <- renderUI({
    nsxList <- productInfo[productInfo$Name == input$prodName,"NSX"]
    selectInput(
      inputId = "nsxSelector",
      label = "NSX",
      choices = unique(nsxList)
    )
  })
  output$lotSelector <- renderUI({
    prodCodeCurrent <- productInfo[productInfo$Name==input$prodName &
                               productInfo$NSX==input$nsxSelector, "prodCode"]
    avaiLot <- Inventory[Inventory$prodCode==prodCodeCurrent,'Lot']
    selectizeInput(
      inputId = "lotSelector", label = "Lot",
      choices = unique(avaiLot), options = list(create = TRUE)
    )
  })

  output$unitSelector <- renderUI({
    prodCodeCurrent <- productInfo[productInfo$Name==input$prodName &
                            productInfo$NSX==input$nsxSelector, "prodCode"]
    unitList <- Packaging[Packaging$prodCode == prodCodeCurrent,"Unit"]
    unitList <- unique(unitList)
    unitList <- unitList[unitList!='pack']
    selectizeInput(
      inputId = "unitSelector",
      label = localisation$actual[localisation$label=='Unit'],
      choices = unitList,
      options = list(create=T)
    )
  })

  output$pxkSelector <- renderUI({
    
    conn <- dbOpen(dbType, configDict)
    PXKInfo <- dbReadTable(conn,"PXKInfo")
    dbDisconnect(conn)
    
    currentPXK <- PXKInfo[PXKInfo$completionCode==0,'PXKNum']
    if (length(currentPXK)>0){
      currentPXK = unique(currentPXK)
    }else{
      currentDate <- strftime(Sys.time(),'%d%m%y')
      i <- 1;newPXKNum <- F
      while (!newPXKNum){
        tmpNum = paste0(strftime(Sys.time(),'%d%m%y'),sprintf("%02d",i))
        if (length(PXKInfo[PXKInfo$PXKNum==as.integer(tmpNum),'PXKNum'])==0){
          currentPXK <- tmpNum
          newPXKNum <- T
        }else{
          i <- i+1
        }
      }
    }
    selectInput(
      inputId = "pxkSelector",
      label = localisation$actual[localisation$label=='pxkNum'],
      choices = currentPXK
    )
  })

  output$warehouseSelector <- renderUI({
    warehouseList <- warehouseInfo$Warehouse
    selectInput(
      inputId = "warehouseSelector",
      label = localisation$actual[localisation$label=='Warehouse'],
      choices = warehouseList,
      selected = 'MDS' # dynamic selection later on
    )
  })

# ------------------- UI for the salesView tab ---------------------------------
  output$testText <- renderText({
    customerCode <- customerInfo$customerCode[customerInfo$customerName==
                                                input$saleViewCustomerName]
    paste('Mã KH:',customerCode)
  })
  output$saleViewPlot <- renderPlot({
    tmp <- saleLog[saleLog$customerCode == customerInfo$customerCode[
      customerInfo$customerName == input$saleViewCustomerName]]
    tmp$saleMonth <- as.Date(strptime(tmp$invoiceDate,
                                      format = '%Y-%m-%d %H:%M:%S'))
    currentMth <- as.Date(as.character(cut(Sys.Date(), "month")),'%Y-%m-%d')
    backDate <- rollBackDate(input$rollingMth)
    tmp <- tmp[tmp$saleMonth >= backDate & tmp$saleMonth < currentMth,]
    tmp$saleMonth <- as.Date(as.character(cut(tmp$saleMonth,'month')))  
    # graph by month:
    ggplot(data = tmp,
           aes(saleMonth, unitAmt)) +
      stat_summary(fun.y = sum, # adds up all observations for the month
                   geom = "bar") + # or "line"
      scale_x_date(
        labels = date_format("%Y-%m"),
        breaks = "1 month") # custom x-axis labels
  })

# ------------------------ Nhap linh kien action button ------------------------
  # events that happen once clicked
  observeEvent(input$inventoryOut, {

    # Write the event to transaction table to keep track of the transaction
    # connect to database, also re-read PXKInfo
    conn <- dbOpen(dbType, configDict)
    saleLog <- dbReadTable(conn,"saleLog")
    PXKInfo <- dbReadTable(conn,"PXKInfo")
    dbDisconnect(conn)

    appendPXKInfo <- data.frame(
      # if this PXK is not in the database yet, create new with completionCode 0
      if (length(PXKInfo[PXKInfo$PXKNum==as.integer(input$pxkSelector),'PXKNum'])==0){
        appendPXKInfo <- data.frame(
          PXKNum = input$pxkSelector,
          saleDate = as.integer(format(Sys.Date(),'%d%m%y')),
          customerCode = customerInfo[
            customerInfo$customerName == input$customerName,'customerCode'],
          PXKType = 'I',
          Warehouse = input$warehouseSelector,
          completionCode = 0
        )
        
        # write this to database, then reload PXKInfo
        conn <- dbOpen(dbType,configDict)
        dbWriteTable(conn,'PXKInfo',appendPXKInfo,append=T)
        PXKInfo <- dbReadTable(conn,"PXKInfo")
        dbDisconnect(conn)
        # set currentStt also
        currentStt <- 1
        
      }else{ #otherwise, read the info from the saleLog
        conn <- dbOpen(dbType,configDict)
        currentStt <- dbGetQuery(conn, "select max(Stt) from saleLog 
                                        where PXKNum = (
                                        select PXKNum from PXKInfo 
                                        where completionCode = 0)")[1,1]
        dbDisconnect(conn)
        
        # if there is a result, increase by 1, otherwise set to 1
        if (is.na(currentStt)){
          currentStt <- 1
        }else{
          currentStt <- currentStt+1
        }
        
      }
    )

    # append the data from input
    appendSaleLog <- data.frame(
      Stt = currentStt,
      prodCode = unique(productInfo[productInfo$Name==input$prodName &
                                      productInfo$NSX==input$nsxSelector, "prodCode"]),
      Unit = input$unitSelector,
      Amount = input$Amount,
      Lot = input$lotSelector,
      customerCode = customerInfo[
        customerInfo$customerName == input$customerName,'customerCode'],
      PXKNum = input$pxkSelector,
      Note = ''
    )
    # print(appendSaleLog)    
    # writing saleLog to database
    conn <- dbOpen(dbType,configDict)
    dbWriteTable(conn,'saleLog',appendSaleLog,append=T)
    dbDisconnect(conn)

    output$currentPXK <- renderTable({
      query <- paste("select * from saleLog where PXKNum =",
                     input$pxkSelector)
      conn <- dbOpen(dbType,configDict)
      outTable <- dbGetQuery(conn,query)
      dbDisconnect(conn)
      outTable
    })
  })
  observeEvent(input$delLastEntry,{
    conn <- dbOpen(dbType,configDict)
    query <- paste("delete from saleLog where PXKNum =",input$pxkSelector,
                   "and Stt = (select max(Stt) from saleLog where PXKNum =",
                   input$pxkSelector,")")
    res <- dbSendStatement(conn,query)
    dbClearResult(res)
    dbDisconnect(conn)
    
    # render current PXK table
    output$currentPXK <- renderTable({
      query <- paste("select * from saleLog where PXKNum =",
                     input$pxkSelector)
      conn <- dbOpen(dbType,configDict)
      outTable <- dbGetQuery(conn,query)
      dbDisconnect(conn)
      outTable
    })
  })
  
  # when completeForm button is pressed, change PXK status to 1 and write the excel form
  observeEvent(input$completeForm,{
    conn <- dbOpen(dbType,configDict)
    query <- paste("update PXKInfo set completionCode = 1 
                   where PXKNum =",input$pxkSelector)
    res <- dbSendStatement(conn,query)
    dbClearResult(res)
    newPXK <- getNewPXK(conn)
    dbDisconnect(conn)
    
    # getting data to write to excel
    printingPXKNum <- input$pxkSelector
    printingWarehouse <- input$warehouseSelector

    # create new PXK file
    origPath <- file.path(homePath,
              configDict$Value[configDict$Name=='formsPath'],
              "PXKform.xlsx")
    destPath <- file.path(homePath,
                          configDict$Value[configDict$Name=='PXKPath'],
                          paste0("PXK.",input$pxkSelector,".xlsx"))
    file.copy(origPath,destPath)
    wb <- loadWorkbook(destPath, create=TRUE)
    setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
    
    conn <- dbOpen(dbType,configDict)
    query <- paste("SELECT saleLog.Stt, productInfo.Name,saleLog.Unit,
                      saleLog.Amount,saleLog.Lot,saleLog.customerCode
                    FROM saleLog INNER JOIN productInfo
                    ON saleLog.prodCode = productInfo.prodCode
                    WHERE saleLog.PXKNum =",printingPXKNum)
    Data <- dbGetQuery(conn,query) 
    dbDisconnect(conn)
    writeWorksheet(wb,Data,"PXK",startRow=8,startCol=1,header=F)
    saveWorkbook(wb)
    # open the file
    system(paste0('open ','"',destPath,'"'))
    
    
    output$pxkSelector <- renderUI({
      # render the UI
      selectInput(
        inputId = "pxkSelector",
        label = localisation$actual[localisation$label=='pxkNum'],
        choices = newPXK
      )
    })
  })

})