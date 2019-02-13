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
    selectInput(
      inputId = "lotSelector",
      label = "Lot",
      choices = unique(avaiLot)
    )
  })

  output$unitSelector <- renderUI({
    prodCodeCurrent <- productInfo[productInfo$Name==input$prodName &
                            productInfo$NSX==input$nsxSelector, "prodCode"]
    unitList <- Packaging[Packaging$prodCode == prodCodeCurrent,"Unit"]
    unitList <- unique(unitList)
    unitList <- unitList[unitList!='pack']
    selectInput(
      inputId = "unitSelector",
      label = localisation$actual[localisation$label=='Unit'],
      choices = unitList
    )
  })

  output$pxkSelector <- renderUI({
    sqlite.driver <- dbDriver("SQLite")
    conn <- dbConnect(sqlite.driver, dbname = databasePath)
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
    # connect to database
    sqlite.driver <- dbDriver("SQLite")
    conn <- dbConnect(sqlite.driver, dbname = databasePath)
    saleLog <- dbReadTable(conn,"saleLog")
    dbDisconnect(conn)

    # append the data from input
    appendSaleLog <- data.frame(
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
    
    appendPXKInfo <- data.frame(
      # if this PXK is not in the database yet, create new with completionCode 0
      if (length(PXKInfo[PXKInfo$PXKNum==as.integer(input$pxkSelector),'PXKNum'])==0){
        appendPXKInfo <- data.frame(
          PXKNum = input$pxkSelector,
          saleDate = as.integer(format(Sys.Date(),'%d%m%y')),
          customerCode = customerInfo[
            customerInfo$customerName == input$customerName,'customerCode'],
          PXKType = 'I',
          completionCode = 0
        )
      }
    )

    # writing to database
    sqlite.driver <- dbDriver("SQLite")
    conn <- dbConnect(sqlite.driver, dbname = databasePath)
    dbWriteTable(conn,'saleLog',appendSaleLog,append=T)
    saleLog <- dbReadTable(conn,"saleLog")
    if (length(PXKInfo[PXKInfo$PXKNum==as.integer(input$pxkSelector),'PXKNum'])==0){
      dbWriteTable(conn,'PXKInfo',appendPXKInfo,append=T)
    }
    PXKInfo <- dbReadTable(conn,"PXKInfo")
    dbDisconnect(conn)

    output$currentPXK <- renderTable({
      currentPXK <- PXKInfo[PXKInfo$completionCode==0,'PXKNum']
      if (length(currentPXK)>0){
        saleLog[saleLog$PXKNum==as.integer(currentPXK)]
      }else{
        ''
      }

    })
  }) 
})