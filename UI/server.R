# Invenage server.R
source("global.R",local = F)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output,session) {
  
  # Info line
  output$thongTinSP <- renderText({
    mfgCode <- productInfo[productInfo$Name==input$prodName, "mfgCode"]
    prodCode <- productInfo[productInfo$Name==input$prodName &
                               productInfos$NSX==input$nsxSelector, "prodCode"]
    # totalAvail <- Inventory[Inventory$prodCode == prodCode]
    paste("REF: ",mfgCode,prodCode)
  })
  
  output$nsxSelector <- renderUI({
    nsxList <- productInfos[productInfos$Name == input$prodName,"NSX"]
    # available1 <- parts[parts$MachineCode == machine_code, c("PartName","TTCode","PartCode")]
    # available1$NameWithCode <- paste(available1$PartName,"[",available1$TTCode,"]")
    # available1 <- available1$NameWithCode
    selectInput(
      inputId = "nsxSelector",
      label = "NSX",
      choices = unique(nsxList)
    )
  })
  output$lotSelector <- renderUI({
    prodCodeCurrent <- productInfos[productInfos$Name==input$prodName & 
                               productInfos$NSX==input$nsxSelector, "prodCode"]
    avaiLot <- Inventory[Inventory$prodCode==prodCodeCurrent,'Lot']
    selectInput(
      inputId = "lotSelector",
      label = "Lot",
      choices = unique(avaiLot)
    )
  })
  
  # # ------------------------ Nhap linh kien action button -------------------------------
  # # events that happen once clicked
  # observeEvent(input$inventoryOut, {
  #   
  #   #Write the event to transaction table to keep track of the transaction
  #   # connect to database
  #   sqlite.driver <- dbDriver("SQLite")
  #   conn <- dbConnect(sqlite.driver, dbname = coreDBPath)
  #   currentPXK <- dbReadTable(conn,"currentPXK")
  #   dbDisconnect(conn)
  #   # current_tid <- last_tid+1
  #   # tenlinhkien <- gsub(" \\[.*$","",input$linhkien)
  #   # transaction <- data.frame(
  #   #   TID = current_tid,
  #   #   Date = input$date,
  #   #   NguoiXuat = input$NguoiXuat,
  #   #   LinhKien = tenlinhkien,
  #   #   MachineCode = as.character(machines[machines$Manufacturer == input$mfg &
  #   #                                         machines$MachineName == input$tenmay,"Code"]),
  #   #   SerialNo = input$serialNo,  
  #   #   SoLuong = input$number,
  #   #   Destination = input$khachhang,
  #   #   GhiChu = input$ghichu,
  #   #   PartCode <- parts$PartCode[parts$PartName==tenlinhkien &
  #   #                                parts$MachineCode==as.character(
  #   #                                  machines[machines$Manufacturer == input$mfg &
  #   #                                             machines$MachineName == input$tenmay,"Code"])]
  #   # )
  #   # write.table(transaction, "Data/transactions.txt", sep="\t",append = T,
  #   #             col.names  = F,row.names = F)
  #   # transactions <<- read.delim2("Data/transactions.txt",stringsAsFactors = F)
  #   
  #   # update the parts table with new value
  #   parts$Remain[parts$PartCode==transaction$PartCode] <- 
  #     parts$Remain[parts$PartCode==transaction$PartCode] - as.numeric(as.character(
  #       transaction$SoLuong))
  #   write.table(parts,file="Data/Parts.txt",sep="\t",
  #               row.names = FALSE,fileEncoding = "utf-8")
  #   
  #   #reload the table
  #   parts <<- read.delim2("Data/parts.txt",stringsAsFactors = F)
  #   
  #   js_string <- 'alert("Hoàn Tất Xuất");'
  #   session$sendCustomMessage(type='jsCode', list(value = js_string))
  #   
  #   #render last transaction, this only happen when clicked
  #   output$text1 <- renderText({ 
  #     last_transaction <- read.delim2("Data/transactions.txt",stringsAsFactors = F)
  #     last_ks <- last_transaction[nrow(last_transaction),"NguoiXuat"]
  #     last_lk <- last_transaction[nrow(last_transaction),"LinhKien"]
  #     last_num <- last_transaction[nrow(last_transaction),"SoLuong"]
  #     last_date <- last_transaction[nrow(last_transaction),"Date"]
  #     paste(last_ks,"xuất",last_num,last_lk,"ngày",last_date)
  #   })
  #   
  # })
  
  # # ------------------------ Undo button ------------------------------------
  # # events that happen once clicked on undo
  # observeEvent(input$undo1, {
  #   
  #   #In case of undo we need to read the entire table and remove the last row
  #   latest_transaction <- read.delim2("Data/transactions.txt",stringsAsFactors = F)
  #   if (nrow(latest_transaction)>1){
  #     
  #     #extract the last transaction and update the part table
  #     last_transaction <- latest_transaction[nrow(latest_transaction),]
  #     parts$Remain[parts$PartCode==last_transaction$PartCode] <- 
  #       parts$Remain[parts$PartCode==last_transaction$PartCode] + as.numeric(as.character(
  #         last_transaction$SoLuong))
  #     write.table(parts,file="Data/parts.txt",sep="\t",row.names = FALSE,fileEncoding = "utf-8")
  #     parts <<- read.delim2("Data/parts.txt",stringsAsFactors = F)
  #     
  #     #update the transaction table
  #     latest_transaction <- latest_transaction[1:nrow(latest_transaction)-1,]
  #     write.table(latest_transaction, "Data/transactions.txt", sep="\t",append = F,
  #                 col.names  = T,row.names = F)
  #     transactions <<- read.delim2("Data/transactions.txt",stringsAsFactors = F)
  #     
  #   }
  #   #render last transaction, this only happen when clicked
  #   output$text1 <- renderText({
  #     last_ks <- latest_transaction[nrow(latest_transaction),"NguoiXuat"]
  #     last_lk <- latest_transaction[nrow(latest_transaction),"LinhKien"]
  #     last_num <- latest_transaction[nrow(latest_transaction),"SoLuong"]
  #     last_date <- latest_transaction[nrow(latest_transaction),"Date"]
  #     paste(last_ks,"xuất",last_num,last_lk,"ngày",last_date)
  #   })
  #   
  #   js_string <- 'alert("Hoàn Tất Xoá Dữ Liệu");'
  #   session$sendCustomMessage(type='jsCode', list(value = js_string))
  #   
  # })
  
  # ---------------------- Cong Cu: Add Customers ---------------------------------------
  # events that happen once clicked on addCustomer
  # observeEvent(input$addCustomer, {
  #   customer <- rbind(customer,input$new_customer)
  #   customer <- data.frame(Name=customer[order(customer),])
  #   write.table(customer,file="Data/customers.txt",sep="\t",append=F,col.names = F,row.names = F)
  #   js_string <- 'alert("Đã Thêm Khách Hàng! Vui lòng khởi động lại app");'
  #   session$sendCustomMessage(type='jsCode', list(value = js_string))
  # })
  
})