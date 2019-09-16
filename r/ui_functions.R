# All functions shared by the UIs
# call all required packages
# ------------------------------- Base functions -------------------------------
# roll back x months from current month, not counting current month,
# return the beginning date of the rolled back month
roll_back_date <- function(rolling_mth){
  beginDate <- as.Date(as.character(cut(Sys.Date(), "month")),'%Y-%m-%d')
  backDate <- as.Date(as.character(cut(beginDate - 28*rolling_mth,'month')))
  return(backDate)
}

# get currentPXK is a function that use the database connection object conn
getNewPXK <- function(conn){
  PXKNumList <- dbGetQuery(conn,'select PXKNum from PXKInfo')
  currentPXK <- dbGetQuery(conn,
                    'select PXKNum from PXKInfo where completionCode = 0')
  if (nrow(currentPXK)>0){
    newPXK = currentPXK$PXKNum[1]
  }else{
    currentDate <- strftime(Sys.time(),'%d%m%y')
    i <- 1;newPXKNum <- F
    while (!newPXKNum){
      tmpNum <- as.numeric(paste0(strftime(Sys.time(),'%d%m%y'),
                                  sprintf("%02d",i)))
      # print(tmpNum)
      if (length(PXKNumList[PXKNumList$PXKNum==tmpNum,'PXKNum'])==0){
        newPXK <- tmpNum
        newPXKNum <- T
      }else{
        i <- i+1
      }
    }
  }
  return(newPXK)
}

# the db_open create the appropriate connection for Invenage
db_open <- function(config_dict){
  db_type <- config_dict$value[config_dict$name=='db_type']
  if (db_type == 'SQLite'){
    database_path <- config_dict$value[config_dict$name=='db_file']
    sqlite.driver <- dbDriver("SQLite")
    conn <- dbConnect(sqlite.driver, dbname = database_path)
    return(conn)
  }
}

# convertToPack is a critical function
convertToPack <- function(inputDF,Packaging,stringSL,packString){
  inputDF <- merge(
    inputDF,Packaging %>% select(prodCode,Unit,unitsPerPack),all.x=T)
  # check integrity
  if(nrow(inputDF[is.na(inputDF$unitsPerPack),])>0){
    print(inputDF[is.na(inputDF$unitsPerPack),])
    stop('inputDF contains unrecognised packaging')
  }
  inputDF[[packString]] <- inputDF[[stringSL]]/inputDF$unitsPerPack
  # clean up
  inputDF$Unit <- 'pack'
  inputDF[[stringSL]] <- NULL
  # inputDF$unitsPerPack <- NULL
  return(inputDF)
}

# function to rebuild the Inventory table from import_log and sale_log
# if moreThanZero ==T, it will only return items with positive stock
update_inventory <- function(import_log,sale_log, pos_item=TRUE){
  tmp <- import_log %>% select(prodCode,Unit,Quantity,Lot,expDate)
  tmp <- convertToPack(tmp,Packaging,'Quantity','importQty')
  tmp <- tmp %>% group_by(prodCode,Unit,Lot) %>% 
    summarise(totalImportQty = sum(importQty)) %>% ungroup()
  tmp2 <- sale_log %>% select(prodCode,Unit,Amount,Lot)
  tmp2 <- convertToPack(tmp2,Packaging,'Amount','saleQty')
  tmp2 <- tmp2 %>% group_by(prodCode,Unit,Lot) %>% 
    summarise(totalSaleQty = sum(saleQty)) %>% ungroup()
  totalInventory <- merge(tmp,tmp2,all=T,by=c('prodCode','Unit','Lot'))
  totalInventory$totalSaleQty[is.na(totalInventory$totalSaleQty)] <- 0
  totalInventory$totalImportQty[is.na(totalInventory$totalImportQty)] <- 0
  totalInventory$remainingQty <- totalInventory$totalImportQty - 
    totalInventory$totalSaleQty
  
  # keep only the available items
  if (moreThanZero){
    threshold <- 0.01
    totalInventory <- totalInventory[totalInventory$remainingQty>threshold,] %>% 
    distinct()
  }
  # recover the expDate
  expDateData <- import_log[!duplicated(import_log[c('prodCode','Lot')]),] %>% 
    select(prodCode,Lot,expDate) %>% distinct()
  
  # merge, distinct and remove NA
  totalInventory <- merge(totalInventory,expDateData,all.x=T) %>% distinct()
  totalInventory <- totalInventory[!is.na(totalInventory$prodCode),]
  
  # calculate the intExpDate, which is the expDate in standard format
  totalInventory$expDate <- gsub('/','-',totalInventory$expDate)
  totalInventory$expDate <- gsub(' .*$','',totalInventory$expDate)
  totalInventory$intExpDate <- parse_date_time(
    totalInventory$expDate,c('%Y-%m','%m-%Y','%d-%m-%Y','%Y-%m-%d'))
  
  return(totalInventory)
}

# the getAvailableLot function get a list of available Lot, it returns a vector
# if sortType ='fifo', the earliest expDate will be on top
getAvailableLot <- function(selectedProdCode,Invetory,sortType){
  if (sortType == 'fifo'){
    availableLot <- Inventory[Inventory$prodCode==selectedProdCode,]
    availableLot <- availableLot[order(availableLot$intExpDate,
                                       na.last = F, # put NA lot first
                                       decreasing = F),] #lowest expDate first
  }
  availableLot <- availableLot$Lot
  return(availableLot)
}

# function to rebuild the productInfo HTML string
buildProductInfoPane <- function(Inventory,productInfo,Packaging,input){
  currentMfgCode <- productInfo[
    productInfo$Name==input$prodNameSelector, "mfgCode"]
  currentProdCode <- productInfo[
    productInfo$Name==input$prodNameSelector, "prodCode"]
  currentNSX <- productInfo[productInfo$Name==input$prodNameSelector, "NSX"]
  totalAvail <- Inventory[Inventory$prodCode == currentProdCode &
                            Inventory$Lot == input$lotSelector, 'remainingQty']
  currentExpDate <- Inventory[Inventory$prodCode == currentProdCode &
                                Inventory$Lot == input$lotSelector, 'expDate']
  renderedPackaging <- Packaging[
    Packaging$prodCode == currentProdCode & 
      Packaging$Unit == input$unitSelector,]
  renderedPackaging <- paste0(renderedPackaging$unitsPerPack[1],
                              renderedPackaging$Unit[1],'/pack')
  return(paste("REF: ",currentMfgCode,'<br/>',
               localisation$Actual[localisation$Label=='prodCode'],':',
               currentProdCode, '<br/>',
               localisation$Actual[localisation$Label=='NSX'],':',
               currentNSX, '<br/>',
               localisation$Actual[localisation$Label=='expDate'],':',
               currentExpDate, '<br/>',
               localisation$Actual[localisation$Label=='totalAvail'],':',
               totalAvail, '<br/>',
               localisation$Actual[localisation$Label=='renderedPackaging'],
               ':',renderedPackaging)
  )
}

# function to select customer using the database to look at PXK
getCustomerSelection <- function(conn){
  PXKInfo <- dbReadTable(conn,"PXKInfo")
  customerInfo <- dbReadTable(conn,"customerInfo")
  currentPXK <- PXKInfo[PXKInfo$completionCode==0,'PXKNum']
  # if currentPXK has completion code then we force customerName
  if (length(currentPXK)>0){
    currentCustID <- PXKInfo$customerID[PXKInfo$PXKNum==currentPXK]
    custChoices <- customerInfo$customerName[
      customerInfo$customerID==currentCustID]
  }else{
    custChoices <- customerInfo$customerName
  }
  return(custChoices)
}

# function to render current PXK as an integer
getCurrentPXK <- function(conn){
  PXKInfo <- dbReadTable(conn,"PXKInfo")
  currentPXK <- PXKInfo[PXKInfo$completionCode==0,'PXKNum']
  if (length(currentPXK)>0){
    currentPXK = as.integer(unique(currentPXK))
  }else{
    currentDate <- strftime(Sys.time(),'%d%m%y')
    i <- 1;newPXKNum <- F
    while (!newPXKNum){
      # for a given day, increase last 2 digit until we cannot find a match
      tmpNum = as.integer(
        paste0(strftime(Sys.time(),'%d%m%y'),sprintf("%02d",i)))
      if (length(PXKInfo[PXKInfo$PXKNum==tmpNum,'PXKNum'])==0){
        currentPXK <- tmpNum
        newPXKNum <- T
      }else{
        i <- i+1
      }
    }
  }
  return(currentPXK)
}

# render raw pxk into readable format
renderPXK <- function(currentPXK){
  query <- paste("select sale_log.Stt, productInfo.Name, sale_log.Unit, 
                sale_log.unitPrice,sale_log.Amount, sale_log.Lot,
                sale_log.PXKNum, sale_log.Note 
                from sale_log inner join productInfo
                 on sale_log.prodCode = productInfo.prodCode 
                 where sale_log.PXKNum =",
                 currentPXK)
  conn <- db_open(config_dict)
  outTable <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  return(outTable)
}

# function to build estimated import cost from import_log
getEstImportCost <- function(import_log, algorithm='weighted_average'){
  if (algorithm=='weighted_average'){
    import_log <- convertToPack(import_log,Packaging,stringSL='Quantity',
                               packString = 'packQty')
    import_log$packImportCost <- 
      import_log$actualUnitImportCost*import_log$unitsPerPack
    import_log$totalImportCost <- 
      import_log$packImportCost*import_log$packQty
    tmp <- import_log %>% group_by(prodCode,Lot) %>%
      summarise(totalPack = sum(packQty),
                sumImportCost = sum(totalImportCost)) %>%
      ungroup
    tmp$avePackImportCost <- tmp$sumImportCost/tmp$totalPack
    tmp <- tmp %>% select(prodCode,Lot,avePackImportCost)
    return(tmp)
  }
}

buildCompletePath <- function(pathString,sep=';'){
  pathString <-unlist(strsplit(pathString,split = ';'))
  for (i in c(1:length(pathString))){
    if (i==1){
      fullPath <- pathString[i]
    }else{
      fullPath <- file.path(fullPath,pathString[i])
    }
  }
  return(fullPath)
}