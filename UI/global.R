# This file can be placed anywhere, but need the invenageConf.csv to work
library(RSQLite)
library(shiny)
library(shinydashboard)
require(gdata)
require(ggplot2)
require(scales)
# ------------------------------- functions ------------------------------------
# roll back x months from current month, not counting current month,
# return the beginning date of the rolled back month
rollBackDate <- function(rollingMth){
  beginDate <- as.Date(as.character(cut(Sys.Date(), "month")),'%Y-%m-%d')
  backDate <- as.Date(as.character(cut(beginDate - 28*rollingMth,'month')))
  return(backDate)
}

# get currentPXK is a function that use the database
getNewPXK <- function(conn){
  PXKNumList <- dbGetQuery(conn,'select PXKNum from PXKInfo')
  currentPXK <- dbGetQuery(conn,'select PXKNum from PXKInfo where completionCode = 0')
  if (nrow(currentPXK)>0){
    newPXK = currentPXK$PXKNum[1]
  }else{
    currentDate <- strftime(Sys.time(),'%d%m%y')
    i <- 1;newPXKNum <- F
    while (!newPXKNum){
      tmpNum = paste0(strftime(Sys.time(),'%d%m%y'),sprintf("%02d",i))
      # print(tmpNum)
      if (length(PXKNumList[PXKNumList$PXKNum==as.integer(tmpNum),'PXKNum'])==0){
        newPXK <- tmpNum
        newPXKNum <- T
      }else{
        i <- i+1
      }
    }
  }
  return(newPXK)
}


# --------------------- Configure Basic Information ----------------------------
# check the configuration file
if (file.exists(file.path(path.expand('~'),'invenageConf.xlsx'))){
  configDict <- read.xls(file.path(path.expand('~'),'invenageConf.xlsx'),
                         stringsAsFactors = F)
}else{
  stop('invenageConf.xlsx not found!')
}
companyName <- configDict$Value[configDict$Name=='companyName']
appLang <- configDict$Value[configDict$Name=='appLang']
appPath <- file.path(path.expand('~'),
                     configDict$Value[configDict$Name=='appPath'])
dbType <- configDict$Value[configDict$Name=='dbType']

if (dbType == 'SQLite'){
  databasePath <- file.path(path.expand('~'),
                            configDict$Value[configDict$Name=='databasePath'])}

# use this to abstract betwwen SQLite and MySQL
dbOpen <- function(dbType,configDict){
    if (dbType == 'SQLite'){
        databasePath <- file.path(path.expand('~'),
                          configDict$Value[configDict$Name=='databasePath'])
        sqlite.driver <- dbDriver("SQLite")
        conn <- dbConnect(sqlite.driver, dbname = databasePath)
    return(conn)
  }
}



# connect to database
conn <- dbOpen(dbType,configDict)
productInfo <- dbReadTable(conn,"productInfo")
localisation <- dbReadTable(conn,"localisation")
inventoryBase <- dbReadTable(conn,"inventoryBase")
customerInfo <- dbReadTable(conn,"customerInfo")
Packaging <- dbReadTable(conn,"Packaging")
saleLog <- dbReadTable(conn,"saleLog")
saleLogBase <- dbReadTable(conn,"saleLogBase")
PXKInfo <- dbReadTable(conn,"PXKInfo")
warehouseInfo <- dbReadTable(conn,"warehouseInfo")
dbDisconnect(conn)

# use the configured language
localisation <- localisation[localisation$lang==appLang,]

# building the total inventory
Inventory <- inventoryBase

# !!! REMOVE THIS when in production: hacks used for testing, 
# do this so I dont have to modify the database everytime
# saleLog$customerCode[grepl('CR-BV',saleLog$customerCode)] <- 'CHORAY'
# saleLog <- saleLog[0,]
# dbWriteTable(conn,'saleLog',saleLog,row.names=F)

# productInfo$oldProdCode <- productInfo$prodCode
# productInfo$NSXShort <- ''
# productInfo$NSXShort[grepl('Cambrid',productInfo$NSX)] <- 'CAM' 
