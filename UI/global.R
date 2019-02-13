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
databasePath <- file.path(path.expand('~'),
                        configDict$Value[configDict$Name=='databasePath'])

# connect to database
sqlite.driver <- dbDriver("SQLite")
conn <- dbConnect(sqlite.driver, dbname = databasePath)
productInfo <- dbReadTable(conn,"productInfo")
localisation <- dbReadTable(conn,"localisation")
inventoryBase <- dbReadTable(conn,"inventoryBase")
customerInfo <- dbReadTable(conn,"customerInfo")
Packaging <- dbReadTable(conn,"Packaging")
saleLog <- dbReadTable(conn,"saleLog")
saleLogBase <- dbReadTable(conn,"saleLogBase")
PXKInfo <- dbReadTable(conn,"PXKInfo")
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
