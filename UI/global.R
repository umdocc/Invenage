# This file can be placed anywhere, but need the invenageConf.csv to work
rm(list=ls())
# call all required packages
requiredPackagesList <- c('RSQLite','shiny','shinydashboard','ggplot2',
                          'scales','XLConnect')
new.packages <- requiredPackagesList[!(requiredPackagesList %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = 'https://cloud.r-project.org')

lapply(requiredPackagesList, require, character.only = TRUE)

# ------------------------------- functions ------------------------------------
 # detecting operating system
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(as.character(os)))
  
}

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

# get home directory, accounting for windows and various computers 
# so that we can read config
getHomePath <- function(){
  osType <- get_os()
  homePath <- path.expand('~')
  
  # if for some reason we get to Documents in windows, remove it
  if (osType == 'windows' & grepl('Documents',homePath)){
    homePath <- dirname(homePath)
  }
  return(homePath)
}

# --------------------- Configure Basic Information ----------------------------
# attemp to remove Documents folder in windows homePath
homePath <- getHomePath()
configFullPath <- file.path(homePath,'invenageConf.csv')

if (file.exists(configFullPath)){
  configDict <- read.csv(configFullPath, stringsAsFactors = F)
}else{
  stop('invenageConf.csv not found!')
}

# if this is windows environment, check if we have chrome installed and set the
# default browser parameter
osType <- get_os()
if (osType == 'windows'){
  chromePath <- configDict$Value[configDict$Name=='chromePath']
  if (file.exists(chromePath)){
    # set the chrome option
    options(browser = chromePath)
  }else{
    stop('Path to Chrome not found or incorrect!!')
    }
}


companyName <- configDict$Value[configDict$Name=='companyName']
appLang <- configDict$Value[configDict$Name=='appLang']
appPath <- file.path(homePath,
                     configDict$Value[configDict$Name=='appPath'])
dbType <- configDict$Value[configDict$Name=='dbType']

if (dbType == 'SQLite'){
  databasePath <- file.path(homePath,
                            configDict$Value[configDict$Name=='databasePath'])}

# use this to abstract betwwen SQLite and MySQL
dbOpen <- function(dbType,configDict){
    if (dbType == 'SQLite'){
        databasePath <- file.path(homePath,
                          configDict$Value[configDict$Name=='databasePath'])
        sqlite.driver <- dbDriver("SQLite")
        conn <- dbConnect(sqlite.driver, dbname = databasePath)
    return(conn)
  }
}



# connect to database and read start-up information
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

# --------------------- UI Configurations --------------------------------------
# use the configured language
localisation <- localisation[localisation$lang==appLang,]

#Dynamic UI sidebar
allowSalesView <- configDict$Value[configDict$Name=='allowSalesView']=='yes'

# list of tables in lookups
lookupTableList <- 'productInfo'
if (configDict$Value[configDict$Name=='allowImportPriceView']=='yes'){
  lookupTableList <- c(lookupTableList,'importPrice')
}
lookupTableList <- data.frame(label = lookupTableList)
lookupTableList <- merge(lookupTableList,localisation)
lookupTableList <- lookupTableList$actual
# ----------------------------------- Tables -----------------------------------

Inventory <- inventoryBase

# !!! REMOVE THIS when in production: hacks used for testing, 
# do this so I dont have to modify the database everytime
# saleLog$customerCode[grepl('CR-BV',saleLog$customerCode)] <- 'CHORAY'
# saleLog <- saleLog[0,]
# dbWriteTable(conn,'saleLog',saleLog,row.names=F)

# productInfo$oldProdCode <- productInfo$prodCode
# productInfo$NSXShort <- ''
# productInfo$NSXShort[grepl('Cambrid',productInfo$NSX)] <- 'CAM' 
