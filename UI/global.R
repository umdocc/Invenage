# This file can be placed anywhere, but need the invenageConf.csv to work
library(RSQLite)
library(shiny)
library(shinydashboard)
# --------------------- Configure Basic Information ----------------------------
# check the configuration file
if (file.exists(file.path(path.expand('~'),'invenageConf.csv'))){
  configDict <- read.csv(file.path(path.expand('~'),'invenageConf.csv'),
                         stringsAsFactors = F)
}else{
  stop('softanageConf.csv not found!')
}
companyName <- configDict$Value[configDict$Name=='companyName']
appLang <- configDict$Value[configDict$Name=='appLang']
appPath <- file.path(path.expand('~'),
                     configDict$Value[configDict$Name=='appPath'])
coreDBPath <- file.path(path.expand('~'),
                            configDict$Value[configDict$Name=='coreDBPath'])
invViewPath <- file.path(path.expand('~'),
                            configDict$Value[configDict$Name=='invViewPath'])

# connect to invView database
sqlite.driver <- dbDriver("SQLite")
conn <- dbConnect(sqlite.driver, dbname = invViewPath)
productInfo <- dbReadTable(conn,"productInfo")
localisation <- dbReadTable(conn,"localisation")
Inventory <- dbReadTable(conn,"Inventory")
dbDisconnect(conn)

# use the configured language
localisation <- localisation[localisation$lang==appLang,]


