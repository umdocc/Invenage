# rm(list=ls()) # boot clean-up
required_package <- c('RSQLite','shiny','shinydashboard','ggplot2',
                          'scales','openxlsx','dplyr','data.table','lubridate')
new.packages <- required_package[
  !(required_package %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,
                                          repos = 'https://cloud.r-project.org')
lapply(required_package, require, character.only = TRUE)

# --------------------- Configure Basic Information ----------------------------
# read the configuration data file
home_path <- path.expand('~')
home_path <- gsub('\\\\','/',home_path) #windows fix
home_path <- gsub('/Documents','',home_path)

config_path <- file.path(home_path,'invenage_data','invenage_conf.csv')

if (file.exists(config_path)){
  config_dict <- read.csv(config_path, stringsAsFactors = F)
}else{
  stop('invenage_conf.csv not found!')
}
# build the path
config_dict$value <- gsub('/;',';',config_dict$value)
config_dict$value[config_dict$type=='abs'] <- 
  gsub(';','/',config_dict$value[config_dict$type=='abs'])
app_path <- config_dict$value[config_dict$name=='app_path']
config_dict$value[config_dict$type=='relative'] <- 
  gsub(';','/',config_dict$value[config_dict$type=='relative'])
config_dict$value[config_dict$type=='relative'] <- 
  file.path(app_path,config_dict$value[config_dict$type=='relative'])

# this will only work after config_dict
os_name <- config_dict$value[config_dict$name=='os_name']
if (all(grepl('windows',os_name))){
  chrome_path <- config_dict$value[config_dict$name=='chrome_path']
  if (file.exists(chrome_path)){
    # set the chrome option
    options(browser = chrome_path)
  }else{
    stop('Path to Chrome not found or incorrect!!')
  }
}

# source the ui_functions script
ui_func_path <- file.path(
  config_dict$value[config_dict$name=='app_path'],
  'r','ui_functions.R')
source(ui_func_path)
print('loading UI functions ok')

company_name <- config_dict$value[config_dict$name=='company_name']
copyright_str <- paste(
  'Copyright (C) 2017-2019, Data built for:', company_name)
app_lang <- config_dict$value[config_dict$name=='app_lang']

# connect to database and read start-up information
conn <- db_open(config_dict)
product_info <- dbReadTable(conn,"product_info")
localisation <- dbReadTable(conn,"localisation")
import_log <- dbReadTable(conn,"import_log")
customer_info <- dbReadTable(conn,"customer_info")
packaging <- dbReadTable(conn,"packaging")
sale_log <- dbReadTable(conn,"sale_log")
pxk_info <- dbReadTable(conn,"pxk_info")
warehouse_info <- dbReadTable(conn,"warehouse_info")
dbDisconnect(conn)

# --------------------- UI Configurations --------------------------------------
# use the configured language
localisation <- localisation[localisation$app_lang==app_lang,]
# extract sub-tables from localisation
ui_elem <- localisation[localisation$group=='ui_elements',]

# list of tables in lookups
lu_tbl_list <- unlist(strsplit(
  config_dict$value[config_dict$name=='lookup_tbl_list'],';'))

lu_tbl_list <- data.frame(label = lu_tbl_list)
lu_tbl_list <- merge(lu_tbl_list,ui_elem,all.x = T)
lu_tbl_list <- lu_tbl_list$actual

# get the report list
report_list_label <- config_dict$value[config_dict$name=='report_list_label']
report_list_label <- data.frame(label = unlist(strsplit(report_list_label,
                                                        ';')))
report_list <- merge(report_list_label,ui_elem)
# report_list <- report_list$actual

# -------------------------- Start-up Data -------------------------------------
col_name_label <- localisation$label[localisation$group=='col_rename']
col_name_actual <- localisation$actual[localisation$group=='col_rename']
# print(home_path)