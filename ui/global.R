rm(list=ls()) # boot clean-up
options(warn=-1) # supress warnings

# install all required packages
required_package <- c('shinythemes','DBI','DT', 'shiny', 'shinydashboard',
                      'ggplot2', 'scales','shinyalert','tidyr','openxlsx', 
                      'dplyr', 'data.table', 'lubridate','RMariaDB')
new.packages <- required_package[
  !(required_package %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(
  new.packages, repos = 'https://cloud.r-project.org')

# first only load the sys package
lapply(required_package, require, character.only = TRUE)

# get app_path
app_path <- dirname(getwd()) # app_path is one level up from shiny folder

# point to local file as app_path will be invalid during dev
if(!file.exists(file.path(app_path,"ui","global.R"))){
  home_path <- path.expand("~")
  if(Sys.info()[["sysname"]]=="Windows"){
    app_path <- file.path(home_path,"GitHub","Invenage")
  }else{
    app_path <- file.path(home_path,"Docum
                          
                          
                          
                          
                          
                          
                          
                          ents","GitHub","Invenage")
  }
}

#get boot_path
boot_path <- file.path(app_path,'boot')

# path for reading the local config data
home_path <- path.expand('~')
local_config_path <- file.path(home_path,'appData','invenage','config.tsv')

# ------------------------------- boot -----------------------------------------

# source boot helper
source(file.path(boot_path,'boot_helper.R'))

# load local config
local_config <- load_local_config(local_config_path)

# create the connection object
conn <- dbConnect(
  drv = RMariaDB::MariaDB(),
  username = local_config$value[local_config$name=='sql_usr'],
  password = local_config$value[local_config$name=='sql_pswd'],
  host = local_config$value[local_config$name=='sql_host'],
  port = 3306, dbname = local_config$value[
    local_config$name=='sql_db_name'])

#load the remaining config
db_config <- load_db_config(local_config)
config <- create_config(local_config,db_config)

# create uielem
uielem <- create_uielem(config)

# load the list of tabs to hide
hidden_tab <- split_semi(config$hidden_tab)

# load the remaining function
func_list <- list.files(file.path(app_path),full.names = T, recursive = T,
                        pattern = ".R")
func_list <- func_list[!grepl('boot|ui',func_list)] # exclude
sapply(func_list, source)


# ------------------------- chrome config for windows --------------------------
# if (grepl('windows',config$os_name)){
#   browser_path <- config$browser_path
#   if (file.exists(browser_path)){
#     # set the chrome option
#     options(browser = browser_path)
#   }else{
#     stop('Path to Browser not found or incorrect!!')
#   }
# }

# -------------------------- compatibility fixes -------------------------------
# fix  packaging_str=NA
db_exec_query(
  "update product_info set packaging_str='' where packaging_str is null")

# -------------------------- load global tables --------------------------------
gbl_load_tbl(c("sale_log", "payment_type", "packaging", "product_info",
               "import_log", "customer_info", "tender_info", "warehouse_info",
               "vendor_info","import_price","product_type", "tender_detail"))

prod_choices <- db_get_prodlist(config$prod_search_str)
gbl_update_inventory()
ordering_unit <- get_ordering_unit(packaging)
gbl_write_var("error_free",T)

# create global data
create_global_data()

# checking database integrity
db_integrity_check()