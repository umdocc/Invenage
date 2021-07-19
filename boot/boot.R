# once global.R figured ot the app_path, we use boot.R to handle the boot
# ----------------------------------- init -------------------------------------

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
if (grepl('windows',config$os_name)){
  browser_path <- config$browser_path
  if (file.exists(browser_path)){
    # set the chrome option
    options(browser = browser_path)
  }else{
    stop('Path to Browser not found or incorrect!!')
  }
}
