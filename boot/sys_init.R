# initilize database connection with just enough information for the
# rest to follow
rm(list=ls()) # boot clean-up
options(warn=-1) # supress warnings

# -------------------- sys init from local config ---------------------------
library("DBI"); library("RMariaDB"); library(dplyr); library("tidyr")
# path
local_config_path <- file.path(path.expand("~/"),"appData","invenage","config.tsv")
local_config <- read.table(local_config_path, header = T)

# source boot helper for simple init
source(file.path(local_config$value[local_config$name=="app_path"], "boot",
                 'boot_helper.R'))

# re-load local config for integrity
local_config <- load_local_config(local_config_path)

# assign global variables
assign("app_path", 
       local_config$value[local_config$name=="app_path"],
       envir=globalenv()
       )

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
assign("config",config,envir=globalenv())