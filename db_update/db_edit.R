# db_edit use real sql command to edit the database
# for integrity and ease of use, db editing will be submitted using
# excel request form

# -------------------- sys init from local config ---------------------------
# path
local_config_path <- file.path(path.expand("~/"),"appData","invenage","config.tsv")
local_config <- read.table(local_config_path, header = T)


# source boot helper for simple init
source(file.path(local_config$value[local_config$name=="app_path"], "boot",
                 'boot_helper.R'))

# re-load local config for integrity
local_config <- load_local_config(local_config_path)

# create the connection object
require("DBI","RMariaDB")
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

# ----------------- Main ----------------------------------