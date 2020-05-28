# the boot.r file handle all remaining configuration after config_dict
# it will assume the global variable config_dict and all paths has been
# translated

# ------------------------ load required packages ------------------------------
required_package <- c('shinythemes','DBI','DT', 'shiny', 'shinydashboard', 
                      'scales', 'openxlsx', 'dplyr', 'data.table', 'lubridate')
lapply(required_package, require, character.only = TRUE)

# ---------------------------- paths configuration -----------------------------
# read the configuration data file
print(app_path)
home_path <- path.expand('~')
home_path <- gsub('\\\\','/',home_path) #windows fix
home_path <- gsub('/Documents','',home_path)
config_path <- file.path(home_path,'invenage_data','invenage_conf.csv')

if (file.exists(config_path)){
  config_dict <- read.csv(config_path, stringsAsFactors = F)
}else{
  stop('invenage_conf.csv not found!')
}
# build paths in config_dict
config_dict$value[config_dict$type=='abs'] <- 
  gsub(';','/',config_dict$value[config_dict$type=='abs'])
config_dict$value[config_dict$type=='relative'] <- 
  gsub(';','/',config_dict$value[config_dict$type=='relative'])
config_dict$value[config_dict$type=='relative'] <- 
  file.path(app_path,config_dict$value[config_dict$type=='relative'])

# ------------------------ database configuration ------------------------------
db_type <- config_dict$value[config_dict$name=='db_type']
if (db_type == 'SQLite'){require(RSQLite)}
if (db_type == 'MariaDB'){require(RMariaDB)}

# ------------------------- chrome config for windows --------------------------
os_name <- config_dict$value[config_dict$name=='os_name']
if (all(grepl('windows',os_name))){
  browser_path <- config_dict$value[config_dict$name=='browser_path']
  if (file.exists(browser_path)){
    # set the chrome option
    options(browser = browser_path)
  }else{
    stop('Path to Browser not found or incorrect!!')
  }
}

# ------------------------ load remaining scripts ------------------------------
func_list <- c('base','ui_helper','ui_buttons','report','ui_render')
for (script_to_load in func_list){
  script_path <- file.path(app_path, 'r', paste0(script_to_load,'.R'))
  source(script_path)
}

company_name <- config_dict$value[config_dict$name=='company_name']
copyright_str <- paste(
  'Copyright (C) 2017-2019, Data built for:', company_name)
app_lang <- config_dict$value[config_dict$name=='app_lang']

# connect to database and read start-up information
product_info <- reload_tbl(config_dict,'product_info')
conn <- db_open(config_dict)
report_info <- dbReadTable(conn,"output_info")
localisation <- dbReadTable(conn,"localisation")
import_log <- dbReadTable(conn,"import_log")
customer_info <- dbReadTable(conn,"customer_info")
packaging <- dbReadTable(conn,"packaging")
sale_log <- dbReadTable(conn,"sale_log")
pxk_info <- dbReadTable(conn,"pxk_info")
warehouse_info <- dbReadTable(conn,"warehouse_info")
payment_type <- dbReadTable(conn,"payment_type")
importlic_data <- dbReadTable(conn,"importlic_data")
tender_detail <- dbReadTable(conn,"tender_detail")
tender_info <- dbReadTable(conn,"tender_info")
import_price <- dbReadTable(conn,"import_price")
vendor_info <- dbReadTable(conn,"vendor_info")
product_type <- dbReadTable(conn,"product_type")
guess_table <- dbReadTable(conn,"guess_table")
dbDisconnect(conn)

# customise
report_info <- report_info[report_info$type=='report_output',]


# --------------------- UI Configurations --------------------------------------
# use the configured language
localisation <- localisation[localisation$app_lang==app_lang,]
# create ui_lem
ui_elem <- localisation[localisation$group=='ui_elements',]

# create all tables that rely on ui_elem
product_type <- merge(product_type,ui_elem)

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

# -------------------------- Start-up Data -------------------------------------
col_name_label <- localisation$label[localisation$group=='col_rename']
col_name_actual <- localisation$actual[localisation$group=='col_rename']

# variables that should be read from config_dict
date_format <- config_dict$value[config_dict$name=='date_format']
date_range_tbl <- config_dict$value[config_dict$name=='date_range_tbl']

#formatting variables
date_range_tbl_local <- ui_elem$actual[ui_elem$label == date_range_tbl]

date_format_alt <- gsub('%d','dd',date_format)
date_format_alt <- gsub('%m','mm',date_format_alt)
date_format_alt <- gsub('%Y','yyyy',date_format_alt)

# error_file
error_file <- config_dict$value[config_dict$name=='error_log']
# error_text <- readLines(error_file)

admin_id <- config_dict$value[config_dict$name=='admin_id']
if (length(admin_id)!=1){
  stop('admin_id not set or corrupted!')
}

# temporary fix for null comm_name
conn <- db_open(config_dict)
dbExecute(conn,
          'update product_info set comm_name = name where comm_name is null')
dbDisconnect(conn)
