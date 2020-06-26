# once global.R fiigured ot the app_path, we use boot.R to handle the boot
# ------------------------ load required packages ------------------------------
required_package <- c('shinythemes','DBI','DT', 'shiny', 'shinydashboard', 
                      'scales', 'openxlsx', 'dplyr', 'data.table', 'lubridate',
                      'emayili')
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
func_list <- c('base','refresh_ui',
               'inv_in_tab','inv_in_helper',
               'inv_out_tab','inv_out_helper',
               'report_helper','report_tab',
               'ui_helper','ui_render',
               'update_db_tab')
for (script_to_load in func_list){
  script_path <- file.path(app_path, 'r', paste0(script_to_load,'.R'))
  source(script_path)
}

# localisation information
reload_tbl(config_dict,"localisation") # need this first
company_name <- config_dict$value[config_dict$name=='company_name']
copyright_str <- paste(
  'Copyright (C) 2017-2019, Data built for:', company_name)
app_lang <- config_dict$value[config_dict$name=='app_lang']
# use the configured language
localisation <- localisation[localisation$app_lang==app_lang,]
# create ui_lem
ui_elem <- localisation[localisation$group=='ui_elements',]


# connect to database and read start-up information
reload_tbl(config_dict, c('currency',
  'product_info', "output_info", "import_log","customer_info", "guess_table",
  "packaging", "sale_log", "pxk_info" , "warehouse_info", "payment_type",
  "importlic_data", "tender_detail", "tender_info", "import_price", 
  "vendor_info","product_type","po_info","report_type"))

# customise
report_info <- output_info[output_info$type=='report_output',]

# --------------------- UI Configurations --------------------------------------

# create raw table
#handle report_group first
lu_report_list <- create_lu_report_list(config_dict)
report_group <- unique(report_type$actual)

lu_report_list <- merge(lu_report_list,ui_elem,all.x = T)
lu_report_list <- lu_report_list$actual

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

# hide tabs as per config_dict
hidden_tab <- unlist(strsplit(config_dict$value[config_dict$name=='hidden_tab'],';'))
