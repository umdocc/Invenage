# once global.R fiigured ot the app_path, we use boot.R to handle the boot
# ----------------------------------- init -------------------------------------
required_package <- c('shinythemes','DBI','DT', 'shiny', 'shinydashboard', 
                      'scales', 'openxlsx', 'dplyr', 'data.table', 'lubridate',
                      'emayili','shinyalert')
lapply(required_package, require, character.only = TRUE)

# source the base script, which provide functions for boot
source(file.path(app_path, 'r', paste0('base.R')))

# create config_dict
config_dict <- create_config_dict(app_path,'home')
config_dict$source <- 'local'

# read config_dict from db and merge with local
if (config_dict$value[config_dict$name=='config_from_db']=='TRUE'){
  admin_id <- config_dict$value[config_dict$name=='admin_id']
  conn <- db_open(config_dict)
  db_config <- dbReadTable(conn,'config_dict')
  db_config <- db_config[db_config$admin_id==admin_id|db_config$admin_id==0,]
  dbDisconnect(conn)
  
  # if there is duplicated items in db_config, use the one with admin_id
  db_config$value[db_config$name==db_config$name[duplicated(db_config$name)]] <-
    db_config$value[db_config$name==db_config$name[duplicated(db_config$name)]&
                      db_config$admin_id==admin_id][1]
  db_config <- db_config[!duplicated(db_config$name),]
  
  #remove admin_id and finalise
  db_config$admin_id <- NULL
  db_config$source <- 'db'
  db_config <- build_config_dict_path(db_config)
  # by rbind these two data frame and remove duplicates
  # any config in db will be overwritten by local config
  config_dict <- rbind(config_dict,db_config)
  config_dict <- config_dict[!duplicated(config_dict$name),]
}



# database configuration
db_type <- config_dict$value[config_dict$name=='db_type']
if (db_type == 'SQLite'){require(RSQLite)}
if (db_type == 'MariaDB'){require(RMariaDB)}

# load basic tables: ui_elem
ui_elem <- create_ui_elem()

# create the list of tables included in lu_report, localised name
reload_tbl(config_dict,'report_type')
lu_report_list <- create_lu_report_list(config_dict)
report_group <- unique(report_type$actual)
lu_report_list <- merge(lu_report_list,ui_elem,all.x = T)
lu_report_list <- lu_report_list$actual

# load the list of tabs to hide
hidden_tab <- unlist(
  strsplit(config_dict$value[config_dict$name=='hidden_tab'],';'))

# load the remaining function
func_list <- list.files(file.path(app_path,'r'),full.names = T, recursive = T)
# exclude base/boot otherwise it will stuck in a loop
func_list <- func_list[!grepl('base|boot',func_list)]
for (script_to_load in func_list){
  # print(script_to_load)
  source(script_to_load)
}

# localisation information
company_name <- config_dict$value[config_dict$name=='company_name']
copyright_str <- paste(
  'Copyright (C) 2017-2019, Data built for:', company_name)

# connect to database and read start-up information
reload_tbl(config_dict, c('currency','staff_info','staff_activity_log',
  'product_info', "output_info", "import_log","customer_info", "guess_table",
  "packaging", "sale_log", "pxk_info" , "warehouse_info", "payment_type",
  "importlic_data", "tender_detail", "tender_info", "import_price", 
  "vendor_info","product_type","po_info","vendor_invoice","tender_comm_name"))

# customise
report_info <- output_info[output_info$type=='report_output',]


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


# the list of colnames of pxk when rendered
pxk_render_colnames <- unlist(
  strsplit(config_dict$value[config_dict$name=='pxk_render_colnames'],';'))
allow_edit_pxk_colnum <- as.numeric(unlist(
  strsplit(config_dict$value[config_dict$name=='allow_edit_pxk_colnum'],';')))

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