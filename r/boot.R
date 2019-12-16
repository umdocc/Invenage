# the boot.r file handle all remaining configuration after config_dict
# it will assume the global variable config_dict and all paths has been
# translated

# ------------------------ load required packages ------------------------------
required_package <- c('DT', 'shiny', 'shinydashboard', 'ggplot2', 'scales',
                      'openxlsx', 'dplyr', 'data.table', 'lubridate')
new.packages <- required_package[
  !(required_package %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(
  new.packages, repos = 'https://cloud.r-project.org')
lapply(required_package, require, character.only = TRUE)

# ------------------------ database configuration ------------------------------
db_type <- config_dict$value[config_dict$name=='db_type']
if (db_type == 'SQLite'){
  require(RSQLite)
}
if (db_type == 'MariaDB'){
  require(RMariaDB)
}

# ------------------------- chrome config for windows --------------------------
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

# ------------------------ load remaining scripts ------------------------------
function_paths <- file.path(config_dict$value[config_dict$name=='app_path'], 
                            'r')
base_func_path <- file.path(function_paths,'base_functions.R')
ui_func_path <- file.path(function_paths,'ui_functions.R')
report_func_path <- file.path(function_paths,'report_functions.R')
render_func_path <- file.path(function_paths,'render_functions.R')
source(base_func_path)
source(ui_func_path)
source(report_func_path)
source(render_func_path)


company_name <- config_dict$value[config_dict$name=='company_name']
copyright_str <- paste(
  'Copyright (C) 2017-2019, Data built for:', company_name)
app_lang <- config_dict$value[config_dict$name=='app_lang']

# connect to database and read start-up information
conn <- db_open(config_dict)
report_info <- dbReadTable(conn,"output_info")
product_info <- dbReadTable(conn,"product_info")
localisation <- dbReadTable(conn,"localisation")
import_log <- dbReadTable(conn,"import_log")
customer_info <- dbReadTable(conn,"customer_info")
packaging <- dbReadTable(conn,"packaging")
sale_log <- dbReadTable(conn,"sale_log")
pxk_info <- dbReadTable(conn,"pxk_info")
warehouse_info <- dbReadTable(conn,"warehouse_info")
dbDisconnect(conn)
report_info <- report_info[report_info$type=='report_output',]
# --------------------- UI Configurations --------------------------------------
# use the configured language
localisation <- localisation[localisation$app_lang==app_lang,]
# extract sub-tables from localisation
ui_elem <- localisation[localisation$group=='ui_elements',]
# build the ui_elem_dict

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
# print(home_path)

# variables that should be read from config_dict
date_format <- config_dict$value[config_dict$name=='date_format']
date_range_tbl <- config_dict$value[config_dict$name=='date_range_tbl']

#formatting variables
date_range_tbl_local <- ui_elem$actual[ui_elem$label == date_range_tbl]

date_format_alt <- gsub('%d','dd',date_format)
date_format_alt <- gsub('%m','mm',date_format_alt)
date_format_alt <- gsub('%Y','yyyy',date_format_alt)