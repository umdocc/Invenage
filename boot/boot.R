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

# create local and db config
config_dict <- load_local_config(local_config_path)
admin_id <- as.integer(
  config_dict$value[config_dict$name=='admin_id'])
db_config <- load_db_config(admin_id)

# bind the two config, sort by source_rank, then remove duplicates
config_dict <- rbind(config_dict,db_config)
config_dict <- config_dict[order(config_dict$source_rank),]
config_dict <- config_dict[!duplicated(config_dict$name),]

# 
# # load external config  script if available
# ext_config_path <- file.path(
#   path.expand("~"),'invenage_data','ext_config.R')
# if(file.exists(ext_config_path)){
#   source(ext_config_path)
# }
# 
# # create a wide format of config_dict for ease of use
# config <- config_dict %>% select(name,value)
# config <- spread(config,name,value)
# 
# # database configuration
# db_type <- config_dict$value[config_dict$name=='db_type']
# if (db_type == 'SQLite'){require(RSQLite)}
# if (db_type == 'MariaDB'){require(RMariaDB)}
# 
# # create uielem table
# ui_elem <- create_ui_elem()
# uielem <- spread(ui_elem %>% select(label,actual),label,actual)
# 
# # create the list of tables included in lu_report, localised name
# reload_tbl(config_dict,'report_type')
# lu_report_list <- create_lu_report_list(config_dict)
# report_group <- unique(report_type$actual)
# lu_report_list <- merge(lu_report_list,ui_elem,all.x = T)
# lu_report_list <- lu_report_list$actual
# 
# # load the list of tabs to hide
# hidden_tab <- split_semi(config$hidden_tab)
# 
# # load the remaining function
# func_list <- list.files(file.path(app_path,'r'),full.names = T, recursive = T)
# # exclude base/boot otherwise it will stuck in a loop
# func_list <- func_list[!grepl('base|boot',func_list)]
# for (script_to_load in func_list){
#   # print(script_to_load)
#   source(script_to_load)
# }
# 
# # localisation information
# company_name <- config$company_name
# copyright_str <- paste(
#   'Copyright (C) 2017-2019, Data built for:', company_name)
# 
# # connect to database and read start-up information
# reload_tbl(config_dict, c('currency','staff_info','staff_activity_log',
#   'product_info', "output_info", "import_log","customer_info", "guess_table",
#   "packaging", "sale_log", "pxk_info" , "warehouse_info", "payment_type",
#   "importlic_data", "tender_detail", "tender_info", "import_price", 
#   "vendor_info","product_type","po_info","vendor_invoice","tender_comm_name"))
# 
# # customise
# report_info <- output_info[output_info$type=='report_output',]
# 
# 
# # -------------------------- Start-up Data -------------------------------------
# 
# # variables that should be read from config_dict
# date_format <- config$date_format
# 
# date_format_alt <- gsub('%d','dd',date_format)
# date_format_alt <- gsub('%m','mm',date_format_alt)
# date_format_alt <- gsub('%Y','yyyy',date_format_alt)
# 
# # error_file
# error_file <- config_dict$value[config_dict$name=='error_log']
# # error_text <- readLines(error_file)
# 
# admin_id <- config_dict$value[config_dict$name=='admin_id']
# if (length(admin_id)!=1){
#   stop('admin_id not set or corrupted!')
# }
# 
# # the list of colnames of pxk when rendered
# pxk_render_colnames <- unlist(
#   strsplit(config_dict$value[config_dict$name=='pxk_render_colnames'],';'))
# allow_edit_pxk_colnum <- as.numeric(unlist(
#   strsplit(config_dict$value[config_dict$name=='allow_edit_pxk_colnum'],';')))
# 
# # ------------------------- chrome config for windows --------------------------
# os_name <- config_dict$value[config_dict$name=='os_name']
# if (all(grepl('windows',os_name))){
#   browser_path <- config_dict$value[config_dict$name=='browser_path']
#   if (file.exists(browser_path)){
#     # set the chrome option
#     options(browser = browser_path)
#   }else{
#     stop('Path to Browser not found or incorrect!!')
#   }
# }
# 
# # ---------------------------- update database ---------------------------------
# update_po_info(config_dict) # update po with new files from local source
# 

