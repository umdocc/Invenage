# extract pxk is a tool that extract the information from multiple excel pxk
# and write it back to database
# independent tool use the tool name in appData folder, for this one it will be
# 'extract_pxk'. Expect to see the following files minimum:
# extract_pxk_config.tsv: data file that contains machine specific config for task
# customer_id_patch: patch that read customer_id if we cannot find a match in db

# ------------------------------------ Main ------------------------------------
# load required package
# rm(list=ls())
lapply(c('tidyr','openxlsx', 'dplyr'), require, character.only = TRUE)

# create config
# read and merge configuration files
main_path <- file.path(path.expand("~"),"appData","invenage")
data_path <- file.path(main_path,"extract_pxk")
config_m <- read.table(file.path(main_path,"config.tsv"),
                       header = T, stringsAsFactors = F)
config_t <- read.table(file.path(data_path,"extract_pxk_config.tsv"),
                       header = T, stringsAsFactors = F)
config_dict <- rbind(config_m,config_t)
# source the data_transform functions from app_path so that we can 
app_path <- config_m$value[config_m$name=='app_path']
source(file.path(app_path,'base','data_transform.R'))
config <- convert_config(config_dict)

#more paths and source from config
source(file.path(config$app_path,"tools","extract_pxk_helper.R"))
pxk_path <- file.path(data_path,"pxk") # location for pxk used for recovery

append_pxk_info <- extract_pxk_info(pxk_path, config)
# if there is any missing customer_id we write out a report
missing_c_id <- append_pxk_info[append_pxk_info$customer_id==0,]

extract_pxk_data <- function(config,pxk_path){
  i <- 1
  pxk_data <- read.xlsx(file.path(pxk_path,file_list[i]), 
                        startRow = config$pxk_start_row)
  names(pxk_data) <- unlist(strsplit(config$pxk_rname, split=";"))
  # filter items with null qty
  pxk_data <- pxk_data[!is.na(pxk_data$qty),]
  pxk_data <- merge(pxk_data,product_info %>%select(ref_smn,prod_code),
                    all.x=T)
  pxk_data$qty <- as.numeric(sub(" .*", "", pxk_data$qty_unit))
  pxk_data$unit <- trimws(sub(".*? ", "", pxk_data$qty_unit))
  pxk_data$unit_price <- sub("/.*", "", pxk_data$unit_price_unit)
  pxk_data$unit_price <- as.numeric(sub(",", "", pxk_data$unit_price))
}

