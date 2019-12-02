# the global script main purpose is to read the configuration file
rm(list=ls()) # boot clean-up
options(warn=-1) # supress warnings

# --------------------- Configure Basic Information ----------------------------
# read the configuration data file
home_path <- path.expand('~')
home_path <- gsub('\\\\','/',home_path) #windows fix
home_path <- gsub('/Documents','',home_path)
config_path <- file.path(home_path,'invenage_data','invenage_conf.csv')

if (file.exists(config_path)){
  config_dict <- read.csv(config_path, stringsAsFactors = F)
}else{
  stop('invenage_conf.csv not found!')
}

# ----------------------- build paths in config_dict ---------------------------
config_dict$value <- gsub('/;',';',config_dict$value)
config_dict$value[config_dict$type=='abs'] <- 
  gsub(';','/',config_dict$value[config_dict$type=='abs'])
app_path <- config_dict$value[config_dict$name=='app_path']
config_dict$value[config_dict$type=='relative'] <- 
  gsub(';','/',config_dict$value[config_dict$type=='relative'])
config_dict$value[config_dict$type=='relative'] <- 
  file.path(app_path,config_dict$value[config_dict$type=='relative'])

# load the boot.r, only work after config_dict paths are built
source(file.path(app_path,'r','boot.R'))

