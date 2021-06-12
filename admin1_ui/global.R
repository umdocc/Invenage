# the purpose of global is to figure out app_path so that we can load boot.R
rm(list=ls()) # boot clean-up
options(warn=-1) # supress warnings
app_path <- dirname(getwd()) # app_path is one level up from shiny folder
# print(app_path)
boot_path <- file.path(app_path,'r','core','boot.R')

# point to local file as app_path will be invalid during dev
if(!file.exists(boot_path)){
  app_path <- '~/Documents/GitHub/Invenage/'
}

# path for reading the local config data
home_path <- path.expand('~')
local_config_path <- file.path(home_path,'invenage','config_v02.csv')


# load boot
source(file.path(app_path,'r','core','boot.R'))