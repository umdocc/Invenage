# the global script main purpose is to read boot.R
rm(list=ls()) # boot clean-up
options(warn=-1) # supress warnings
app_path <- dirname(getwd()) # app_path is one level up from shiny folder
# print(app_path)
boot_path <- file.path(app_path,'r','boot.R')

if(file.exists(boot_path)){
  source(file.path(app_path,'r','boot.R'))
}else{
  app_path <- '~/Documents/GitHub/Invenage/'
  source(file.path(app_path,'r','boot.R'))
}