# the global script main purpose is to read boot.R
rm(list=ls()) # boot clean-up
options(warn=-1) # supress warnings
app_path <- dirname(getwd()) # app_path is one level up from shiny folder
source(file.path(app_path,'r','boot.R'))
