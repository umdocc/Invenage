#Install location (relative to home folder)
install_location <- 'Desktop'

# install script for initiliasing Invenage
# install required R packages
requiredPackagesList <- c('RSQLite','shiny','shinydashboard','ggplot2',
                          'scales','openxlsx','dplyr','data.table','lubridate')
new.packages <- requiredPackagesList[
  !(requiredPackagesList %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,
                                          repos = 'https://cloud.r-project.org')
lapply(requiredPackagesList, require, character.only = TRUE)

# copying Invenage files
homePath <- path.expand('~')
