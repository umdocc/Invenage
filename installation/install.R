
# install all required packages
required_package <- c('shinythemes','DBI','DT', 'shiny', 'shinydashboard',
                      'ggplot2', 'scales', 'RMariaDB', 'RSQLite',
                      'openxlsx', 'dplyr', 'data.table', 'lubridate',
                      'shinyalert','emayili')
new.packages <- required_package[
  !(required_package %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(
  new.packages, repos = 'https://cloud.r-project.org')