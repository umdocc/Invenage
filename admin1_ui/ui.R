## Invenage ui.R ##
library(shiny); library(shinythemes); library(shinyalert)
require(DT)
navbarPage(
  theme = shinytheme("united"), title = company_name, id = 'main',
  # actual ui object construction for each tab is located in <tabname>_tab.R file
  inv_out_tab,
  inv_in_tab,
  lu_report_tab,
  pxk_man_tab,
  update_db_tab, # end update_db tab
  hr_log_tab
) # end navbarPage