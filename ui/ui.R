## Invenage ui.R ##
library(shiny); library(shinythemes); library(shinyalert)
require(DT)
navbarPage(
  
  theme = shinytheme(config$app_theme), title = config$app_title, id = 'main',
  
  # sale menu
  navbarMenu(title = uielem$inventory_out,
             cdn_tab
  ),
  
  # import
  navbarMenu(title = uielem$add_import_item,
             aii_tab,
             sep_tab
  ),
  
  # reports menu
  navbarMenu(title = uielem$report,
             pir_tab,
             ilr_tab,
             slr_tab
             ),
  
  #update_db menu
  navbarMenu(title = uielem$update_db,
             upi_tab,
             uvi_tab,
             uci_tab
  ),

  # sys info
  sysinfo_tab
) # end navbarPage