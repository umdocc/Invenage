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
             slr_tab,
             ilr_tab
             ),
  # 
  # # update_db menu
  # navbarMenu(
  #   get_actual('update_db'),
  #   update_customer_tab,
  #   update_product_tab,
  #   update_vendor_invoice_tab,
  #   update_import_price_tab,
  #   update_vendor_tab
  # ),
  # 
  # # tools menu
  # navbarMenu(
  #   get_actual('tools'),
  #   sync_excel_po_tab
  # ),
  # 
  # # service and warranty menu
  # navbarMenu(
  #   get_actual('service_and_warranty'),
  #   tech_service_log_tab,
  #   tech_service_warranty_tab
  # ),
  # 
  # ceo menu
  navbarMenu(title = uielem$ceo,
             ceo_tab
  )
) # end navbarPage