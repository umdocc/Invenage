## Invenage ui.R ##
library(shiny); library(shinythemes); library(shinyalert)
require(DT)
navbarPage(
  theme = shinytheme(config$app_theme), title = config$app_title, id = 'main',

  # sale menu
  navbarMenu(title = uielem$inventory_out,
    cdn_tab
  )
  # 
  # # import menu
  # iti_tab,
  # 
  # # reports menu
  # navbarMenu(title = uielem$reports,
  #            pir_tab,
  #            slr_tab,
  #            ilr_tab
  #            ),
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
  # # ceo menu
  # navbarMenu(
  #   get_actual('ceo'),
  #   vendor_import_performance_tab
  # )
) # end navbarPage