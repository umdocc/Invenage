## Invenage ui.R ##
library(shiny); library(shinythemes); library(shinyalert)
require(DT)
navbarPage(
  theme = shinytheme("united"), title = company_name, id = 'main',
  # actual ui object construction for each tab is located 
  # in <tabname>_tab.R file
  
  # sale menu
  navbarMenu(
    get_actual('inv_out'),
    cdn_tab,
    pxk_man_tab
  ),
  
  # import menu
  inv_in_tab,
  
  # reports menu
  navbarMenu(title = uielem$reports,
             po_inventory_tab,
             sale_log_report_tab,
             import_log_report_tab
             ),
  
  # update_db menu
  navbarMenu(
    get_actual('update_db'),
    update_customer_tab,
    update_product_tab,
    update_vendor_invoice_tab,
    update_import_price_tab,
    update_vendor_tab
  ),
  
  # tools menu
  navbarMenu(
    get_actual('tools'),
    sync_excel_po_tab
  ),
  
  # service and warranty menu
  navbarMenu(
    get_actual('service_and_warranty'),
    tech_service_log_tab,
    tech_service_warranty_tab
  ),
  
  # ceo menu
  navbarMenu(
    get_actual('ceo'),
    vendor_import_performance_tab
  )
) # end navbarPage