## Invenage ui.R ##
library(shiny); library(shinythemes); library(shinyalert)
require(DT)
navbarPage(
  theme = shinytheme("united"), title = company_name, id = 'main',
  # actual ui object construction for each tab is located 
  # in <tabname>_tab.R file
  navbarMenu(
    get_actual('inv_out'),
    inv_out_tab,
    pxk_man_tab
  ),
  inv_in_tab,
  navbarMenu(
    get_actual('lu_report'),
    lu_report_tab,
    po_report_tab
  ),
  hr_log_tab,
  navbarMenu(
    get_actual('update_db'),
    update_customer_tab,
    update_product_tab,
    invoice_update_tab,
    update_import_price_tab,
    update_vendor_tab
  ),
  navbarMenu(
    get_actual('tools'),
    sync_excel_po_tab
  ),
  navbarMenu(
    get_actual('service_and_warranty'),
    tech_service_log_tab,
    tech_service_warranty_tab
  ),
  navbarMenu(
    get_actual('ceo'),
    vendor_import_performance_tab
  )
) # end navbarPage