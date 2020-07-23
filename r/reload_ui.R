# attemp to handle all ui refresh in single file
reload_ui <- function(input,output,ui_list){
# --------------------------- inv_out ui elements ------------------------------
  # inv_out customer selector
  if ('customer_selector' %in% ui_list){
    output$customer_selector <- render_customer_list(
      'customer_name', type='inv_out', input) # customer
  }
  #inv_out prod_name selector
  if ('prod_name_select' %in% ui_list){
    output$prod_name_select <- render_prod_name_list(
      input,product_info,'prod_name_select') # prod_name
  }
  # inv_out qty selector
  if ('qty_selector' %in% ui_list){
    output$qty_selector <- render_qty(iid='qty_selector')
  }
  # inv_out unit selector
  if ('unit_selector' %in% ui_list){
  output$unit_selector <- render_unit(input,iid='unit_selector')
  }
  # inv_out warehouse selector
  if ('warehouse_selector' %in% ui_list){
    output$warehouse_selector <- render_warehouse(
      input, 'warehouse_selector',warehouse_info)
  }
  # inv_out lot selector
  if ('lot_select' %in% ui_list){
    output$lot_select <- render_lot(input, iid='lot_select')
  }
  #inv_out payment type
  if ('payment_selector' %in% ui_list){
    output$payment_selector <- render_payment_type(
      input, iid = 'payment_type', ui_type = 'inv_out')
  }
  # inv_out unit price
  if ('unit_price' %in% ui_list){
    output$unit_price <- render_price(input,iid='unit_price')
  }
  #inv_out tender name
  if ('tender_name' %in% ui_list){
    output$tender_name <- render_tender_list('tender_name', config_dict, input)
  }
  # inv_out pxk note
  if ('pxk_note' %in% ui_list){
    output$pxk_note <- render_note(iid='pxk_note') #Note
  }
  # inv_out product info panel
  if ('prod_info_str' %in% ui_list){
    output$prod_info_str <- render_prod_info(input)
  }
  # inv_out current pxk information line
  if ('current_pxk_info' %in% ui_list){
    output$current_pxk_info <- render_current_pxk_infostr(config_dict)
  }
  # inv_out current pxk display table
  if ('current_pxk_tbl' %in% ui_list){
    output$current_pxk_tbl <- render_invout_pxktable() # reload the PXK table
  }
  # inv_out list of stt under the display table 
  if ('invout_stt_list' %in% ui_list){
    output$invout_stt_list <- render_invout_stt_list(
        config_dict, 'invout_stt_list')
  }

  # --------------------------- inv_in ui elements ----------------------------
  # inv_in unit selector
  if ('in_unit' %in% ui_list){
    output$in_unit <- render_unit(input,'in_unit',type='inv_in') # in_unit
  }
  
  # --------------------------- lu_report ui elements --------------------------
  # pxk_man list of pxk
  if ('man_pxk_list' %in% ui_list){
    output$man_pxk_list <- render_pxk_list(input, config_dict, 'man_pxk_list')
  }
  if ('lu_report_tbl_selector' %in% ui_list){
    output$lu_report_tbl_selector <- render_lu_report_list(
      input, 'lu_report_tbl_selector')
  }
  
  # --------------------------- update_db ui elements --------------------------
  if ('add_orig_vendor' %in% ui_list){
    output$add_orig_vendor <- render_vendor_list(
      input, 'add_orig_vendor', allow_add = T, tab = 'update_db')
  }
  # ----------------------------- hr_log elements -------------------------------
  if ('admin_name' %in% ui_list){
    output$admin_name <- render_admin_name()
  }
  if ('hour_logged' %in% ui_list){
    output$hour_logged <- render_hour_logged()
  }
  if ('task_desc' %in% ui_list){
    output$task_desc <- render_task_desc()
  }
  if ('admin_activity_log' %in% ui_list){
    output$admin_activity_log <- render_activity_log(admin_id)
  }
  
  # ------------------------ invoice_update ui elements ------------------------
  if ('invoice_vendor' %in% ui_list){
    output$invoice_vendor <- render_vendor_list(
      input, 'invoice_vendor', allow_add = F, tab = 'invoice_update')
  }
  if ('vendor_invoice_num' %in% ui_list){
    output$vendor_invoice_num <- render_invoice_num(
      input, 'vendor_invoice_num', allow_add = T)
  }
  if ('invoice_currency' %in% ui_list){
    output$invoice_currency <- render_invoice_currency(
      input, 'invoice_currency', allow_add = T)
  }
  if ('invoice_po_num' %in% ui_list){
    output$invoice_po_num <- render_po_list(
      'invoice_po_num', config_dict)
  }
  return(output)
}