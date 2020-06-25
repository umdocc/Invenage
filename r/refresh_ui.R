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
  
  # ---------------------------- pxk_man ui elements ---------------------------
  # pxk_man list of pxk
  if ('man_pxk_list' %in% ui_list){
    output$man_pxk_list <- render_pxk_list(input, config_dict, 'man_pxk_list')
  }
  return(output)
}