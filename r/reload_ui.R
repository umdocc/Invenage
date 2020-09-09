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
      input, iid = 'warehouse_selector',
      ui_label = ui_elem$actual[ui_elem$label=='warehouse'],
      tab='inv_out')
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
  if ('in_invoice_num' %in% ui_list){
    output$in_invoice_num <- render_in_invoice_num(
      'in_invoice_num',ui_elem$actual[ui_elem$label=='invoice_num']) # in_unit
  }
  if ('in_prodname_select' %in% ui_list){  
    output$in_prodname_select <- render_prod_name_list(
      input,config_dict,'in_prodname_select') # prod_name
  }
  if ('in_vendor' %in% ui_list){    
    output$in_vendor <- render_vendor_list(
      input, iid = 'in_vendor', 
      ui_label = ui_elem$actual[ui_elem$label=='vendor'],
      allow_add = F)
  }
  if ('in_unit' %in% ui_list){    
    output$in_unit <- render_unit(input,'in_unit',type='inv_in')
  }
  if ('in_note' %in% ui_list){  
    output$in_note <- render_note('in_note')
  }
  if ('in_actual_unit_cost' %in% ui_list){  
    output$in_actual_unit_cost <- render_in_cost(
      'in_actual_unit_cost', input, config_dict)
  }
  if ('po_list_2load' %in% ui_list){  
    output$po_list_2load <-  render_po_list(input,'po_list_2load', config_dict)
  }
  if ('in_vat_percent' %in% ui_list){ 
    output$in_vat_percent <-  render_vat_percent(
      input,iid='in_vat_percent', ui_elem$actual[ui_elem$label=='vat_percent'],
      tab = 'inv_in')
  }
  if ('in_warehouse' %in% ui_list){ 
    output$in_warehouse <-  render_warehouse(
      input, iid='in_warehouse', 
      ui_label=ui_elem$actual[ui_elem$label=='in_warehouse'], 
      tab = 'inv_in')
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
  
  # --------------------------- update_prod ui elements --------------------------
  if ('add_orig_vendor' %in% ui_list){
    output$add_orig_vendor <- render_vendor_list(
      input, iid = 'add_orig_vendor', 
      ui_label = get_actual('orig_vendor'))
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
    output$admin_activity_log <- render_dt(
      input, tbl_name='admin_activity_log')
  }
  
  if ('hrl_del_stt' %in% ui_list){
    output$hrl_del_stt <- render_stt(input, iid='hrl_del_stt')
  }
  
  if ('hrl_sum_plot' %in% ui_list){
    output$hrl_sum_plot <- render_hrl_plot(input)
  }
  
  
  # ------------------------ invoice_update ui elements ------------------------
  if ('invoice_vendor' %in% ui_list){
    output$invoice_vendor <- render_vendor_list(
      input, 'invoice_vendor', 
      ui_label = ui_elem$actual[ui_elem$label=='orig_vendor'],
      allow_add = F)
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
    output$invoice_po_num <- render_po_list(input,
      'invoice_po_num', config_dict,ui_label='po_name')
  }
  if ('invoice_amount' %in% ui_list){
    output$invoice_amount <- render_invoice_amount(
      input,
      'invoice_amount', ui_label='invoice_amount')
  }
  if ('invoice_cd_num' %in% ui_list){
    output$invoice_cd_num <- render_invoice_cd_num(
      input, 'invoice_cd_num', ui_label='invoice_cd_num')
  }
  if ('vendor_invoice_tbl' %in% ui_list){
    output$vendor_invoice_tbl <- render_output_tbl(input, 'vendor_invoice')
  }
  
  if ('piu_bankslip_vendor' %in% ui_list){
    output$piu_bankslip_vendor <- render_vendor_list(
      input, 'piu_bankslip_vendor',
      ui_label = get_actual('orig_vendor'),
      allow_add = F)
  }
  
  if ('piu_bankslip_invoice_num' %in% ui_list){
    output$piu_bankslip_invoice_num <- render_invoice_num(
      input, 'piu_bankslip_invoice_num', allow_add = F)
  }
  
  # ----------------------- write_po_price ui ----------------------------------
  if ('po_man_po_list' %in% ui_list){
    output$po_man_po_list <- render_po_list(input,
      'po_man_po_list', config_dict,ui_label='select_po')
  }
  if ('po_man_po_detail' %in% ui_list){
    output$po_man_po_detail <- render_output_tbl(input, 'po_detail')
  }
  
  # --------------------- update_import_price ui -------------------------------
  if ('uip_prod_name' %in% ui_list){
    output$uip_prod_name <- render_prod_name_list(
      input,product_info,'uip_prod_name') # prod_name
  }
  if ('uip_vendor' %in% ui_list){
    output$uip_vendor <- render_vendor_list(
      input, iid='uip_vendor', 
      ui_label=ui_elem$actual[ui_elem$label=='vendor'])
  }
  if ('uip_import_price' %in% ui_list){
    output$uip_import_price <- render_import_price(
      input, iid = 'uip_import_price',
      tab = 'update_import_price')
  }
  if ('uip_currency' %in% ui_list){
    output$uip_currency <- render_currency(
      input, iid = 'uip_currency')
  }
  if ('uip_min_order' %in% ui_list){
    output$uip_min_order <- render_min_order(input, iid = 'uip_min_order')
  }
  
  if ('uv_vendor' %in% ui_list){
    output$uv_vendor <- render_vendor_list(
      input, iid='uv_vendor', 
      ui_label=ui_elem$actual[ui_elem$label=='vendor'])
  }
  if ('vendor_info_tbl' %in% ui_list){
    output$vendor_info_tbl <- render_dt(
      input, tbl_name='vendor_info_tbl')
  }
  
  return(output)
}