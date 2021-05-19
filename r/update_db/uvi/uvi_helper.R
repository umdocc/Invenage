# this function handle all invoice update, it will need only the shiny input
update_invoice_data <- function(input){
  # collect all data
  current_invoice_num <- input$vendor_invoice_num
  current_invoice_date <- input$uvi_invoice_date
  append_invoice <- data.frame(
    vendor_id = vendor_info$vendor_id[vendor_info$vendor==input$invoice_vendor],
    invoice_num = current_invoice_num,
    invoice_date = current_invoice_date,
    invoice_amount = as.numeric(input$invoice_amount),
    currency_code = currency$currency_code[
      currency$currency==input$invoice_currency],
    invoice_note = input$invoice_note,
    invoice_cd_num = input$invoice_cd_num,
    po_name = input$invoice_po_num
  )
  # update by first removing the old data
  
  del_query <- paste0("delete from vendor_invoice where invoice_num like'",
                      input$vendor_invoice_num,"'")
  db_exec_query(del_query)
  
  append_tbl_rld(config_dict,'vendor_invoice',append_invoice)
  show_alert('success','invoice_update_success','success')
}