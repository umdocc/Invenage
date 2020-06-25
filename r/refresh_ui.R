# attemp to handle all ui refresh in single file
refresh_ui <- function(input,output,ui_list){
  # reload UI
  if ('in_unit' %in% ui_list){
  output$in_unit <- render_unit(input,'in_unit',type='inv_in') # in_unit
  }
  
  if ('unit_selector' %in% ui_list){
  output$unit_selector <- render_unit(input,iid='unit_selector')
  }
  
  if ('customer_selector' %in% ui_list){
  output$customer_selector <- render_customer_list(
    'customer_name', type='inv_out', input) # customer
  }
  return(output)
}