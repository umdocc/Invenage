vii_load_ui <- function(input,output,ui_list){
  if ('vii_vendor' %in% ui_list){
    output$vii_vendor <- render_vii_vendor(input)
  }
  if ('vii_invoice_num' %in% ui_list){
    output$vii_invoice_num <- render_vii_invoice_num(input)
  }
  if ('vii_amount' %in% ui_list){
    output$vii_amount <- render_vii_amount(input)
  }
  if ('vii_invoice_cdn' %in% ui_list){
    output$vii_invoice_cdn <- render_vii_invoice_cdn(input)
  }
  return(output)
}

vii_init <- function(input, output){
  output <- vii_load_ui(
    input,output,
    c("vii_vendor", "vii_invoice_num"))
  return(output)
}

render_vii_vendor <- function(input){renderUI({
  
  #simply a list of vendor with import_from = 1
  vendor_list <- vendor_info$vendor[vendor_info$import_from==1]
  
  #render ui
  selectizeInput(
    inputId = "vii_vendor", label = uielem$vendor,
    choices = vendor_list,
    selected = vendor_list[1],
    options = list(create = F))
})
}

render_vii_invoice_num <- function(input){renderUI({
  
  invoice_list <- NULL
  
  #render ui
  selectizeInput(
    inputId = "vii_invoice_num", label = uielem$invoice_num,
    choices = invoice_list,
    selected = invoice_list[1],
    options = list(create = T))
})
}