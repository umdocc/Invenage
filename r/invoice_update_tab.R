# the invoice update tab is for logistics staff to update new 
# import-related invoice
if('invoice_update' %in% hidden_tab){
  invoice_update <- tabPanel(ui_elem$actual[ui_elem$label=='invoice_update'])
}else{
  invoice_update_tab <- tabPanel(
    theme = shinytheme("united"), 
    ui_elem$actual[ui_elem$label=='invoice_update'],
    fluidRow(
      box(
        width=3, height = 800,
        htmlOutput('invoice_vendor'),
        htmlOutput('vendor_invoice_num'),
        htmlOutput('invoice_currency'),
        actionButton(
          'update_invoice',
          ui_elem$actual[ui_elem$label=='invoice_update'])
      )
    )
  )
}

render_invoice_num <- function(input,iid,allow_add=T){renderUI({
  current_venid <- vendor_info$vendor_id[
    vendor_info$vendor==input$invoice_vendor]
  invoice_choice <- vendor_invoice$invoice_num[
    vendor_invoice$vendor_id==current_venid]
  selectizeInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='invoice'],
    choices = invoice_choice, options = list(create = allow_add))
  })
}

render_invoice_currency <- function(input,iid,allow_add=T){renderUI({
  current_venid <- vendor_info$vendor_id[
    vendor_info$vendor==input$invoice_vendor]
  currency_choice <- currency$currency
  currency_selected <- unique(
    vendor_invoice$currency_code[vendor_invoice$vendor_id==current_venid])[1]
  currency_selected <- currency$currency[
    currency$currency_code==currency_selected]
  selectInput(
    inputId = iid, label = ui_elem$actual[ui_elem$label=='currency'],
    choices = currency_choice,selected = currency_selected)
  })
}