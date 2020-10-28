# renders for invoice related


render_invoice_num <- function(input,iid,allow_add=T){renderUI({
  if(iid=='vendor_invoice_num'){
    current_venid <- vendor_info$vendor_id[
      vendor_info$vendor==input$invoice_vendor]
    invoice_choice <- vendor_invoice$invoice_num[
      vendor_invoice$vendor_id==current_venid]
  }

  if(iid=='piu_bankslip_invoice_num'){
    current_venid <- vendor_info$vendor_id[
      vendor_info$vendor==input$piu_bankslip_vendor]
    invoice_choice <- vendor_invoice$invoice_num[
      vendor_invoice$vendor_id==current_venid]
  }
  
  selectizeInput(
    inputId = iid, label = get_actual('invoice'),
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

render_invoice_amount <- function(
  input,iid,ui_label='invoice_amount', allow_add=T){renderUI({
    current_invoice <- input$vendor_invoice_num
    invoice_amount <- db_read_query(
      paste0("select invoice_amount from vendor_invoice where invoice_num like'",
             current_invoice,"'"))$invoice_amount
    selectizeInput(
      inputId = iid, label = ui_elem$actual[ui_elem$label==ui_label],
      choices = invoice_amount,selected = invoice_amount[1],
      options = list(create = allow_add))
  })
}

render_invoice_cd_num <- function(
  input,iid,ui_label='invoice_cd_num',allow_add=T){renderUI({
    current_invoice <- input$vendor_invoice_num
    cd_num <- db_read_query(
      paste0("select invoice_cd_num from vendor_invoice where invoice_num like'",
             current_invoice,"'"))$invoice_cd_num
    selectizeInput(
      inputId = iid, label = ui_elem$actual[ui_elem$label==ui_label],
      choices = cd_num,selected = cd_num[1],
      options = list(create = allow_add))
  })
}