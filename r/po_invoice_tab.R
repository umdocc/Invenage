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
        htmlOutput('invoice_amount'),
        htmlOutput('invoice_cd_num'),
        htmlOutput('invoice_po_num'),
        textInput('payment_id',
                  ui_elem$actual[ui_elem$label=='payment_id']),
        textInput('invoice_note',
                  ui_elem$actual[ui_elem$label=='note']),
        actionButton(
          'update_invoice',
          ui_elem$actual[ui_elem$label=='invoice_update'])
      ),
      box(
        width=9, height = 800,
        DT::dataTableOutput("vendor_invoice_tbl")
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