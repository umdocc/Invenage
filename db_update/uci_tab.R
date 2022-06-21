# this tab handle all the update ui and will be big

# ui layout code
# ----------------------------- update_customer_tab ----------------------------
if ('update_customer_info' %in% hidden_tab){
  uci_tab <- tabPanel(
    uielem$update_customer_info)
}else{
  uci_tab <- tabPanel(
    uielem$update_customer_info,
    fluidRow(
      box(width = 3, height = 800,
          div(style="display: inline-block;padding-top:2px;;width: 200px",
          htmlOutput('uci_customer_name')),
          textInput('uci_customer_address',
                    label=uielem$customer_address),
          textInput('uci_customer_email',
                    label=ui_elem$customer_email),
          textInput('uci_customer_phone',
                    label=ui_elem$customer_phone),
          textInput('uci_customer_tfn',
                    label=ui_elem$customer_tfn),
          actionButton("uci_add_customer",
                       ui_elem$add_customer)
      )
    )
  )
}


