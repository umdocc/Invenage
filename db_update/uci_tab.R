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
          htmlOutput('uci_customer_name'),
          textInput('uci_customer_address',
                    label=uielem$customer_address),
          textInput('uci_customer_email',
                    label=uielem$customer_email),
          textInput('uci_customer_phone',
                    label=uielem$customer_phone),
          textInput('uci_customer_tfn',
                    label=uielem$customer_tfn),
          actionButton("uci_add_customer",
                       uielem$add_customer)
      ),
      box(
        width=9, height = 800,
        DT::dataTableOutput("uci_data"),
        p()
      )
    )
  )
}


