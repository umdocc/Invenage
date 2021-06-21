# this tab handle all the update ui and will be big

# ui layout code
# ----------------------------- update_customer_tab ----------------------------
if ('update_customer' %in% hidden_tab){
  update_customer_tab <- tabPanel(
    ui_elem$actual[ui_elem$label=='update_customer'])
}else{
  update_customer_tab <- tabPanel(
    ui_elem$actual[ui_elem$label=='update_customer'],
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      box(width = 3, height = 400, style = "background-color:#f5f5f5;",
          div(style="display: inline-block;padding-top:2px;;width: 200px",
              h4(ui_elem$actual[ui_elem$label=='add_customer'])),
          htmlOutput('add_customer_name'),
          textInput('add_customer_address',
                    label=ui_elem$actual[ui_elem$label=='customer_address']),
          textInput('add_customer_email',
                    label=ui_elem$actual[ui_elem$label=='customer_email']),
          div(style="display: inline-block;vertical-align:top;width: 150px",
              textInput('add_customer_phone',
                        label=ui_elem$actual[ui_elem$label=='customer_phone'])),
          div(style="display: inline-block;vertical-align:top;width: 150px",
              textInput('add_customer_tfn',
                        label=ui_elem$actual[ui_elem$label=='customer_tfn'])),
          actionButton(
            "add_customer", ui_elem$actual[ui_elem$label=='add_customer'])
      )
    )
  )
}


