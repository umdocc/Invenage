# ----------------------------- update_prod_tab --------------------------------
if ('update_product' %in% hidden_tab){
  update_product_tab <- tabPanel(ui_elem$actual[ui_elem$label=='update_product'])
}else{
  update_product_tab <- tabPanel(
    ui_elem$actual[ui_elem$label=='update_product'],
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      # add_product box
      box(width = 3, height = 400,
          h4(ui_elem$actual[ui_elem$label=='add_product']),
          textInput(inputId = 'udp_prod_code', 
                    label = uielem$prod_code),
          textInput(inputId = 'udp_comm_name', 
                    label = uielem$comm_name),
          div(style="display: inline-block;vertical-align:top;width: 150px",
              htmlOutput('udp_vendor')),
          div(style="display: inline-block;vertical-align:top;width: 120px",
              textInput(inputId = 'udp_ref_smn',
                        label = uielem$ref_smn)),
          div(style="display: inline-block;vertical-align:top;width: 90px",
              textInput(inputId = 'udp_ordering_unit', 
                        label = uielem$ordering_unit)),
          div(style="display: inline-block;vertical-align:top;width: 90px",
              htmlOutput('add_prod_type')),
          div(style="display: inline-block;vertical-align:top;width: 90px",
              htmlOutput('add_warehouse')),
          actionButton(inputId  = "udp_add_product", 
                       label = uielem$add_product)
      ),
      box(width = 3, height = 400,
          h4(ui_elem$actual[ui_elem$label=='add_pkg']),
          htmlOutput('add_pkg_prod_name'),
          div(style="display: inline-block;vertical-align:top;width: 80px",
              textInput('add_pkg_unit',label=ui_elem$actual[ui_elem$label=='unit'])
          ),
          div(style="display: inline-block;vertical-align:top;width: 80px",
              textInput('add_unitspp',label=ui_elem$actual[ui_elem$label=='qty'])
          ),
          h4(ui_elem$actual[ui_elem$label=='explanation']),
          htmlOutput("add_pkg_str"),
          actionButton(
            "add_pkg", ui_elem$actual[ui_elem$label=='add_pkg'])
      )
    )
  )
}