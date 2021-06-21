# ----------------------------- update_prod_tab --------------------------------
if ('update_product' %in% hidden_tab){
  update_product_tab <- tabPanel(ui_elem$actual[ui_elem$label=='update_product'])
}else{
  update_product_tab <- tabPanel(
    ui_elem$actual[ui_elem$label=='update_product'],
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      # add_product box
      box(width = 3, height = 400, style = "background-color:#f5f5f5;",
          div(style="display: inline-block;padding-top:2px;;width: 200px",
              h4(ui_elem$actual[ui_elem$label=='add_product'])),
          htmlOutput('add_prod_code'),
          htmlOutput('add_name'),
          div(style="display: inline-block;vertical-align:top;width: 130px",
              htmlOutput('add_ref_smn')),
          div(style="display: inline-block;vertical-align:top;width: 130px",
              htmlOutput('add_ordering_unit')),
          div(style="display: inline-block;vertical-align:top;width: 130px",
              htmlOutput('add_orig_vendor')),
          div(style="display: inline-block;vertical-align:top;width: 130px",
              htmlOutput('add_prod_type')),
          div(style="display: inline-block;vertical-align:top;width: 130px",
              htmlOutput('add_warehouse')),
          div(style="display: inline-block;padding-bottom:2px;;width: 200px",
              actionButton(
                "add_product", ui_elem$actual[ui_elem$label=='add_product']))
      ),
      box(width = 3, height = 400, style = "background-color:#f5f5f5;",
          div(style="display: inline-block;padding-top:2px;;width: 200px",
              h4(ui_elem$actual[ui_elem$label=='add_pkg'])),
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