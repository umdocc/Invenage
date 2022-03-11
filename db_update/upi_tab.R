# ----------------------------- update_prod_tab --------------------------------
if ('update_product_info' %in% hidden_tab){
  upi_tab <- tabPanel(uielem$update_product_info)
}else{
  upi_tab <- tabPanel(
    uielem$update_product_info,
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      # add_product box
      box(width = 3, height = 400,
          h4(uielem$add_product),
          div(style="display: inline-block;vertical-align:top;width: 150px",
              htmlOutput('upi_vendor')),
          div(style="display: inline-block;vertical-align:top;width: 120px",
              htmlOutput('upi_ref_smn')),
          htmlOutput('upi_prod_code'),
          textInput(inputId = 'upi_comm_name', label = uielem$comm_name),
          div(style="display: inline-block;vertical-align:top;width: 90px",
              htmlOutput('upi_ordering_unit')),
          div(style="display: inline-block;vertical-align:top;width: 90px",
              htmlOutput('upi_prod_type')),
          div(style="display: inline-block;vertical-align:top;width: 90px",
              htmlOutput('upi_default_warehouse')),
          actionButton(inputId  = "upi_add_product", 
                       label = uielem$add_product)
      ), # end add product box
      box(width = 3, height = 400,
          h4(uielem$add_pkg),
          htmlOutput('upi_add_pkg_comm_name'),
          div(style="display: inline-block;vertical-align:top;width: 80px",
              htmlOutput('upi_add_pkg_unit')),
          div(style="display: inline-block;vertical-align:top;width: 80px",
              htmlOutput('upi_add_unit_spp')),
          h4(uielem$explanation),
          htmlOutput("upi_add_pkg_explanation"),
          actionButton(
            "upi_add_pkg", uielem$add_pkg)
      ) # end add packaging box
    )
  )
}