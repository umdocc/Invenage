# ----------------------------- update_customer_tab ----------------------------
if ('tech_service_warranty' %in% hidden_tab){
  tech_service_warranty_tab <- tabPanel(
    get_actual('tech_service_warranty'))
}else{
  tech_service_warranty_tab <- tabPanel(
    get_actual('tech_service_warranty'),
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      box(width = 3, height = 400, style = "background-color:#f5f5f5;",
          htmlOutput("tsw_product_name")
      ),
      box(width = 9, height = 800, style = "background-color:#f5f5f5;",
          p()
      )
    )
  )
}
