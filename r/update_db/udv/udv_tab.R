# ------------------------- update_vendor tab ----------------------------------
if ('update_vendor' %in% hidden_tab){
  update_vendor_tab <- tabPanel(get_actual('update_vendor'))
}else{
  update_vendor_tab <- tabPanel(
    get_actual('update_vendor'),
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      box(width = 3, height = 400, style = "background-color:#f5f5f5;",
          h4(get_actual("add_vendor")),
          htmlOutput('uv_vendor'),
          htmlOutput('uv_vendor_orig'),
          htmlOutput('uv_vendor_local'),
          actionButton("uv_update_vendor",get_actual('update_vendor'))
      ),
      box(
        width=9, height = 800,
        DT::dataTableOutput("vendor_info_tbl")
      )
    )
  )
}