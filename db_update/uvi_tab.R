# ------------------------- update_vendor tab ----------------------------------
if ('update_vendor_info' %in% hidden_tab){
  uvi_tab <- tabPanel(uielem$update_vendor_info)
}else{
  uvi_tab <- tabPanel(
    uielem$update_vendor_info,
    fluidRow(
      box(width = 3, height = 800,
          htmlOutput('uvi_vendor'),
          htmlOutput('uvi_vendor_orig'),
          htmlOutput('uvi_vendor_local'),
          actionButton("uvi_add_vendor",uielem$add_vendor),
      ),
      box(
        width=9, height = 800,
        # DT::dataTableOutput("vendor_info_tbl"),
        p()
      )
    )
  )
}