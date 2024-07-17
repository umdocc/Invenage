# ------------------------------ sysinfo tab -----------------------------------
# should be default on all release
  sysinfo_tab <- tabPanel(
    theme = shinytheme(config$app_theme), "SysInfo",
    fluidRow(
      h3("System Information"),
      p(paste("Server:", config$sql_host)),
      p(paste("Admin ID:", config$admin_id)),
      h3("To Do"),
      p("cdn oversold check"),
      p()
    )# end fluidRow
  ) # end of ui object
