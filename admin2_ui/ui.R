## Admin2 ui.R ##
# Admin2 is admin 1 without the ability to import/export inventory
library(shiny); library(shinythemes); library(shinyalert)
require(DT)
navbarPage(
  theme = shinytheme("united"), title = company_name,
  tabPanel(
    ui_elem$actual[ui_elem$label=='lookups'],
    fluidRow(
      style = "background-color:#f5f5f5;",
      div(
        style="display: inline-block;vertical-align:top;",
        selectInput(
        inputId = 'lu_tbl_selector', 
        label = ui_elem$actual[ui_elem$label=='choose_table'],
        choices = lu_tbl_list)),
      div(
        style="display: inline-block;vertical-align:top;",
        HTML("<br>"),
        actionButton(
        "print_lu_tbl",
        ui_elem$actual[ui_elem$label=='printReport'])),
      DT::dataTableOutput('lookup_tbl_output')
    )
  ), # end lookup tab
  tabPanel(
    ui_elem$actual[ui_elem$label=='reports'],
    fluidRow(
      style = "background-color:#f5f5f5;",
      box(width = 2, height = 800,
          selectInput(
            inputId = 'report_type', label = ui_elem$actual[
              ui_elem$label=='reportType'],
            choices = report_list$actual),
          conditionalPanel(
            condition = paste0(
              "input.report_type == '",date_range_tbl_local,"'"),
            dateInput(inputId = 'from_date',
                      label = ui_elem$actual[
                        ui_elem$label=='from_date'],
                      value = "2019-11-04", format = date_format_alt)
          ),
          dateInput(inputId = 'to_date',
                      label = ui_elem$actual[
                        ui_elem$label=='to_date'],
                      value = Sys.Date(), format = date_format_alt
          ),
          actionButton(
            "printReport", ui_elem$actual[ui_elem$label=='printReport'])
          ,
          DT::dataTableOutput('report_tbl_output')
      )
    )
  ) # end Reports tab
) # end navbarPage