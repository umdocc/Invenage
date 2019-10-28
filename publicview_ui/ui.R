## Invenage ui.R ##
library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = company_name, titleWidth = 200
  ),
  ## Sidebar content
  dashboardSidebar(width=200,
                   sidebarMenu(
                     menuItem(ui_elem$actual[ui_elem$label=='lookups'],
                              tabName = "Lookup", icon = icon("search")),
                     menuItem(ui_elem$actual[ui_elem$label=='reports'],
                              tabName = "Reports", icon = icon("wrench"))
                   )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Lookup',
              fluidRow(
                selectInput(inputId = 'lu_tbl_selector',
                            label = ui_elem$actual[ui_elem$label=='choose_table'],
                            choices = lu_tbl_list),
                dataTableOutput('lookup_tbl_output')
              )
      )
      , # end of Lookup tab
      tabItem(tabName = 'Reports',
              fluidRow(
                box(width = 4, height = 400,
                    h3(ui_elem$actual[ui_elem$label=='Reports']),
                    selectInput(inputId = 'report_type',
                                label = ui_elem$actual[
                                  ui_elem$label=='reportType'],
                                choices = report_list$actual),
                    actionButton("printReport",
                                 ui_elem$actual[
                                   ui_elem$label=='printReport'])
                )
              )
      )
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage