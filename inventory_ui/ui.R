## Invenage ui.R ##
library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = company_name, titleWidth = 200
  ),
  ## Sidebar content
  dashboardSidebar(width=200,
                   sidebarMenu(
                     menuItem(ui_elem$actual[ui_elem$label=='inv_out'],
                              tabName="inventoryOut", icon = icon("minus")),
                     menuItem(ui_elem$actual[ui_elem$label=='lookups'],
                              tabName = "Lookup", icon = icon("search")),
                     menuItem(ui_elem$actual[ui_elem$label=='reports'],
                              tabName = "Reports", icon = icon("wrench"))
                   )
  ),
  dashboardBody(
    # ---------------------------- Xuat Kho tab UI -----------------------------
    tabItems(
      tabItem(tabName = "inventoryOut",
              fluidRow(
                box(width=3, height = 600,
                    htmlOutput('pxk_selector'),
                    htmlOutput('customer_selector'),
                    htmlOutput('prod_name_selector'),
                    htmlOutput("qty_selector"),
                    htmlOutput("unit_selector"),
                    htmlOutput("lot_selector"),
                    htmlOutput("warehouse_selector"),
                    
                    h5('')
                ),
                box(width = 2, height = 600,
                    htmlOutput("unit_price"),
                    htmlOutput("payment_selector"),
                    htmlOutput("pxk_note"),
                    h4(ui_elem$actual[ui_elem$label=='product_info']),
                    htmlOutput("prod_info_str"),
                    actionButton("inventory_out",
                                 ui_elem$actual[ui_elem$label=='inv_out']),
                    h5('')
                ),
                box(width = 7, height = 500,
                    h3(ui_elem$actual[ui_elem$label=='current_pxk']),
                    tableOutput("current_pxk_tbl"),
                    actionButton("del_last_entry",
                                 ui_elem$actual[ui_elem$label=='del_last_entry']),
                    actionButton("complete_form",
                                 ui_elem$actual[ui_elem$label=='complete_form']),
                    actionButton("reload_pxk",
                                 ui_elem$actual[ui_elem$label=='reload_pxk'])
                )
              )
      )
      , # end of export tab
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
                box(width = 3, height = 400,
                    h3(ui_elem$actual[ui_elem$label=='Reports']),
                    selectInput(inputId = 'report_type',
                                label = ui_elem$actual[
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
                    conditionalPanel(
                      condition = paste0(
                        "input.report_type == '",date_range_tbl_local,"'"),
                    dateInput(inputId = 'to_date',
                              label = ui_elem$actual[
                                ui_elem$label=='to_date'], 
                              value = "2019-11-10", format = date_format_alt)
                    ),
                    actionButton("printReport",
                                 ui_elem$actual[
                                   ui_elem$label=='printReport'])
                )
              )
      )
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage