## Invenage ui.R ##
library(shiny)
library(shinydashboard)
require(DT)
dashboardPage(
  dashboardHeader(title = company_name, titleWidth = 200
  ),
  ## Sidebar content
  dashboardSidebar(width=200,
                   sidebarMenu(
                     menuItem(ui_elem$actual[ui_elem$label=='inv_out'],
                              tabName="inv_out", icon = icon("minus")),
                     menuItem(ui_elem$actual[ui_elem$label=='lookups'],
                              tabName = "Lookup", icon = icon("search")),
                     menuItem(ui_elem$actual[ui_elem$label=='reports'],
                              tabName = "Reports", icon = icon("list-alt")),
                     menuItem(ui_elem$actual[ui_elem$label=='pxk_man'],
                              tabName = "pxk_man", icon = icon("wrench")),
                     
                     menuItem('TT',
                              tabName = "sys_info", icon = icon("cog"))
                   )
  ),
  dashboardBody(
    # ---------------------------- Xuat Kho tab UI -----------------------------
    tabItems(
      tabItem(tabName = "inv_out",
              fluidRow(
                box(width=3, height = 580,
                    htmlOutput('customer_selector'),
                    htmlOutput('prod_name_select'),
                    div(style="display: inline-block;vertical-align:top; \
                        width: 100px", htmlOutput("qty_selector")),
                    div(style="display: inline-block;vertical-align:top; \
                        width: 100px",
                        htmlOutput("unit_selector")),
                    div(style="display: inline-block;vertical-align:top; \
                        width: 100px",htmlOutput("lot_select")),
                    div(style="display: inline-block;vertical-align:top; \
                        width: 100px",
                        htmlOutput("warehouse_selector")),
                    div(style="display: inline-block;vertical-align:top; \
                        width: 100px", htmlOutput("unit_price")),
                    div(style="display: inline-block;vertical-align:top; \
                        width: 100px", htmlOutput("payment_selector")),
                    htmlOutput("prod_info_str"),
                    actionButton("inventory_out",
                                 ui_elem$actual[ui_elem$label=='inv_out'])
                    # h4(ui_elem$actual[ui_elem$label=='sys_msg']),
                    # htmlOutput("sys_msg")
                ),

                box(width = 9, height = 600,
                    htmlOutput("current_pxk_info"),
                    DT::dataTableOutput("current_pxk_tbl"),
                    h4(), #space
                    div(
                      style="display: inline-block;vertical-align:top;", 
                      h5(ui_elem$actual[ui_elem$label=='del_selected_stt'])
                    ),
                    div(style="display: inline-block;vertical-align:top; \
                        width: 5px;",HTML("<br>")
                    ),
                    div(style="display: inline-block;vertical-align:top; \
                        width: 100px;",
                        htmlOutput('invout_stt_list')
                    ),
                    div(style="display: inline-block;vertical-align:top; \
                        width: 150px;",
                        actionButton(
                          "del_invout_stt", 
                          ui_elem$actual[ui_elem$label=='delete_stt'])
                    ),
                    div(style="display: inline-block;vertical-align:top; \
                        position:absolute;right:10px",
                        actionButton(
                          "complete_form",
                          ui_elem$actual[ui_elem$label=='complete_form'])
                    )
                )
              )
      ), # end of export tab
      tabItem(tabName = 'Lookup',
              fluidRow(
                selectInput(inputId = 'lu_tbl_selector',
                            label = ui_elem$actual[ui_elem$label=='choose_table'],
                            choices = lu_tbl_list),
                DT::dataTableOutput('lookup_tbl_output')
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
      ),
      tabItem(tabName = 'pxk_man',
              fluidRow(
                box(width = 3, height = 600,
                    htmlOutput('man_pxk_list')
                ),
                box(width = 9, height = 600,
                    h3(ui_elem$actual[ui_elem$label=='pxk_info']),
                    DT::dataTableOutput('pxk_detail'),
                    div(
                      style="display: inline-block;vertical-align:top;", 
                      h5(ui_elem$actual[ui_elem$label=='del_selected_stt'])
                    ),
                    div(style="display: inline-block;vertical-align:top; \
                        width: 100px;", htmlOutput('stt_select')),
                    div(style="display: inline-block;vertical-align:top; \
                        width: 150px;",
                        actionButton(
                          "delete_stt_man",
                          ui_elem$actual[ui_elem$label=='delete_stt'])
                    )
                )
              )
      ), # end of pxk_man tab
      tabItem(tabName = 'sys_info',
              fluidRow(
                box(width = 3, height = 400,
                    h3('invenage development build')
                )
              )
      ) # end of sys_info tab
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage