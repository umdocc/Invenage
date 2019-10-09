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
      menuItem(ui_elem$actual[ui_elem$label=='tools'],
               tabName = "Tools", icon = icon("wrench")),
      menuItem(ui_elem$actual[ui_elem$label=='lookups'],
               tabName = "Lookup", icon = icon("search"))
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
            selectizeInput(inputId = "unit_price",
                             label = ui_elem$actual[
                               ui_elem$label=='unit_price'],
                             choices='',options = list(create=T)),
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
      # , # end of export tab
  # tabItem(tabName = 'Lookup',
  #       fluidRow(
  #         selectInput(inputId = 'lookup_tbl_select',
  #             label = ui_elem$actual[ui_elem$label=='choose_table'],
  #             choices = lu_tbl_list),
  #         dataTableOutput('lookup_tbl_output')
  #       )
  #     )
  # , # end of Lookup tab
  #     tabItem(tabName = 'Tools',
  #       fluidRow(
  #         box(width = 3, height = 400,
  #           h3(ui_elem$actual[ui_elem$label=='addCustomer']),
  #             textInput(inputId = 'addCustomerName',
  #               label = ui_elem$actual[
  #                 ui_elem$label=='customerName']),
  #           textInput(inputId = 'addCustomerEmail',
  #                     label = ui_elem$actual[
  #                       ui_elem$label=='customerEmail']),
  #             actionButton("addCustomer",
  #               ui_elem$actual[
  #                 ui_elem$label=='addCustomer']),
  #           htmlOutput("addCustomerSuccess")
  #           ),
  #         box(width = 3, height = 400,
  #         h3(ui_elem$actual[ui_elem$label=='addPackaging']),
  #         htmlOutput('addPackagingName'),
  #         htmlOutput('addPackagingUnit'),
  #         textInput(inputId ="addPackagingNum",
  #                   label = ui_elem$actual[
  #                        ui_elem$label=='numericPackaging']),
  #         actionButton("addPackaging",
  #                      ui_elem$actual[
  #                        ui_elem$label=='addPackaging']),
  #         htmlOutput("addPackagingSuccess")
  #         ),
  #         box(width = 3, height = 400,
  #             h3(ui_elem$actual[ui_elem$label=='Reports']),
  #             selectInput(inputId = 'reportType',
  #                         label = ui_elem$actual[
  #                           ui_elem$label=='reportType'],
  #                         choices = ui_elem$actual[
  #                           ui_elem$Group=='reportType']),
  #             actionButton("printReport",
  #                          ui_elem$actual[
  #                            ui_elem$label=='printReport'])
  #         ),
  #         box(width = 3, height = 400,
  #             h3(ui_elem$actual[ui_elem$label==''])
  #         ),
  #         h5(copyrightString) # placeholder
  #         )
  #     )
  # , # end of Tools tab
  # tabItem(tabName = 'systemInfo',
  #         fluidRow(
  #           box(width=6, height = 650,
  #             h5('Invenage development build')
  #           )
  #         )
  #       ) # end of info tab
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage