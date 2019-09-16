## Invenage ui.R ##
library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = companyName, titleWidth = 200
  ),
  ## Sidebar content
  dashboardSidebar(width=200,
    sidebarMenu(
      menuItem(localisation$Actual[localisation$Label=='inventoryOut'],
               tabName="exportEntryView", icon = icon("minus")),
      menuItem("Công Cụ", tabName = "Tools", icon = icon("wrench")),
      menuItem("Tra Cứu", tabName = "Lookup", icon = icon("search")),
      menuItem("Thong Tin", tabName = "systemInfo", icon = icon("minus"))
    )
  ),
  dashboardBody(
    # ---------------------------- Xuat Kho tab UI -----------------------------
    tabItems(
      tabItem(tabName = "exportEntryView",
        fluidRow(
          box(width=3, height = 500,
            htmlOutput('pxkSelector'),
            htmlOutput('customerSelector'),
            htmlOutput('prodNameSelector'),
            htmlOutput("amountSelector"),
            htmlOutput("unitSelector"),
            selectizeInput(inputId = "unitPrice",
                           label = localisation$Actual[
                             localisation$Label=='unitPrice'],
                           choices='',options = list(create=T)),
            h5('')
          ),
          box(width = 2, height = 500,
            htmlOutput("lotSelector"),
            htmlOutput("pxkNote"),
            h4(localisation$Actual[localisation$Label=='productInfo']),
            htmlOutput("productInfoPane"),
            actionButton("inventoryOut",
                    localisation$Actual[localisation$Label=='inventoryOut'])
          ),
          box(width = 7, height = 500,
            h3(localisation$Actual[localisation$Label=='currentPXK']),
            tableOutput("currentPXKTable"),
            actionButton("delLastEntry",
                  localisation$Actual[localisation$Label=='delLastEntry']),
            actionButton("completeForm",
                       localisation$Actual[localisation$Label=='completeForm']),
            actionButton("reloadPXK",
                         localisation$Actual[localisation$Label=='reloadPXK'])
          )
        )
      ), # end of export tab
  tabItem(tabName = 'Lookup',
        fluidRow(
          selectInput(inputId = 'lookupTableSelector',
              label = localisation$Actual[localisation$Label=='chooseTable'],
              choices = lookupTableList),
          dataTableOutput('lookupTableOutput')
        )
      ), # end of Lookup tab
      tabItem(tabName = 'Tools',
        fluidRow(
          box(width = 3, height = 400,
            h3(localisation$Actual[localisation$Label=='addCustomer']),
              textInput(inputId = 'addCustomerName',
                label = localisation$Actual[
                  localisation$Label=='customerName']),
            textInput(inputId = 'addCustomerEmail',
                      label = localisation$Actual[
                        localisation$Label=='customerEmail']),
              actionButton("addCustomer",
                localisation$Actual[
                  localisation$Label=='addCustomer']),
            htmlOutput("addCustomerSuccess")
            ),
          box(width = 3, height = 400,
          h3(localisation$Actual[localisation$Label=='addPackaging']),
          htmlOutput('addPackagingName'),
          htmlOutput('addPackagingUnit'),
          textInput(inputId ="addPackagingNum",
                    label = localisation$Actual[
                         localisation$Label=='numericPackaging']),
          actionButton("addPackaging",
                       localisation$Actual[
                         localisation$Label=='addPackaging']),
          htmlOutput("addPackagingSuccess")
          ),
          box(width = 3, height = 400,
              h3(localisation$Actual[localisation$Label=='Reports']),
              selectInput(inputId = 'reportType',
                          label = localisation$Actual[
                            localisation$Label=='reportType'],
                          choices = localisation$Actual[
                            localisation$Group=='reportType']),
              actionButton("printReport",
                           localisation$Actual[
                             localisation$Label=='printReport'])
          ),
          box(width = 3, height = 400,
              h3(localisation$Actual[localisation$Label==''])
          ),
          h5(copyrightString) # placeholder
          )
      ), # end of Tools tab
  tabItem(tabName = 'systemInfo',
          fluidRow(
            box(width=6, height = 650,
              h5('Invenage development build')
            )
          )
        ) # end of info tab
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage