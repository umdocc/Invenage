## Invenage ui.R ##
library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(
    title = companyName
  ),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Xuất Kho", tabName = "export", icon = icon("minus")),
      menuItem("Doanh Số", tabName = "salesView", icon = icon("minus")),
      menuItem("Công Cụ", tabName = "Tools", icon = icon("minus"))
    )
  ),
  dashboardBody(
    # ---------------------------- Xuat Linh Kien tab UI -----------------------
    tabItems(
      tabItem(tabName = "export",
        fluidRow(
          box(width=4,
            htmlOutput("pxkSelector"),
            selectizeInput(inputId = 'customerName',
              label = localisation$actual[localisation$label=='customerName'],
              choices = customerInfo$customerName),
            selectInput(inputId = "prodName",
              label = localisation$actual[localisation$label=='prodName'],
              choices=productInfo$Name),
            htmlOutput("nsxSelector"),
            htmlOutput("unitSelector"),
            selectizeInput(inputId = "unitPrice",
                           label = localisation$actual[localisation$label=='unitPrice'],
                           choices='',options = list(create=T)),
            selectizeInput(inputId = "Amount",
                        label = localisation$actual[localisation$label=='Amount'],
                        choices=c(1:100),options = list(create=T)),
            htmlOutput("lotSelector"),
            htmlOutput("expDateSelector"),
            htmlOutput("warehouseSelector"),
            h4(localisation$actual[localisation$label=='productInfo']),
            htmlOutput("thongTinSP"),
            actionButton("inventoryOut",
                    localisation$actual[localisation$label=='inventoryOut'])
          ),
          box(width = 8,
            h3(localisation$actual[localisation$label=='currentPXK']),
            tableOutput("currentPXK"),
            actionButton("delLastEntry",
                  localisation$actual[localisation$label=='delLastEntry']),
            actionButton("completeForm",
                       localisation$actual[localisation$label=='completeForm'])
          
          )
        )
      ), # end of export tab
      tabItem(tabName = 'salesView',
        fluidRow(
          box(width = 4,
            selectInput(inputId = 'saleViewCustomerName',
              label = localisation$actual[localisation$label=='customerName'],
              choices = customerInfo$customerName),
            selectInput(inputId = 'rollingMth',
                        label = localisation$actual[localisation$label=='rollingMth'],
                        choices = 1:24,selected=12),
            selectInput(inputId = 'groupType',
                        label = localisation$actual[localisation$label=='groupType'],
                        choices = localisation$actual[localisation$label=='All'])
          ),
          box(width = 8,
            textOutput('testText'),
            plotOutput('saleViewPlot')
          )
        )
      ), # end of salesView  tab
      tabItem(tabName = 'Tools',
              fluidRow(
                box(width = 4,
                    h3(localisation$actual[localisation$label=='addCustomer']),
                    textInput(inputId = 'customerCode',
                                label = localisation$actual[
                                  localisation$label=='customerCode']),
                    h5('Ex: CHORAY,NGUYENHOANG,.....'),
                    textInput(inputId = 'customerName',
                              label = localisation$actual[
                                localisation$label=='customerName']),
                    h5('Ex: Bệnh Viện Chợ Rẫy TP HCM,
                       Phòng Khám Nguyễn Hoàng,.....'),
                    actionButton("addCustomer",
                                 localisation$actual[
                                   localisation$label=='addCustomer'])
                )
              )
      ) # end of Tools tab
    ) # end tabItems
  ) # end dashboardBody 
) # end dashboardPage