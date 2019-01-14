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
      menuItem("Xuất Kho", tabName = "export", icon = icon("minus"))
    )
  ),
  dashboardBody(
    # ---------------------------- Xuat Linh Kien tab UI -----------------------
    tabItems(
      tabItem(tabName = "export",
        fluidRow(
          box(
            
            selectInput(inputId = "prodName",
              label = localisation$actual[localisation$label=='prodName'],
              choices=productInfos$Name),
            htmlOutput("nsxSelector"),
            htmlOutput("lotSelector"),
            htmlOutput("expDateSelector"),
            h4(localisation$actual[localisation$label=='productInfo']),
            textOutput("thongTinSP"),
            actionButton("inventoryOut", 
                    localisation$actual[localisation$label=='inventoryOut'])
          ),
          box(
            h3(localisation$actual[localisation$label=='lastEntry'])),
            textOutput("text1"),
            actionButton("delLastEntry",
                  localisation$actual[localisation$label=='delLastEntry'])
          )
        )
      )
    )
  )
