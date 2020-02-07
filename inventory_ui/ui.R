## Invenage ui.R ##
library(shiny)
library(shinythemes)
require(DT)
navbarPage(
  theme = shinytheme("united"), title = company_name,
  tabPanel(
    theme = shinytheme("united"), ui_elem$actual[ui_elem$label=='inv_out'],
    fluidRow(
      box(
        width=3, height = 800,
        p(), #space
        htmlOutput('customer_selector'),
        htmlOutput('prod_name_select'),
        div(style="display: inline-block;vertical-align:top;width: 110px", 
            htmlOutput("qty_selector")),
        div(style="display: inline-block;vertical-align:top; \
                        width: 110px",
            htmlOutput("unit_selector")),
        div(style="display: inline-block;vertical-align:top; \
                        width: 110px",htmlOutput("lot_select")),
        div(style="display: inline-block;vertical-align:top; \
                        width: 110px",
            htmlOutput("warehouse_selector")),
        div(style="display: inline-block;vertical-align:top; \
                        width: 110px", htmlOutput("unit_price")),
        div(style="display: inline-block;vertical-align:top; \
                        width: 110px", htmlOutput("payment_selector")),
        textInput('pxk_note', ui_elem$actual[ui_elem$label=='note']),
        htmlOutput("prod_info_str"),
        actionButton("inventory_out",
                     ui_elem$actual[ui_elem$label=='inv_out']),
        htmlOutput("sys_msg")
      ),
      box(
        width = 9, height = 800,
        # style = "background-color:#c2e6ff;",
        p(),
        htmlOutput("current_pxk_info"),
        p(),
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
        div(
          style="display: inline-block;vertical-align:top;width: 150px;",
          actionButton(
            "del_invout_stt", ui_elem$actual[ui_elem$label=='delete_stt'])
        ),
        div(style="display: inline-block;vertical-align:top; \
                        position:absolute;right:15px",
            actionButton(
              "complete_form",
              ui_elem$actual[ui_elem$label=='complete_form'])
        )
      )# end inv_out box2
    )# end inv_out fluidRow
  ), #end inv_out tab
  tabPanel(
    theme = shinytheme("united"), ui_elem$actual[ui_elem$label=='inv_in'],
    fluidRow(
      box(
        width=3, height = 800,
        p(), # space
        htmlOutput('in_prodname_select'),
        div(style="display: inline-block;vertical-align:top;width: 110px",
            selectizeInput(
              inputId = 'in_qty', label = ui_elem$actual[ui_elem$label=='qty'],
              choices = 1:1000, options = list(create = TRUE))),
        div(style="display: inline-block;vertical-align:top;width: 110px",
            htmlOutput('in_unit')),
        div(style="display: inline-block;vertical-align:top; \
                        width: 5px;",HTML("<br>")),
        div(style="display: inline-block;vertical-align:top; \
                        width: 110px",
            textInput('in_lot',label=ui_elem$actual[ui_elem$label=='lot'])),
        div(style="display: inline-block;vertical-align:top; \
                        width: 110px",
            textInput('in_expdate',
                      label=ui_elem$actual[ui_elem$label=='exp_date'])),
        div(style="display: inline-block;vertical-align:top; \
                        width: 5px;",HTML("<br>")
        ),
        selectizeInput(
          inputId = 'in_unit_cost', label = ui_elem$actual[
            ui_elem$label=='unit_import_cost'],
          choices = 0, options = list(create = TRUE)),
        actionButton("inv_in",
                     ui_elem$actual[ui_elem$label=='inv_in']),
        p()
      ),
      box(
        width=9, height = 800,
        h3(ui_elem$actual[ui_elem$label=='recent_import']),
        DT::dataTableOutput("latest_import_tbl"),
        p() #space

      )
    )
  ), # end of import tab
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
  ), # end Reports tab
  tabPanel(
    ui_elem$actual[ui_elem$label=='pxk_man'],
    fluidRow(
      style = "background-color:#f5f5f5;",
      box(width = 3, height = 800,
          h4(ui_elem$actual[ui_elem$label=='edit_info']),
          htmlOutput('man_pxk_list'),
          htmlOutput('man_pxk_cust_select'),
          htmlOutput("manpxk_pay_change"),
          actionButton(
            "edit_pxk_info",
            ui_elem$actual[ui_elem$label=='edit_info'])
      ),
      box(width = 9, height = 800,
          h3(ui_elem$actual[ui_elem$label=='pxk_info']),
          htmlOutput('man_pxk_info'),
          p(),
          DT::dataTableOutput('pxk_detail'),
          h4(), #space
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
          ),
          div(style="display: inline-block;vertical-align:top; \
                        position:absolute;right:15px",
              actionButton(
                "print_pxk_man",
                ui_elem$actual[ui_elem$label=='print_pxk'])
          ),
          h5(ui_elem$actual[ui_elem$label=='edit_instructions'])
      )
    )
  ), # end pxk_man tab
  tabPanel(
    'About',
    fluidRow(
      style = "background-color:#f5f5f5;",
      textOutput('error_text')
    )
  ) # end About tab
) # end navbarPage