# as invenage expand, we need to consider separate file for tabs

hr_log_tab <- tabPanel(
  ui_elem$actual[ui_elem$label=='hr_log'],
  fluidRow(
    style = "background-color:#f5f5f5;",
    box(width = 3, height = 800,
        htmlOutput('admin_name'),
    ),
    box(width = 9, height = 800,
        
    )
  )
)

pxk_man_tab <- tabPanel(
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
)