# inv_out tab ui and functions
# ---------------------------- shiny ui object --------------------------------
if('inv_in' %in% hidden_tab){
  inv_in_tab <- tabPanel(ui_elem$actual[ui_elem$label=='inv_in'])
}else{
inv_in_tab <- tabPanel(
  theme = shinytheme("united"), ui_elem$actual[ui_elem$label=='inv_in'],
  fluidRow(
    box(
      width=3, height = 800,
      p(), # space
      h3(ui_elem$actual[ui_elem$label=='inv_in']),
      htmlOutput('in_prodname_select'),
      htmlOutput('in_vendor'),
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
      div(style="display: inline-block;vertical-align:top;width: 5px;",
          HTML("<br>")),
      div(style="display: inline-block;vertical-align:top;width: 140px",
          htmlOutput('in_actual_unit_cost')),
      div(style="display: inline-block;vertical-align:top;width: 140px",
          htmlOutput('in_note')),
      actionButton("inv_in",
                   ui_elem$actual[ui_elem$label=='inv_in']),
      p(),
      h3(ui_elem$actual[ui_elem$label=='load_excel_po']),
      htmlOutput('po_list_2load'),
      actionButton("load_excel_po",
                   ui_elem$actual[ui_elem$label=='load_excel_po']),
    ),
    box(
      width=9, height = 800,
      h3(ui_elem$actual[ui_elem$label=='recent_import']),
      DT::dataTableOutput("latest_import_tbl"),
      p() #space
      
    )
  )
)
}
render_import_tbl <- function(){DT::renderDataTable({
  import_log_tbl <-merge(
    import_log, product_info %>% select(prod_code,comm_name), all.x=T) 
  import_log_tbl <- import_log_tbl[order(import_log_tbl$id, decreasing = T),]
  import_log_tbl <- import_log_tbl %>% 
    select(comm_name, po_name, qty, unit, lot, exp_date, actual_unit_cost, note)
  output <- translate_tbl_column(import_log_tbl, ui_elem)
  DT::datatable(output, options = list(pageLength = 10),rownames=F)
})
}