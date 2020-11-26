# inv_out tab ui and functions
# ---------------------------- shiny ui object --------------------------------
if('sync_excel_po' %in% hidden_tab){
  sync_excel_po_tab <- tabPanel(
    theme = shinytheme("united"), get_actual('sync_excel_po'))
}else{
  sync_excel_po_tab <- tabPanel(
    theme = shinytheme("united"), get_actual('sync_excel_po'),
    fluidRow(
      box(
        width=3, height = 800,
        p(), # space
        htmlOutput('sep_po_list'),
        htmlOutput('sep_po_status'),
        actionButton("sep_sync_excel_po",
                     get_actual('sync_excel_po')),
        actionButton("sep_write_import_price",
                     get_actual('add_import_price')),
        p()
      ),
      box(
        width=9, height = 800,
        p(), #space
        DT::dataTableOutput("sep_po_data_tbl"),
        p()
      )
    )
  )
}

# render for sep_po_list
render_sep_po_list <- function(){
  renderUI({
    po_list <- po_info$po_name[po_info$completed==0]
    po_choice <- po_list[1]

    selectizeInput(
      inputId = "sep_po_list", label = get_actual("select_po"),
      choices = po_list, selected =  po_choice
    )
  })
}

render_po_data_tbl <- function(input){
  DT::renderDataTable({
    # set ui variables
    numofrow <- 10; allow_edit <- T
    
    # get the data
    out_table <- po_info
    
    # translate the column label and display
    out_table <- translate_tbl_column(out_table,ui_elem)
    DT::datatable(out_table, options = list(pageLength = numofrow),rownames=F,
                  editable = allow_edit)
  })
}
