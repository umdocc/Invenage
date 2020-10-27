# the purpose of sync_excel_po is to synchronise between the excel po and
# invenage database system.

if ('sync_excel_po' %in% hidden_tab){
  sync_excel_po_tab <- tabPanel(
    get_actual('sync_excel_po'))
}else{
  sync_excel_po_tab <- tabPanel(
    get_actual('sync_excel_po'),
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      box(width = 3, height = 400, style = "background-color:#f5f5f5;",
          htmlOutput('sep_po_name'),
          htmlOutput('sep_po_status'),
          actionButton("sync_excel_po",
                       get_actual('sync_excel_po')),
          actionButton("reload_po_list",
                       get_actual('reload_list'))
    )
  )
)
}

# ui loader 
sep_load_ui <- function(input,output,ui_list){
  if ('sep_po_name' %in% ui_list){
    output$sep_po_name <- render_sep_po_name()
  }
  if ('sep_po_status' %in% ui_list){
    output$sep_po_status <- render_sep_po_status()
  }
}

render_sep_po_name <- function(){renderUI({
  po_list <- get_po2sync()
  selectizeInput(
    inputId = "sep_po_name", label = get_actual("po_name"),
    choices = po_list, selected = po_list[1])
})}

render_sep_po_status <- function(){renderUI({
  radioButtons(
    inputId = 'sep_po_status',
    label = NULL,
    choices = c("complete","final"),
    selected = NULL,
    inline = T)
})}