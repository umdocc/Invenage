# ui renderer for hr_log tab
# ------------------------------- shiny ui object --------------------------------
hr_log_tab <- tabPanel(
  ui_elem$actual[ui_elem$label=='hr_log'],
  fluidRow(
    style = "background-color:#f5f5f5;",
    box(width = 3, height = 800,
        htmlOutput('admin_name'),
        htmlOutput('hour_logged'),
        htmlOutput('task_desc'),
        actionButton('task_input',
                     ui_elem$actual[ui_elem$label=='enter_data'])
    ),
    box(width = 9, height = 800,
        DT::dataTableOutput('admin_activity_log')
    )
  )
)

# ---------------------------- ui renderers --------------------------------------
render_admin_name <- function(){renderUI({
  admin_name <- staff_info$admin_name[
    staff_info$admin_id==as.integer(
      config_dict$value[config_dict$name=='admin_id'])]
  selectInput('admin_name',
              ui_elem$actual[ui_elem$label=='admin_name'],
              choices = admin_name,selected = admin_name)
}) }

render_hour_logged <- function(){renderUI({
  selectizeInput('hour_logged',
                 ui_elem$actual[ui_elem$label=='hour_logged'],
                 choices = seq(0.25,3,0.25),selected = 0.25)
}) }

render_task_desc <- function(){renderUI({
  textInput('task_desc',
                 ui_elem$actual[ui_elem$label=='task_desc'])
}) }

# function to render activity log table

# function to handle task_input button
write_activity_log <- function(input){
  activity_data <- data.frame(
    admin_id = admin_id,
    activity_date = format(Sys.Date(),'%Y-%m-%d'),
    hour_logged = input$hour_logged,
    detail = input$task_desc
  )
  append_tbl_rld(config_dict,'staff_activity_log',activity_data)
}

# render data for selected staff only
render_activity_log <- function(admin_id){DT::renderDataTable({
  current_staff_log <- staff_activity_log[
    staff_activity_log$admin_id==as.numeric(admin_id),]
  current_staff_log <- merge(
    current_staff_log,staff_info %>% select(admin_id,admin_name))
  # remove id, translate and render
  current_staff_log$id <- NULL
  current_staff_log$admin_id <- NULL
  current_staff_log <- translate_tbl_column(current_staff_log,ui_elem)
  DT::datatable(current_staff_log, options = list(pageLength = 10),rownames=F)
})
}