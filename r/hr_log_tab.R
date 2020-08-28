# ui renderer for hr_log tab
# ------------------------------- shiny ui object --------------------------------
hr_log_tab <- tabPanel(
  ui_elem$actual[ui_elem$label=='hr_log'],
  fluidRow(
    style = "background-color:#f5f5f5;",
    box(width = 3, height = 800,
        htmlOutput('admin_name'),
        dateInput('hrl_log_date', label=get_actual('entry_date')),
        htmlOutput('hour_logged'),
        htmlOutput('task_desc'),
        actionButton('task_input',
                     ui_elem$actual[ui_elem$label=='enter_data'])
    ),
    box(width = 9, height = 800,
        DT::dataTableOutput('admin_activity_log'),
        p(),
        div(style="display: inline-block;padding-top:2px;;width: 200px",
            dateInput('hrl_del_log_date', label=get_actual('entry_date'))),
        div(style="display: inline-block;padding-top:2px;;width: 100px",
            htmlOutput('hrl_del_stt')),
        div(style="display: inline-block;padding-top:2px;;width: 200px",
            actionButton('del_hrl_entry', label=get_actual('delete_stt')
        ))
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
    activity_date = input$hrl_log_date,
    hour_logged = input$hour_logged,
    detail = input$task_desc,
    stt = get_new_hrl_stt(input,stt_type='hrl_new')
  )
  # print(activity_data)
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
  
  # select column,sort, translate
  current_staff_log <- current_staff_log %>% 
    arrange(desc(activity_date),desc(stt))
  current_staff_log <- current_staff_log %>%
    select(stt,activity_date,detail,hour_logged,admin_name)
  current_staff_log <- translate_tbl_column(current_staff_log,ui_elem)
  
  DT::datatable(current_staff_log, options = list(pageLength = 10),rownames=F)
})
}

# delete an entry from activity log
del_hrl_stt <- function(input){
  stt_to_del <- input$hrl_del_stt
  # only do something if stt is not blank
  if(stt_to_del!=''){
    date_to_del <- input$hrl_del_log_date
    admin_to_del <- staff_info$admin_id[staff_info$admin_name==input$admin_name]
    query <- paste0("delete from staff_activity_log where activity_date='",
                    date_to_del,"' and stt=",stt_to_del," and admin_id=",
                    admin_to_del)
    print(query)
    db_exec_query(query)
    reload_tbl(config_dict,'staff_activity_log')
  }
}