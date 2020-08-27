# render stt, used for delete entry purpose

render_stt <- function(input, iid){renderUI({
  if(iid=='hrl_del_stt'){
    selected_date <- input$hrl_log_date
    current_data <- db_read_query(
      paste0("select * from staff_activity_log where admin_id=",admin_id,
             " and activity_date='",selected_date,"'"))
    hrl_avail_stt <- current_data$stt
    selectInput(iid,
                label=get_actual('stt'),
                choices = hrl_avail_stt,
                selected = hrl_avail_stt[1])
  }  
})}
