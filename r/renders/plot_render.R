render_hrl_plot <- function(input){renderPlot({
  c_admin_id <- staff_info$admin_id[staff_info$admin_name==input$admin_name]
  activity_summary <- staff_activity_log %>% filter(admin_id==c_admin_id) %>%
    group_by(activity_date) %>% 
    summarise(total_hour = sum(hour_logged),.groups='drop')
  ggplot(activity_summary,aes(x=activity_date,y=total_hour))+geom_line()
  })
}