render_hour_logged <- function(){renderUI({
  # use lookup mode if supported
  hour_table <- guess_table[guess_table$guess_type=='activity_hour',]
  if (nrow(hour_table)>0){
    hour_choices <- hour_table$input_str
  }else{
    hour_choices <- seq(0.25,8,0.25)
  }
  hour_selected <- hour_choices[1]
  
  selectizeInput('hour_logged', label = get_actual('hour_logged'),
                 multiple = T, choices = hour_choices,
                 selected = hour_selected)
}) }