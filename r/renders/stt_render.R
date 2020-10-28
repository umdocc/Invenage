# render stt, used for delete entry purpose

render_stt <- function(input, iid){renderUI({
  if(iid=='hrl_del_stt'){
    selected_date <- input$hrl_del_log_date
    current_data <- db_read_query(
      paste0("select * from staff_activity_log where admin_id=",admin_id,
             " and activity_date='",selected_date,"'"))
    box_choices <- current_data$stt
    box_selected <- box_choices[1]
    box_label <- NULL
  }
  selectInput(iid, label = box_label, 
              choices = box_choices, selected = box_selected)
})}

# function to get new hrl_stt
get_new_hrl_stt <- function(input,stt_type){
# if request a new stt for hrl
  if(stt_type=='hrl_new'){
  selected_date <- input$hrl_log_date
  current_data <- db_read_query(
    paste0("select * from staff_activity_log where admin_id=",admin_id,
           " and activity_date='",selected_date,"'"))
  current_stt_list <- current_data$stt
  # print(current_stt_list)
  new_stt <- get_new_stt(current_stt_list)
  }
  return(new_stt)
}

# give a list of stt and generate a new stt bases on algorithm
# 'fill' wil fill any missing position in tthe stt list
get_new_stt <- function(stt_list,algorithm='fill'){
  if (length(stt_list)>0){
    stt_list <- as.integer(stt_list)
    if(any(is.na(stt_list))){
      stop('list of stt is not all integer')
    }
    if(algorithm == 'fill'){
      new_stt <- max(stt_list)+1
      for (i in 1:max(stt_list)){
        if (!(i %in% stt_list)){
          new_stt <- i
          break
        }
      }
    }
  }else{
    new_stt <- 1
  }
  return(new_stt)
}