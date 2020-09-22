# render a list of active machine from tsl module
render_analyser_list <- function(input,iid,active=T){renderUI({
  reload_tbl(config_dict,"tsl_analyser_list")
  analyser_list <- tsl_analyser_list
  if(active){
    analyser_list <- analyser_list[analyser_list$active==1,]
  }
  analyser_list <- analyser_list[!is.na(analyser_list$lot)&
                                   analyser_list$lot!='',]
  selectizeInput(inputId = iid,
                 label = get_actual("analyser_list"),
                 choices=analyser_list$search_str)
}) }