# simply call this when error_free = F
show_error <- function(type="unit_notfound",var_list=""){
  error_msg <- paste(uielem[,type],var_list)
  shinyalert(uielem$error, error_msg, type = "error")
}

show_success <- function(type="add_success",var_list=""){
  success_msg <- paste(uielem[,type],var_list)
  shinyalert(uielem$success, success_msg, type = "success")
}