# simply call this when error_free = F
show_error <- function(err_type=uielem$unit_notfound,var_list="",set_error=T){
  error_msg <- paste(err_type,var_list)
  shinyalert(uielem$error, error_msg, type = "error")
  
  # set error_free variable if specified
  if(set_error){
   gbl_set_error_free(F) 
  }
}

show_success <- function(type="add_success",var_list="", timeout=0){
  success_msg <- paste(uielem[,type],var_list)
  shinyalert(uielem$success, success_msg, type = "success", timer=timeout)
}