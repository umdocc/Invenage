# functions to check various error during operation
# and display message instead of getting blank screen
# these return a boolean error_free variable

# check for all problems when we print the pxk
check_print_pxk <- function(input){
  error_free <- T #init
  
  # check if the pxk_form exists
  orig_form_path <- file.path(config$form_path, "pxk_form.xlsx")
  if(!file.exists(orig_form_path)){
    show_alert("error","pxk_form_notfound","error")
    error_free <- F
  }
  
  return(error_free) #return
}