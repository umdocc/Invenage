alert_col_notfound <- function(col_name){
  if(config$app_lang=="Vi"){
    shinyalert(uielem$error, 
               paste(uielem$col_notfound,col_name), 
               type = "error")
  }else{
    shinyalert(uielem$error, 
               paste(col_name, uielem$col_notfound), 
               type = "error")
  }
}

alert_ref_notfound <- function(ref_list){
  if(config$app_lang=="Vi"){
    shinyalert(uielem$ref_notfound, 
               paste(ref_list), 
               type = "error")
  }else{
    shinyalert(uielem$ref_notfound, 
               paste(ref_list), 
               type = "error")
  }
}