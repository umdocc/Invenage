# each tab helper should have load_ui, load_data and init
cpr_load_ui <- function(input,output,ui_list){
  if('cpr_customer' %in% ui_list){
    output$cpr_customer <- render_cpr_customer(input)
  }
  if('cpr_data' %in% ui_list){
    output$cpr_data <- render_cpr_data(input)
  }
  
  return(output)
}

# used to load cdn data into memory
cpr_load_data <- function(input){
  # assign("current_pxk",get_current_pxk(input),envir=globalenv())
  # assign("current_pxk_data",get_pxk_data(current_pxk$pxk_num),
  #        envir=globalenv())
}

cpr_init <- function(input,output){
  
  output <- cpr_load_ui(
    input, output, 
    c("cpr_customer"))
  
  return(output)
}

render_cpr_customer <- function(input){renderUI({
  
  cust_choices <- c(uielem$all,customer_info$customer_name)

  selectizeInput(
    inputId = "cpr_customer", label = uielem$customer_name, 
    choices = cust_choices, selected = cust_choices[1], 
    options = list(create = F))
})
}