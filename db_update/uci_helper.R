uci_load_ui <- function(input, output, ui_list){
  
  if ("uci_data" %in% ui_list){
    output$uci_data <- render_uci_data()
  }
  
  return(output)
}

uci_init <- function(input, output){
  output <- uci_load_ui(input, output, "uci_data")
  return(output)
}

uci_add_customer <- function(input, output){
  
  return(output)
}