# ------------------------------ UI Load & Render ------------------------------
# init function
uvi_init <- function(input,output){
  output <- uvi_load_ui(
    input,output,
    c('uvi_vendor', "uvi_vendor_orig"))
  return(output)
}

# function to reload additional ui
uvi_load_ui <- function(input,output,ui_list){
  if ("uvi_vendor" %in% ui_list){
    output$uvi_vendor <- render_uvi_vendor()
  }
  if ("uvi_vendor_orig" %in% ui_list){
    output$uvi_vendor_orig <- render_uvi_vendor_orig()
  }
  return(output)
}

render_uvi_vendor <- function(){renderUI({
  selectizeInput(
    inputId = "uvi_vendor", label = uielem$vendor,
    choices = NULL, selected = NULL)
})}

render_uvi_vendor_orig <- function(){renderUI({
  radioButtons(
    inputId = "uvi_vendor_orig", label = NULL,
    choices = c(uielem$vendor, uielem$distributor),
    selected = uielem$vendor,
    inline = T,
  )
})}

uvi_add_vendor <- function(input,output){
  print(input$uvi_vendor_orig)
}