# ------------------------------ UI Load & Render ------------------------------
# init function
uvi_init <- function(input,output){
  output <- uvi_load_ui(
    input,output,
    c('uvi_vendor', "uvi_vendor_orig", "uvi_vendor_local"))
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
  if ("uvi_vendor_local" %in% ui_list){
    output$uvi_vendor_local <- render_uvi_vendor_local()
  }
  return(output)
}

render_uvi_vendor <- function(){renderUI({
  choice_list <- vendor_info$vendor
  selectizeInput(
    inputId = "uvi_vendor", label = uielem$vendor,
    choices = choice_list, selected = choice_list[1],
    options = list(create = T))
})}

render_uvi_vendor_orig <- function(){renderUI({
  radioButtons(
    inputId = "uvi_vendor_orig", label = NULL,
    choices = c(uielem$vendor, uielem$distributor),
    selected = uielem$vendor,
    inline = T,
  )
})}

render_uvi_vendor_local <- function(){renderUI({
  radioButtons(
    inputId = "uvi_vendor_local", label = NULL,
    choices = c(uielem$local, uielem$oversea),
    selected = uielem$local,
    inline = T,
  )
})}

uvi_add_vendor <- function(input,output){
  # check if the vendor name exists
  test_df <- vendor_info[vendor_info$vendor==input$uvi_vendor,]
  if(nrow(test_df)>0){
    show_error("data_exist")
  }else{
    append_vendor <- data.frame(
      vendor = input$uvi_vendor, orig_vendor = T, local = T)
    
    if(input$uvi_vendor_orig!=uielem$vendor){
      append_vendor$orig_vendor <- F
    }
    if(input$uvi_vendor_local!=uielem$local){
      append_vendor$local <- F
    }
    db_append_tbl("vendor_info",append_vendor)
    # build the vendor code
    new_vid <- db_read_query(paste0(
      "select * from vendor_info where vendor='",
      append_vendor$vendor,"'"))$vendor_id
    update_query <- paste0("update vendor_info set vendor_code=concat('",
                  uvi_vendor_code_prefix,"',lpad(",new_vid)
  }
}