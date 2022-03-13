# ------------------------------ UI Load & Render ------------------------------
# init function
upi_init <- function(input,output){
  output <- upi_load_ui(input,output,
              c('upi_vendor', "upi_ref_smn", "upi_prod_code", "upi_comm_name",
                "upi_ordering_unit", "upi_prod_type", "upi_default_warehouse"))
  upi_data <- data.frame(vendor_id=0)
  gbl_write_var("upi_data",upi_data)
  return(output)
}

# function to reload additional ui
upi_load_ui <- function(input,output,ui_list){
  if ("upi_vendor" %in% ui_list){
    output$upi_vendor <- render_upi_vendor()
  }
  if ("upi_ref_smn" %in% ui_list){
    output$upi_ref_smn <- render_upi_ref_smn()
  }
  if ("upi_prod_code" %in% ui_list){
    output$upi_prod_code <- render_upi_prod_code()
  }
  if ("upi_comm_name" %in% ui_list){
    output$upi_comm_name <- render_upi_comm_name()
  }
  if ("upi_ordering_unit" %in% ui_list){
    output$upi_ordering_unit <- render_upi_ordering_unit()
  }
  if ("upi_prod_type" %in% ui_list){
    output$upi_prod_type <- render_upi_prod_type()
  }
  if ("upi_default_warehouse" %in% ui_list){
    output$upi_default_warehouse <- render_upi_default_warehouse()
  }
  return(output)
}

render_upi_vendor <- function(){renderUI({
  selectizeInput(
    inputId = "upi_vendor", label = uielem$vendor,
    choices = vendor_info$vendor, selected = vendor_info$vendor[1])
})}

render_upi_ref_smn <- function(){renderUI({
  selectizeInput(
    inputId = "upi_ref_smn", label = uielem$ref_smn,
    choices = NULL, selected = NULL,
    options = list(create=T))
})}

render_upi_prod_code <- function(){renderUI({
  selectizeInput(
    inputId = "upi_prod_code", label = uielem$prod_code,
    choices = NULL, selected = NULL,
    options = list(create=T))
})}

render_upi_comm_name <- function(){renderUI({
  selectizeInput(
    inputId = "upi_comm_name", label = uielem$comm_name,
    choices = NULL, selected = NULL,
    options = list(create=T))
})}

render_upi_ordering_unit <- function(){renderUI({
  selectizeInput(
    inputId = "upi_ordering_unit", label = uielem$ordering_unit,
    choices = NULL, selected = NULL,
    options = list(create=T))
})}

render_upi_prod_type <- function(){renderUI({
  selectizeInput(
    inputId = "upi_prod_type", label = uielem$prod_type,
    choices = NULL, selected = NULL)
})}

render_upi_default_warehouse <- function(){renderUI({
  selectizeInput(
    inputId = "upi_default_warehouse", label = uielem$default_warehouse,
    choices = NULL, selected = NULL)
})}

# collection input data and write to global
upi_update_data <- function(input){
  upi_data$vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor==input$upi_vendor]
  upi_data$ref_smn <- input$upi_ref_smn
  upi_data$prod_code <- input$upi_prod_code
  upi_data$comm_name <- input$upi_comm_name
  upi_data$ordering_unit <- tolower(input$upi_ordering_unit)
  gbl_write_var("upi_data",upi_data)
}

# add product button handler
upi_add_product <- function(input,output){
  
  upi_update_data(input) #collect data
  
  # check if a ref or prod_code exist
  tmp <- product_info[product_info$vendor_id==upi_data$vendor_id & 
                        product_info$ref_smn == upi_data$ref_smn,]
  if(nrow(tmp)>0){
    show_error("ref_exist",upi_data$ref_smn)
  }
  tmp <- product_info[product_info$prod_code==upi_data$prod_code,]
  if(nrow(tmp)>0){
    show_error("prod_code_exist",upi_data$prod_code)
  }

  # compile the line to be added to product_info
  if(error_free){
    append_prod <- upi_data %>% select(vendor_id, ref_smn, prod_code, comm_name)
    append_prod$type <- product_type$prod_type[
        product_type$actual == input$upi_prod_type]
    append_prod$updated_date <- format(Sys.Date())
    append_prod$warehouse_id <- warehouse_info$warehouse_id[
        warehouse_info$warehouse==input$upi_default_warehouse]
    append_prod$active <- 1
  
    # compose line to be added to packaging
    append_pkg <- upi_data %>% select(prod_code, unit=ordering_unit)
    append_pkg$units_per_pack <- 1
    append_pkg$last_updated <- format(Sys.Date())
    
    # writing to db
    db_append_tbl('product_info',append_prod)
    db_append_tbl('packaging',append_pkg)
    show_success()
  }

}
