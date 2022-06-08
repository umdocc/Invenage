# ------------------------------ UI Load & Render ------------------------------
# init function
upi_init <- function(input,output){
  output <- upi_load_ui(input,output,
              c('upi_vendor', "upi_ref_smn", "upi_comm_name",
                "upi_ordering_unit", "upi_prod_type", "upi_default_warehouse",
                "upi_add_pkg_comm_name", "upi_add_pkg_unit",
                "upi_add_unit_spp", "upi_add_pkg_explanation"))
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
  if ("upi_comm_name" %in% ui_list){
    output$upi_comm_name <- render_upi_comm_name(input)
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
  if ("upi_add_pkg_comm_name" %in% ui_list){
    output$upi_add_pkg_comm_name <- render_upi_add_pkg_comm_name()
  }
  if ("upi_add_pkg_unit" %in% ui_list){
    output$upi_add_pkg_unit <- render_upi_add_pkg_unit()
  }
  if ("upi_add_unit_spp" %in% ui_list){
    output$upi_add_unit_spp <- render_upi_add_unit_spp()
  }
  if ("upi_add_pkg_explanation" %in% ui_list){
    output$upi_add_pkg_explanation <- render_upi_add_pkg_explanation(input)
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

render_upi_comm_name <- function(input){renderUI({
  input_ref_smn <- input$upi_ref_smn
  prod_list <- db_read_query(
    paste0("select comm_name from price_list where ref_smn='",
           input_ref_smn,"'"))
  selectizeInput(
    inputId = "upi_comm_name", label = uielem$comm_name,
    choices = prod_list$comm_name, selected = NULL,
    options = list(create=T))
})}

render_upi_ordering_unit <- function(){renderUI({
  unit_choice <- unique(ordering_unit$unit)
  selectizeInput(
    inputId = "upi_ordering_unit", label = uielem$ordering_unit,
    choices = unit_choice, selected = NULL,
    options = list(create=T))
})}

render_upi_prod_type <- function(){renderUI({
  selectizeInput(
    inputId = "upi_prod_type", label = uielem$prod_type,
    choices = product_type$actual, selected = NULL)
})}

render_upi_default_warehouse <- function(){renderUI({
  selectizeInput(
    inputId = "upi_default_warehouse", label = uielem$default_warehouse,
    choices = warehouse_info$warehouse, selected = NULL)
})}

# collection input data and write to global
upi_update_data <- function(input){
  upi_data$vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor==input$upi_vendor]
  upi_data$ref_smn <- input$upi_ref_smn
  upi_data$comm_name <- input$upi_comm_name
  upi_data$ordering_unit <- tolower(input$upi_ordering_unit)
  upi_data$warehouse_id <- warehouse_info$warehouse_id[
    warehouse_info$warehouse==input$upi_default_warehouse]
  upi_data$prod_code <- paste0("VID",upi_data$vendor_id,"_",upi_data$ref_smn)
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

  # compile the line to be added to product_info
  if(error_free){
    append_prod <- upi_data %>% select(prod_code, vendor_id, ref_smn, comm_name)
    append_prod$type <- product_type$prod_type[
        product_type$actual == input$upi_prod_type]
    append_prod$updated_date <- format(Sys.Date())
    append_prod$warehouse_id <- upi_data$warehouse_id
    append_prod$active <- 1
    print(append_prod)
    # compose line to be added to packaging
    append_pkg <- upi_data %>% select(prod_code, unit=ordering_unit)
    append_pkg$units_per_pack <- 1
    append_pkg$last_updated <- format(Sys.Date())
    
    # writing to db
    db_append_tbl('product_info',append_prod)
    db_append_tbl('packaging',append_pkg)
    gbl_load_tbl(c("product_info","packaging"))
    show_success("add_success")
  }

}

render_upi_add_pkg_comm_name <- function(){renderUI({
  
  selectizeInput(
    inputId = "upi_add_pkg_comm_name", label = uielem$comm_name, 
    choices = prod_choices$prod_search_str, 
    selected = prod_choices$prod_search_str[1], 
    options = list(create = F))
})
}

render_upi_add_pkg_unit <- function(){renderUI({
  
  selectizeInput(
    inputId = "upi_add_pkg_unit", label = uielem$unit, 
    choices = unique(packaging$unit), 
    selected = unique(packaging$unit)[1], 
    options = list(create = T))
})
}

render_upi_add_unit_spp <- function(){renderUI({
  
  selectizeInput(
    inputId = "upi_add_unit_spp", label = uielem$qty, 
    choices = 1:10000, 
    selected = 1, 
    options = list(create = T))
})
}

render_upi_add_pkg_explanation <- function(input){renderUI({
  upi_spp <- input$upi_add_unit_spp
  upi_unit <- input$upi_add_pkg_unit
  upi_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str==input$upi_add_pkg_comm_name]
  upi_ordering_unit <- ordering_unit$unit[
    ordering_unit$prod_code==upi_prod_code]
  HTML(paste(uielem$add_pkg,
             upi_spp, upi_unit, "/", upi_ordering_unit,
             input$upi_add_pkg_comm_name))
})
}

upi_append_pkg <- function(input, output){
  upi_prod_code <- prod_choices$prod_code[
    prod_choices$prod_search_str==input$upi_add_pkg_comm_name]
  append_pkg <- data.frame(
    unit = input$upi_add_pkg_unit,
    units_per_pack = input$upi_add_unit_spp,
    prod_code = upi_prod_code,
    last_updated = Sys.Date(),
    ordering_unit = 0
  )
  
  db_append_tbl("packaging",append_pkg)
  load_tbl_and_clean_duplicated(
    "packaging", c("unit", "prod_code"))
  
  return(output)
}