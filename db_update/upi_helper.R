# ------------------------------ UI Load & Render ------------------------------
# function to reload additional ui
upi_load_ui <- function(input,output,ui_list){
  if ("upi_vendor" %in% ui_list){
    output$upi_vendor <- render_upi_vendor()
  }
  return(output)
}
# 
udp_render_vendor <- function(){renderUI({
  vendor_list <- vendor_info$vendor

  selectizeInput(
    inputId = "upi_vendor",
    label = uielem$vendor,
    choices = vendor_info$vendor,
    selected = vendor_info$vendor[1])
})}
# 
# # add product button handler
# udp_add_product <- function(input,output){
#   # collect variables
#   input_vendor_name <- input$udp_vendor
#   input_ref_smn <- input$udp_ref_smn
#   input_prod_code <- input$udp_prod_code
#   
#   # if the vendor not exist yet, add and display a notice
#   if(!check_vendor_exist(input_vendor_name)){
#     add_vendor_with_product(input_vendor_name)
#     show_alert("notice","new_vendor_added","info")
#   }
#   # pulling information from ui
#   prod_vendor_id <- vendor_info$vendor_id[
#     vendor_info$vendor==input_vendor_name]
#   
#   # check if a product exist
#   if(!(check_product_exist(prod_vendor_id,input_ref_smn))){
#     
#     # check if the prod_code exist
#     if(!(check_prod_code_exist(input_prod_code))){
#       
#       # compile the line to be added to product_info
#       append_prod <- data.frame( 
#         prod_code = input_prod_code,
#         comm_name = input$udp_comm_name,
#         vendor_id = prod_vendor_id, 
#         ref_smn = input_ref_smn,
#         type = product_type$prod_type[
#           product_type$actual == input$add_prod_type],
#         updated_date = format(Sys.Date()),
#         warehouse_id = warehouse_info$warehouse_id[
#           warehouse_info$warehouse==input$add_warehouse],
#         active = 1)
#       
#       
#       # compose line to be added to packaging
#       append_pkg <- data.frame(
#         prod_code = input$udp_prod_code, 
#         unit = tolower(input$udp_ordering_unit),
#         units_per_pack = 1, 
#         last_updated = format(Sys.Date())
#       )
#       
#       
#       # check if anything missing
#       if(check_blank(append_prod) | check_blank(append_pkg)){
#         big_msg <- uielem$input_error
#         small_msg <- uielem$missing_fields
#         shinyalert(title = big_msg, text = small_msg, type = "error")
#       }else{
#         append_tbl_rld(config_dict, 'product_info',append_prod)
#         append_tbl_rld(config_dict, 'packaging',append_pkg)
#         
#         big_msg <- ui_elem$actual[ui_elem$label=='done']
#         small_msg <- ui_elem$actual[ui_elem$label=='add_prod_success']
#         shinyalert(title = big_msg, text = small_msg, type = "success")
#       }
#     }
#   }
# }
# 
# # add a vendor when adding a new product, config all parameters automatically
# add_vendor_with_product <- function(input_vendor_name){
#   if(input_vendor_name!=''&!is.na(input_vendor_name)){
#     append_vendor_info <- data.frame(
#       vendor = input_vendor_name, 
#       local=0,
#       orig_vendor = 1)
#     append_tbl_rld(config_dict,'vendor_info',append_vendor_info)
#     
#     if(as.integer(config$add_vendor_code)){
#       add_vendor_code(input_vendor_name)
#     }
#   }else{
#     show_alert("error","empty_vendor_name")
#   }
#   
# }
# 
# add_vendor_code <- function(vendor_name){
#   new_vendor_id <- db_read_query(paste0(
#     "select vendor_id from vendor_info where vendor='",
#     vendor_name,"'"))$vendor_id
#   new_vendor_code <- paste0(
#     config$vendor_code_prefix,
#     formatC(as.integer(new_vendor_id),
#             width=as.integer(config$vendor_code_width), 
#             flag="0"))
#   
#   db_exec_query(paste0(
#     "update vendor_info info set vendor_code='",
#     new_vendor_code,"' where vendor_id=",new_vendor_id))
# }