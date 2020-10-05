# helper provide additional resources and non-render function for the tab
# -------------------------------- Resources -----------------------------------
# load additional tables
reload_tbl(config_dict,c('tsl_analyser_list','tsl_service_type'))


# -------------------------------- UI Loader -----------------------------------
# function to reload additional ui
tsl_load_ui <- function(input,output,ui_list){
  if ('tsl_engineer_name' %in% ui_list){
    output$tsl_engineer_name <- tsl_render_engineer_name()
  }
  
  if ('tsl_customer_name' %in% ui_list){
    output$tsl_customer_name <- tsl_render_customer_name()
  }
  
  if ('tsl_analyser_name' %in% ui_list){
    output$tsl_analyser_name <- tsl_render_analyser_name(input,output)
  }
  
  if ('tsl_analyser_lot' %in% ui_list){
    output$tsl_analyser_lot <- tsl_render_analyser_lot(input,output)
  }
  
  if ('tsl_service_type' %in% ui_list){
    output$tsl_service_type <- tsl_render_service_type()
  }
  
  if ('tsl_service_log' %in% ui_list){
    output$tsl_service_log <- tsl_render_service_log()
  }

  return(output)
}

# -------------------------- Button functions ----------------------------------
exec_tsl_add_entry <- function(input,output){
  # query dependant variables
  current_customer_id <- db_read_query(paste0(
    "select distinct tsl_analyser_list.customer_id from tsl_analyser_list 
    inner join customer_info on
    tsl_analyser_list.customer_id = customer_info.customer_id
    where customer_info.customer_name ='",
    input$tsl_customer_name,"'"))$customer_id
  analyser_prodcode <- db_read_query(paste0(
    "select product_info.prod_code from product_info where comm_name='",
    input$tsl_analyser_name,"'"))$prod_code
  current_admin_id <- db_read_query(paste0(
    "select staff_info.admin_id from staff_info where admin_name='",
    input$tsl_engineer_name,"'"))$admin_id
  current_lot <- db_read_query(paste0(
    "select tsl_analyser_list.lot from tsl_analyser_list 
    where customer_id=",current_customer_id," and prod_code='",
    analyser_prodcode,"' and lot ='",input$tsl_analyser_lot,"'"))$lot
  
  #encode service tpye
  service_type <- data.frame(
    label = unlist(strsplit(config$tsl_input_service_type,";")))
  service_type <- merge(service_type,ui_elem)
  service_type <- merge(service_type,tsl_service_type)
  
  
  #construct the data frame
  tsl_append_log <- data.frame(
  admin_id = current_admin_id,
  customer_id = current_customer_id,
  prod_code = analyser_prodcode,
  lot = current_lot,
  service_date = input$tsl_service_date,
  service_type = 
    service_type$service_type[service_type$actual==input$tsl_service_type],
  service_detail = input$tsl_service_detail
  )
  # print(tsl_append_log)
  append_tbl_rld(config_dict,'tsl_service_log',tsl_append_log)
  show_alert("success","success","success")
  tsl_load_ui(input,output,
            c("tsl_customer_name","tsl_analyser_name",
              "tsl_analyser_lot","tsl_service_log"))
  
}