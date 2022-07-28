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

cpr_get_data <- function(req_customer_id, hide_raw_info=T, trans_col = T){
  
  # get the sale log then filter by customer_id
  report_data <- sale_log
  if(req_customer_id!=0){
  report_data <- report_data[report_data$customer_id==req_customer_id,]
  }
  
  report_data <- report_data %>%
    filter(promotion_price!=1) %>% group_by(prod_code, unit, customer_id) %>% 
    mutate(latest_sale=max(sale_datetime)) %>% 
    filter(sale_datetime==latest_sale) %>% 
    select(customer_id, prod_code, unit, latest_unit_price = unit_price) %>%
    filter(!is.na(latest_unit_price)) %>%
    filter(latest_unit_price >=0)
  
  # add info and clean up
  report_data <- add_product_info(report_data)
  report_data <- add_customer_info(report_data)
  report_data <- report_data %>% 
    select(customer_id, customer_name, prod_code, comm_name, ref_smn, unit, 
           latest_unit_price)
  if(hide_raw_info){
    report_data <- report_data %>% select(-c("prod_code","customer_id"))
  }
  if(trans_col){
    report_data <- translate_tbl_column(report_data)
  }
  
  return(report_data)
}

cpr_get_cid <- function(cpr_name){
  
  # get the customer_id
  cpr_cid <- customer_info$customer_id[
    customer_info$customer_name==cpr_name]
  if(cpr_name==uielem$all){
    cpr_cid <- 0
  }
  
  return(cpr_cid)
}

cpr_create_report <- function(input){
  
  cpr_cid <- cpr_get_cid(input$cpr_customer)
  
  # generate and write the report
  report_data <- cpr_get_data(cpr_cid)
  dest_path <- file.path(config$report_out_path,"invenage_report.xlsx")
  write.xlsx(report_data, dest_path)
  open_location(dest_path)
}