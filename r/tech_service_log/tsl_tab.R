# ----------------------------- update_customer_tab ----------------------------
if ('tech_service_log' %in% hidden_tab){
  tech_service_log_tab <- tabPanel(
    get_actual('tech_service_log'))
}else{
  tech_service_log_tab <- tabPanel(
    get_actual('tech_service_log'),
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      box(width = 3, height = 400, style = "background-color:#f5f5f5;",
          htmlOutput("tsl_engineer_name"),
          htmlOutput("tsl_customer_name"),
          htmlOutput("tsl_analyser_name"),
          htmlOutput("tsl_analyser_lot"),
          htmlOutput("tsl_job_desc"),
          htmlOutput("tsl_job_detail"),
      ),
      box(width = 9, height = 800, style = "background-color:#f5f5f5;",
          DT::dataTableOutput("tsl_out_tbl")
      )
    )
  )
}


tsl_render_customer_name <- function(){renderUI({
  customer_name_list <- db_read_query(
    'select distinct customer_info.customer_name from tsl_analyser_list 
    inner join customer_info
    on tsl_analyser_list.customer_id=customer_info.customer_id')$customer_name
  selectizeInput(
    inputId = "tsl_customer_name",
    label = get_actual("customer_name"),
    choices = customer_name_list,
    selected = customer_name_list[1]
    )
})}

tsl_render_analyser_name <- function(input,output){renderUI({

  # get the customer_id from selected customer
  customer_name <- input$tsl_customer_name
  customer_id <- db_read_query(paste0(
    "select distinct tsl_analyser_list.customer_id from tsl_analyser_list 
    inner join customer_info on
    tsl_analyser_list.customer_id = customer_info.customer_id
    where customer_info.customer_name ='",customer_name,"'"))$customer_id
  
  # get the list of analyser matching the customer
  analyser_list <- db_read_query(paste0(
    "select tsl_analyser_list.search_str from tsl_analyser_list 
    where customer_id =",customer_id))$search_str
  print(analyser_list)
  # render the UI
  selectizeInput(
    inputId = "tsl_analyser_name",
    label = get_actual("analyser_name"),
    choices = analyser_list,
    selected = analyser_list[1]
  )
})}