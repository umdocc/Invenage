# the create function get the raw report and do all the formatting
# if print_report=T an excel/word report will be generated 
create_sale_log_report <- function(input,print_report=F,trans_col=T){
  from_date <- input$slr_from_date
  to_date <- input$slr_to_date
  sale_log_report <- get_sale_log_report(from_date,to_date)
  
  if(!as.numeric(config$slr_show_price)){
    sale_log_report$unit_price <- NULL
    }
  
  if(trans_col){
    sale_log_report <- translate_tbl_column(sale_log_report, ui_elem)
  }
  
  if(print_report){
    write_and_open_report(sale_log_report)
  }

  return(sale_log_report)
}

# get function generate raw report for create function
get_sale_log_report <- function(from_date,to_date){
  sale_log_report <- db_read_query(paste0(
    "SELECT sale_log.stt, product_info.comm_name, sale_log.pxk_num, 
    sale_log.unit_price, sale_log.qty, sale_log.lot,
    customer_info.customer_name
    FROM sale_log inner join pxk_info
    on sale_log.pxk_num = pxk_info.pxk_num
    inner join product_info 
    on sale_log.prod_code = product_info.prod_code
    inner join customer_info
    on pxk_info.customer_id = customer_info.customer_id
    where pxk_info.sale_datetime between '",from_date,"' and '",
    to_date+1,"' 
    order by pxk_info.sale_datetime asc, sale_log.stt asc"))
  return(sale_log_report)
}

# render display data
render_sale_log_report_tbl <- function(input){DT::renderDataTable({
  output_tbl <- create_sale_log_report(input)
  DT::datatable(output_tbl, options = list(pageLength = 10),rownames=F)
})
}