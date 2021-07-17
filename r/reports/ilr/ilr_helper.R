# the create function get the raw report and do all the formatting
# if print_report=T an excel/word report will be generated
create_import_log_report <- function(input,print_report=F,trans_col=T){
  from_date <- input$ilr_from_date
  to_date <- input$ilr_to_date
  import_log_report <- get_import_log_report(from_date,to_date)

  if(!as.numeric(config$ilr_show_price)){
    import_log_report$actual_unit_cost <- NULL
    }

  if(trans_col){
    import_log_report <- translate_tbl_column(import_log_report, ui_elem)
  }

  if(print_report){
    write_and_open_report(import_log_report)
  }

  return(import_log_report)
}

# get function generate raw report for create function
get_import_log_report <- function(from_date,to_date){
  import_log_report <- db_read_query(paste0(
    "SELECT product_info.comm_name, import_log.unit,
    import_log.actual_unit_cost, import_log.qty, import_log.lot,
    import_log.exp_date, import_log.in_invoice_num, import_log.delivery_date
    FROM import_log inner join product_info
    on import_log.prod_code = product_info.prod_code
    where import_log.delivery_date between '",from_date,"' and '",
    to_date,"' order by import_log.delivery_date asc, import_log.id asc"))
  return(import_log_report)
}
 
# render display data
render_import_log_report_tbl <- function(input){DT::renderDataTable({
  output_tbl <- create_import_log_report(input)
  DT::datatable(output_tbl, options = list(pageLength = 10),rownames=F)
})
}