# create all global var
create_global_data <- function(){
  create_pir_data()
}

create_pir_data <- function(){
  pir_data <- data.frame(report_type="", vendor_id=NA)
  pir_data$output_path <- file.path(config$report_out_path,
                             paste0(config$report_name_default,".xlsx"))
  gbl_write_var("pir_data",pir_data)
}

