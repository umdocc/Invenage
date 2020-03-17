# Invenage server.R
source("global.R",local = F)
require(dplyr)
require(DT)
shinyServer(function(input, output,session) {
  session$onSessionEnded( function(){
    stopApp()
  }) # quit on session end 
  # ------------------------------- lookup UI ----------------------------------
  output$lookup_tbl_output <- DT::renderDataTable({
    table_name <- ui_elem$label[
      ui_elem$actual==input$lu_tbl_selector]
    create_lookup_tbl(table_name,config_dict)
  },rownames=F)
  observeEvent(input$print_lu_tbl,{
    table_name <- ui_elem$label[
      ui_elem$actual==input$lu_tbl_selector]
    lu_tbl_out <- create_lookup_tbl(table_name,config_dict)
    dest_path <- file.path(app_path,'lu_tbl.xlsx')
    write.xlsx(lu_tbl_out, dest_path,row.names=F)
    system2('open',dest_path,timeout = 2)
  })
  # ----------------------------- report UI ------------------------------------
  
  output$report_tbl_ouput <- DT::renderDataTable({
    report_type <- ui_elem$label[ui_elem$actual==input$report_type]
    rp_filename <- get_rp_filename(report_type, config_dict)
    create_report(report_type,rp_filename,config_dict,input)
  },rownames=F)
  
  # create the report and open it
  observeEvent(input$printReport, {
    # gather all data
    report_type <- ui_elem$label[ui_elem$actual==input$report_type]
    rp_data <- build_rp_data(report_type,input)
    to_date <- input$to_date
    
    # #debug
    # print(report_type);print(rp_data)
    
    #from_date and to_date depends on rp type
    if (report_type == 'sale_profit_report'){
      from_date <- input$from_date 
    }else{
      from_date <- strftime(Sys.Date())
    }
    if (report_type == 'inv_value_report'){
      rp_filename <- write_inv_value_rp()
    }else{
      rp_filename <- write_report_data(report_type, rp_data, from_date, to_date)
    }
    system(paste0('open ','"',rp_filename,'"'))
  })

})