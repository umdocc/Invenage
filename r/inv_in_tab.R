#### tab file deals with ui renderer
# render table for the inv_in tab

render_import_tbl <- function(){DT::renderDataTable({
  import_log_tbl <-merge(
    import_log, product_info %>% select(prod_code,comm_name), all.x=T) 
  import_log_tbl <- import_log_tbl[order(import_log_tbl$id, decreasing = T),]
  import_log_tbl <- import_log_tbl %>% 
    select(comm_name, po_name, qty, unit, lot, exp_date, actual_unit_cost, note)
  output <- translate_tbl_column(import_log_tbl, ui_elem)
  DT::datatable(output, options = list(pageLength = 10),rownames=F)
})
}