# this function check the local list of po, compare to list from remote database
get_po2sync <- function(){
  po_file_list <- get_local_po_list()
  po_file_list <- merge(po_info,po_file_list) %>% filter(completed==0)
  po2sync <- po_file_list$po_name
  return(po2sync)
}