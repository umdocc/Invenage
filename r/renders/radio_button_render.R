# render radio button used for no yes selection
render_radio_button <- function(input,iid,ui_label="", ui_inline=T){renderUI({
  if(iid=="uv_vendor_orig"){
    ui_choices <- unlist(strsplit(config$orig_vendor_noyes_str,split=';'))
    current_vendor_id <- vendor_info$vendor_id[
      vendor_info$vendor==input$uv_vendor]
    
    if(length(current_vendor_id)!=1){
      ui_selected <- ui_choices[as.numeric(config$orig_vendor_noyes_default)]
    }else{
      ui_selected <- ui_choices[vendor_info$orig_vendor[
        vendor_info$vendor_id==current_vendor_id]+1]
    }
  }

  if(iid=="uv_vendor_local"){
    current_vendor_id <- vendor_info$vendor_id[
      vendor_info$vendor==input$uv_vendor]
    ui_choices <- unlist(strsplit(config$local_noyes_str,split=';'))
      
      if(length(current_vendor_id)!=1){
        ui_selected <- ui_choices[as.numeric(config$local_noyes_default)]
      }else{
        ui_selected <- ui_choices[vendor_info$local[
          vendor_info$vendor_id==current_vendor_id]+1]
      }
  }
  
  # render the ui
  radioButtons(inputId = iid, label=ui_label, choices = ui_choices, 
               selected = ui_selected, inline = ui_inline)
})}