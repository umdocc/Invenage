# app.R experimental code for new ui

# the purpose of global is to figure out app_path so that we can load boot.R
rm(list=ls()); options(warn=-1) # boot clean-up
# app_path is one level up from shiny folder
app_path <- dirname(getwd())
assign("app_path", app_path, envir = .GlobalEnv)
boot_path <- file.path(app_path,'r','boot.R')

# if we cannot find boot file, load the default
if(!file.exists(boot_path)){app_path <- '~/Documents/GitHub/Invenage/'}

# load boot
source(file.path(app_path,'r','boot.R'))

# --------------------------- UI layout -----------------------------------
my_ui <- shinyUI({
  navbarPage(
    theme = shinytheme("united"), title = company_name, id = 'main',
      hr_log_tab
  )
})

# ------------------------- server logic -----------------------------------
my_server <- function(input,output,session){
  # quit when closing browser window
  session$onSessionEnded( function(){stopApp()})  
  output <- reload_ui(input,output,
                      c('admin_name','hour_logged','task_desc',
                        'admin_activity_log'))
  
  observeEvent(input$task_input,{
    write_activity_log(input)
    output <- reload_ui(input,output,'admin_activity_log')
  })
}

# start the app
shinyApp(ui = my_ui, server = my_server)