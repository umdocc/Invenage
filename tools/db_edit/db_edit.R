# db_edit use real sql command to edit the database
# for integrity and ease of use, db editing will be submitted using
# excel request form

# -------------------- sys init from local config ---------------------------
# run sys_init
local_config_path <- file.path(path.expand("~/"),"appData","invenage","config.tsv")
local_config <- read.table(local_config_path, header = T)


# source boot helper for simple init
source(file.path(local_config$value[local_config$name=="app_path"], "boot",
                 'sys_init.R'))


# ----------------- Main ----------------------------------
edit_req <- read.xlsx("")