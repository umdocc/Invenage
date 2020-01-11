# source invenage base, used when running as standalone script
# source('~/Documents/GitHub/Invenage/inventory_ui/global.R')

# ------------------------------ Data Read -------------------------------------
conn <- db_open(config_dict)
tmp <- dbGetQuery(conn,'SELECT * FROM INFORMATION_SCHEMA.COLUMNS')
dbDisconnect(conn)
tmp <- tmp[tmp$TABLE_SCHEMA=='invenage',]
tmp <- tmp %>% select(TABLE_NAME,COLUMN_NAME,COLUMN_TYPE)

# check for columns that are not consistent in database
# tmp[!duplicated(tmp %>% select(COLUMN_NAME, COLUMN_TYPE)),]
tmp <- tmp[!duplicated(tmp %>% select(COLUMN_NAME, COLUMN_TYPE)),]

# list the table fields with inconsistencies, we can then search tmp in Rstudio
# for the exact problem
tmp[duplicated(tmp$COLUMN_NAME),]
