## Invenage - Inventory Management 0.1
### Introduction
Invenage is first created as an exercise to automate tasks I routinely do at work. 
It started as a collection of R scripts I wrote to automate many tasks, then a SQLite 
database to keep all those scripts working together, then python scripts to process excel 
inputs (people at my place love excels and I need to help them work more efficiently), 
then a GUI written in R-shiny so that other people can work with the database, and so on...

Invenage is built for companies with business primarily in foreign import, and products
that have clear packaging details (has reference, lot, expiry date, bottles per box etc)....

It is not a complete product, but if you have skills in R and python you may find it usable

### How it works
As of Aug 2020, Invenage works primarily using a shiny ui

### Structure (subject to revision)

#### database
The app can use sqlite or MariaDB. MariaDB is the preferred database method as it makes
managing multiple users a breeze (the app is boring with only 1 user)

#### config_dict
- The config_dict should be split into 2 parts. The local config_dict 
(located in ~/invenage_data) contains just enough information to connect to database
- The config_dict table in the database contains all remaining parameters to customise
the app. The app read data from server, combine with local data and prioritise 
configuration in the following order: local > server config for user > server config for 
all.
- An item can be customsed for a specific user only by setting the admin_in in the 
database config_dict entry. If admin_id = 0 the config is applied to all user.

#### admin1_ui
- The primary gui for invenage, the global.R script job is to locate the actual
application folder where boot.R is located, it then call boot.R to handle everything else
- server.R primary job is to call the appropriate functions for each button in the UI, as
well as reload the UI elements through reload_ui.R. If config
- ui.R is used to call all the corresponding tab, each tab layout should be placed in the
actual R file in the tabs folder

#### r/renders
- each script in renders folder is used to render one type of ui elements in the app.
It will handle all different variations of that ui elements across the app.

#### r/tabs
- each script in tabs folder serve 2 purpose: containing the actual ui layout for 
corresponding tab and containing the button executing function for all the buttons in that
tab as well.

#### r/core
- core contains base.R, boot.R
