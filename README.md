## Invenage - Inventory Management
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
The primary GUI is built for warehouse management purpose, its purpose is to write records
when selling goods to customer, and make inventory report for different purposes. 
If you're fluent in R-shiny, making multiple GUIs for different department should be possible.

Warehouse importing is done through the excel POs (proposed order). 

Invenage scan the PO folders and import all items it see a lot written in the excel file.
Since the PO is made during the ordering process already, 
in order to mark an item as "imported" the warehouse manager just need to fill the lot on 
the PO and Invenage will import to its database (python)

Invenage also has capacity to scan a special "local import" PO 
(these has much faster update rate than foreign import) and add to its database (python)

Database update is done through an excel sheet, 
the admin just need to fill the info on the sheet and Invenage will read the sheet, update
its database and clear the sheet ready for next time (python)

Obviously many other tasks is possible through writing R/python scripts. 