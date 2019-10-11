# invenage uses an excel file as primary gui for adding items to database
# this script read the excel file and add items to the database
# it is very important to check for data integrity before adding to db
# ---------------------------- Setup ------------------------------------------
import sys, os, pandas as pd #import
from openpyxl import load_workbook
inv_data_path = os.path.join(os.path.expanduser('~'),'invenage_data') 
sys.path.append(inv_data_path)
#build the config_dict
from python_conf import create_config_dict
config_dict = create_config_dict()
# add pythonPath
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv
app_lang = config_dict['app_lang']
# read the database
conn = inv.db_open(config_dict)
localisation = pd.read_sql_query(
        'select * from localisation where app_lang = "'+app_lang+'"',conn)
max_cust_id = pd.read_sql_query(
        'select max(customer_id) as value from customer_info',conn)
conn.close()
# ------------------------------- Main ----------------------------------------
#update file location
update_file = config_dict['db_update_form']

#manupulate the localisation table to create a actual_col_name_to_label dict

acntl_dict = {}
for i in range(0,len(localisation)):
    acntl_dict[localisation.actual[i]] = localisation.label[i]

# process customer_sheet
customer_sheet_name = config_dict['add_customer_sheetname']
append_cust_info = pd.read_excel(update_file,sheet_name = customer_sheet_name)
append_cust_info = append_cust_info.rename(columns=acntl_dict)
# add customer id
append_cust_info['customer_id'] = max_cust_id.value[0]+1

# process packaging sheet
pkg_sheet_name = config_dict['add_pkg_sheetname']
append_pkg_info = pd.read_excel(update_file,sheet_name = pkg_sheet_name)
append_pkg_info = append_pkg_info.rename(columns=acntl_dict)

# writing to database
conn = inv.db_open(config_dict)
append_cust_info.to_sql('customer_info',conn,index=False,if_exists='append')
conn.close()

# clear the form
wb = load_workbook(filename = update_file)
ws = wb[customer_sheet_name]
for r in range(2,200):
    for c in range (1,30):
        ws.cell(row=r,column=c).value = ''
wb.save(update_file)

