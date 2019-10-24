# po_tools is a single file to do certain task for each PO
# it can add price to PO, or check exp date
# it is specific to each PO
# --------------------- Control Variables -------------------------------------
po_name = 'MDS.DiaSorinSpA.PO.211019.xlsx'
exp_filename = 'expiry dates wk 43 less than 6 months.xlsx'

# ---------------------- Setup Block ------------------------------------------
import sys, os, dateutil, pandas as pd, datetime
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
# add pythonPath and import all functions
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv



# ------------------------------ Data Read ------------------------------------
#database
conn = inv.db_open(config_dict)
import_log = pd.read_sql_query('select * from import_log',conn)
product_info = pd.read_sql_query('select * from product_info',conn)
conn.close()

#paths
po_path = config_dict['po_path']
po_list = inv.get_files_info(po_path,'xlsx',['Unused','Draft'])
po_file = po_list[po_list.full_path.str.contains(po_name)].reset_index(
        drop=True).full_path[0]
po_dir = os.path.dirname(po_file)

# logs, dicts
error_file = config_dict['error_log']
msg_dict = inv.create_dict(config_dict,'msg_dict')
rename_dict = pd.read_excel(os.path.join(po_dir,'tools','rename_dict.xlsx'))
rename_dict = inv.convert_to_dict(rename_dict)

# excel data
po_data = inv.read_po_data(po_file,config_dict)
exp_data = pd.read_excel(
        os.path.join(po_dir,'expiry dates wk 43 less than 6 months.xlsx'))
exp_data = exp_data.rename(columns = rename_dict)

# --------------------------- data cleaning -----------------------------------
# ensure the ref_smn is a string
exp_data.ref_smn = exp_data.ref_smn.astype(str).str.strip()
po_data.ref_smn = po_data.ref_smn.astype(str).str.strip()

# clear the name on exp_data, exp_date on po_data
if 'name' in exp_data.columns:
    exp_data = exp_data.drop(columns='name')

# attach shelf_life_date to po_data and clean exp_date
po_data = pd.merge(po_data,exp_data,how='left')
po_data.shelf_life_date = po_data.shelf_life_date.str.replace('\.','-')
po_data.shelf_life_date = po_data.shelf_life_date.astype(str)

# make the 'nan' date as 07-07-9999, then parse
po_data.shelf_life_date[po_data.shelf_life_date=='nan'] = '07-07-2099'
po_data.shelf_life_date = po_data.shelf_life_date.apply(dateutil.parser.parse)

now = datetime.datetime.now()
po_data['remaining_shelf_days']= (po_data.shelf_life_date - now).dt.days
po_data['remarks'] = 'date_ok'
po_data.loc[po_data.remaining_shelf_days<=183,'remarks'] = 'check_exp_date'

# --------------------------- creating report ---------------------------------
po_data.shelf_life_date = po_data.shelf_life_date.dt.strftime('%d-%m-%Y')
output_file_name = os.path.join(po_dir,'po_data_report.xlsx')
po_data.to_excel(output_file_name,index=False)

## split po_data into 2 parts
## those with Lot gets written into importLog
#append_log = po_data.copy()
#append_log = append_log[(append_log.lot!='') & append_log.lot.notnull()]
#
## check to see if entries in append_log exist in import_log
#append_log = inv.check_exists(append_log,import_log,
#                       ['prod_code','lot','po_name','qty'])
#
## only keep entries not exist in import_log
#append_log = append_log[append_log.exist.isnull()]
#
##at this stage we leave actual import cost blank
#append_log.actual_unit_cost = pd.to_numeric(append_log.actual_unit_cost,
#                                            errors='coerce')
#append_log['actual_currency_code'] = 1
#
#
## check that Unit is lower case
#append_log.unit = append_log.unit.str.lower()
#
## add deliveryDate
#append_log['delivery_date'] = datetime.date.today().strftime('%d%m%y')
## if there is a warehouse_id ,use it, if not, create from product_info
#if ('warehouse_id' not in append_log.columns):
#    append_log = append_log.merge(
#            product_info[['prod_code', 'warehouse_id']], how='left')
#
## remove invalid warehouse_id
#append_log = append_log[(append_log.warehouse_id.notnull()) & 
#                        (append_log.warehouse_id != '')]
#
## clean up
#append_log.exp_date = append_log.exp_date.str.replace(' .*$','')
#append_log = append_log[
#        ['prod_code', 'unit', 'qty', 'po_name', 'lot', 'exp_date',
#         'actual_unit_cost', 'actual_currency_code', 'delivery_date',
#         'warehouse_id']]
#
## append to _database
#if len(append_log)>0:
#    print('appending to database:')
#    print(append_log)
#    conn = inv.db_open(config_dict)
#    append_log.to_sql('import_log',conn,index=False,if_exists='append')
#    conn.commit()
#    conn.close()
#
## the remaining of po_data gets written to coming_list
#coming_list = po_data[po_data.lot=='']
#coming_list = coming_list[['name','ref_smn','vendor','qty','po_name']]
#conn = inv.db_open(config_dict)
#coming_list.to_sql('coming_list',conn,index=False,if_exists='replace')
#conn.commit()
#conn.close()

