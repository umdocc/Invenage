# this script handle import tasks when the excel PO is used for import
# ---------------------- Setup Block ------------------------------------------
import sys, os, datetime, pandas as pd
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
# add pythonPath and import all functions
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv

# ------------------------------ Data Read ------------------------------------
#database
db_engine = inv.create_db_engine(config_dict)
conn = inv.db_open(config_dict,db_engine)
import_log = pd.read_sql_query('select * from import_log',conn)
product_info = pd.read_sql_query('select * from product_info',conn)
conn.close()
# other
error_file = config_dict['error_log']
msg_dict = inv.create_dict(config_dict, db_engine, 'msg_dict')

# ------------------------ process PO _data -----------------------------------
po_data = inv.build_po_data(config_dict, db_engine)

# split po_data into 2 parts
# those with Lot gets written into importLog
append_log = po_data.copy()
append_log = append_log[(append_log.lot!='') & append_log.lot.notnull()]

# check to see if entries in append_log exist in import_log
append_log = inv.check_exists(append_log,import_log,
                       ['prod_code','lot','po_name','qty'])

# only keep entries not exist in import_log
append_log = append_log[append_log.exist.isnull()]

#at this stage we leave actual import cost blank
append_log.actual_unit_cost = pd.to_numeric(append_log.actual_unit_cost,
                                            errors='coerce')
append_log['actual_currency_code'] = 1


# check that Unit is lower case
append_log.unit = append_log.unit.str.lower()

# add deliveryDate
append_log['delivery_date'] = datetime.date.today().strftime('%Y-%m-%d')
# if there is a warehouse_id ,use it, if not, create from product_info
if ('warehouse_id' not in append_log.columns):
    append_log = append_log.merge(
            product_info[['prod_code', 'warehouse_id']], how='left')

# remove invalid warehouse_id
append_log = append_log[(append_log.warehouse_id.notnull()) & 
                        (append_log.warehouse_id != '')]

# clean up
append_log.exp_date = append_log.exp_date.str.replace(' .*$','')
append_log = append_log[
        ['prod_code', 'unit', 'qty', 'po_name', 'lot', 'exp_date',
         'actual_unit_cost', 'actual_currency_code', 'delivery_date',
         'warehouse_id']]

# append to _database
if len(append_log)>0:
    print('appending to database:')
    print(append_log)
    conn = inv.db_open(config_dict)
    append_log.to_sql('import_log',conn,index=False,if_exists='append')
    conn.commit()
    conn.close()

# the remaining of po_data gets written to coming_list
coming_list = po_data[po_data.lot=='']
coming_list = coming_list[['name','ref_smn','vendor','qty','po_name']]
conn = inv.db_open(config_dict)
coming_list.to_sql('coming_list',conn,index=False,if_exists='replace')
conn.commit()
conn.close()

if (config_dict['db_type'] == 'MariaDB'):
    db_engine.dispose()
