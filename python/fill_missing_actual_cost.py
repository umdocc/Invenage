# check the database for missing actual cost price and attemp to fill
# ---------------------- Setup Block ------------------------------------------
import sys, os, pandas as pd
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv

# ----------------------------- Data Read -------------------------------------
# database
db_engine = inv.create_db_engine(config_dict)
conn = inv.db_open(config_dict,db_engine)
product_info = pd.read_sql_query('select * from product_info',conn)
import_log = pd.read_sql_query('select * from import_log',conn)
localisation = pd.read_sql_query('select * from localisation',conn)
packaging = pd.read_sql_query('select * from packaging',conn)
conn.close()
# keep track for integrity check
import_log_len = len(import_log)

#other
error_file = config_dict['error_log']
msg_dict = inv.create_dict(config_dict,db_engine,'msg_dict')
rename_dict = inv.create_dict(config_dict,db_engine,'rename_dict')

# attemp to fill missing import_log price by checking the po
missing_price = import_log.copy()
missing_price = missing_price[missing_price.actual_unit_cost.isnull()]

# if there is missing price, attemp to fix
if (len(missing_price)>0):
    # re-read po_data
    po_data = inv.build_po_data(config_dict,db_engine)
    po_data.actual_unit_cost = pd.to_numeric(
            po_data.actual_unit_cost, errors='coerce')
    po_data = po_data[po_data.actual_unit_cost.notnull()]
    po_data['added_actual_cost'] = po_data.actual_unit_cost
    po_data = po_data[['prod_code','qty','po_name','lot','added_actual_cost']]
    
    missing_price = pd.merge(missing_price,po_data,how='left')
    updated_df = missing_price.copy()
    updated_df = updated_df[updated_df.added_actual_cost.notnull()]
    # if there is something to update, proceed
    if (len(updated_df)>0):
        missing_price.actual_unit_cost = missing_price.added_actual_cost
        missing_price = missing_price.drop(columns='added_actual_cost')
    # open database, and clear all null price 
        conn = inv.db_open(config_dict,db_engine)
        conn.execute('delete from import_log where actual_unit_cost is null')
        missing_price.to_sql('import_log',conn,index=False,if_exists='append')        
    # merged_log.to_sql('import_log',conn,index=False,if_exists='replace')
        conn.close()

# dispose the database engine
if (config_dict['db_type'] == 'MariaDB'):
    db_engine.dispose()