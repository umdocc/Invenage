# check the database for missing actual cost price and attemp to fill
# ---------------------- Setup Block ------------------------------------------
import sys, os, pandas as pd
import sqlite3
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv

# ----------------------------- Data Read -------------------------------------
# database
conn = inv.db_open(config_dict)
product_info = pd.read_sql_query('select * from product_info',conn)
import_log = pd.read_sql_query('select * from import_log',conn)
localisation = pd.read_sql_query('select * from localisation',conn)
packaging = pd.read_sql_query('select * from packaging',conn)
conn.close()
# keep track for integrity check
import_log_len = len(import_log)

#other
error_file = config_dict['error_log']
msg_dict = inv.create_dict(config_dict,'msg_dict')
rename_dict = inv.create_dict(config_dict,'rename_dict')

# attemp to fill missing import_log price by checking the po
missing_price = import_log.copy()
missing_price = missing_price[missing_price.actual_unit_cost.isnull()]

# if there is missing price, attemp to fix
if (len(missing_price)>0):
    # re-read po_data
    po_data = inv.build_po_data(config_dict)
    po_data.actual_unit_cost = pd.to_numeric(
            po_data.actual_unit_cost, errors='coerce')
    po_data = po_data[po_data.actual_unit_cost.notnull()]
    po_data['added_actual_cost'] = po_data.actual_unit_cost
    po_data = po_data[['prod_code','qty','po_name','lot','added_actual_cost']]
    
    tmp_conn = sqlite3.connect('tmp.sqlite')
    po_data.to_sql('po_data',tmp_conn,index=False,if_exists='replace')
    import_log.to_sql('import_log',tmp_conn,index=False,if_exists='replace')
    tmp_conn.commit()
    merged_log = pd.read_sql_query(
            'select import_log.prod_code, import_log.unit, \
            import_log.qty, import_log.po_name, import_log.lot, \
            import_log.exp_date, import_log.actual_unit_cost, \
            import_log.actual_currency_code, import_log.delivery_date, \
            import_log.warehouse_id, po_data.added_actual_cost \
            from import_log left join po_data on \
            import_log.prod_code = po_data.prod_code and \
            import_log.po_name = po_data.po_name and \
            import_log.qty = po_data.qty and \
            import_log.lot = po_data.lot',tmp_conn)
    tmp_conn.close()
    merged_log.actual_unit_cost[merged_log.actual_unit_cost.isnull() & 
                                merged_log.added_actual_cost.notnull()] =   \
    merged_log.added_actual_cost[merged_log.actual_unit_cost.isnull() & 
                                 merged_log.added_actual_cost.notnull()]
    
    conn = inv.db_open(config_dict)
    merged_log.to_sql('import_log',conn,index=False,if_exists='replace')
    conn.close()