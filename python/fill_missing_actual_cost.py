# if we have a local import excel file, we handle it here
# ---------------------- Setup Block ------------------------------------------
import sys, os, pandas as pd
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

#other
error_file = config_dict['error_log']
msg_dict = inv.create_dict(config_dict,'msg_dict')
rename_dict = inv.create_dict(config_dict,'rename_dict')

# attemp to fill missing import_log price by checking the po
missing_price = import_log.copy()
missing_price = missing_price[missing_price.actual_unit_cost.isnull()]


