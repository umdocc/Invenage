# check the database integrity and write to log file
#
# ---------------------------- Setup ------------------------------------------
import sys, os, pandas as pd #import
inv_data_path = os.path.join(os.path.expanduser('~'),'invenage_data') 
sys.path.append(inv_data_path)
#build the config_dict
from python_conf import create_config_dict
config_dict = create_config_dict()
# add pythonPath
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv

# read the database
conn = inv.db_open(config_dict)
product_info = pd.read_sql_query('select * from product_info',conn)
packaging = pd.read_sql_query('select * from packaging',conn)
import_log = pd.read_sql_query('select * from import_log',conn)
sale_log = pd.read_sql_query('select * from sale_log',conn)
localisation = pd.read_sql_query('select * from localisation',conn)
conn.close()

# misc
localisation = localisation[localisation.app_lang==config_dict['app_lang']]
msg_dict = inv.create_dict(config_dict,'msg_dict')

error_file = config_dict['error_log']
if os.path.exists(error_file):
    os.remove(error_file)

# ------------------------------- sale_log ------------------------------------
# check sale_log for duplicated entries
test_df = sale_log.copy()
test_df = test_df[
        test_df[['prod_code','unit','qty','lot','pxk_num',
                  'warehouse_id']].duplicated()]
if (len(test_df)>0):
    inv.write_log(error_file,msg_dict['sale_log_check'])
    inv.write_log(error_file,msg_dict['duplicated_entry'])
    test_df.to_csv(error_file,index=False,sep='\t',mode='a')

# check sale_log for missing packaging
test_df = sale_log.copy()
test_df = inv.convertToPack(test_df,packaging,'qty','sale_pack_qty')
test_df = test_df[test_df.units_per_pack.isnull()][['prod_code','unit',
                   'units_per_pack']]
if (len(test_df)>0):
    inv.write_log(error_file,msg_dict['sale_log_check'])
    inv.write_log(error_file,msg_dict['unknown_pkg'])
    test_df.to_csv(error_file,index=False,sep='\t',mode='a')

# ------------------------- product_info --------------------------------------
#check product info for duplicated prod_code
test_df = product_info.copy()
test_df = test_df[test_df.prod_code.duplicated()][['prod_code','name']]
if (len(test_df)>0):
    inv.write_log(error_file,msg_dict['dup_prod_code'])
    test_df.to_csv(error_file,index=False,sep='\t',mode='a')
    
#check product info for duplicated name
test_df = product_info.copy()
test_df = test_df[test_df.name.duplicated()][['prod_code','name']]
if (len(test_df)>0):
    inv.write_log(error_file,msg_dict['dup_prod_name'])
    test_df.to_csv(error_file,index=False,sep='\t',mode='a')
    
#check product info for missing warehouse_id
test_df = product_info.copy()
test_df = test_df[test_df.warehouse_id.isnull()]
if (len(test_df)>0):
    inv.write_log(error_file,msg_dict['missing_warehouse_id'])
    test_df.to_csv(error_file,index=False,sep='\t',mode='a')

test_df = product_info.copy()
test_df = test_df[test_df.name.duplicated()][['prod_code','name']]
if (len(test_df)>0):
    inv.write_log(error_file,msg_dict['dup_prod_name'])
    test_df.to_csv(error_file,index=False,sep='\t',mode='a')

# ---------------------------- import_log -------------------------------------
# check import_log for missing packaging
test_df = import_log.copy()
test_df = inv.convertToPack(test_df,packaging,'qty','import_pack_qty')
test_df = test_df[test_df.units_per_pack.isnull()][['prod_code','unit',
                   'units_per_pack']]
if (len(test_df)>0):
    inv.write_log(error_file,msg_dict['import_log_check'])
    inv.write_log(error_file,msg_dict['unknown_pkg'])
    test_df.to_csv(error_file,index=False,sep='\t',mode='a')
    
# check importLog for missing cost
test_df = import_log[import_log.actual_unit_cost.isnull()]    
test_df = test_df.merge(product_info[['prod_code','name']],how='left')
test_df = test_df[['prod_code','name','lot']]
if len(test_df)>0:
    inv.write_log(error_file,msg_dict['missing_import_price'])
    test_df.to_csv(error_file,index=False,sep='\t',mode='a')

# ------------------------------ inventory ------------------------------------
# check inventory for negatives
inventory_df = inv.build_inv_df(import_log,sale_log,packaging)
test_df = inventory_df.copy()
test_df = test_df[test_df.remaining<0]
if len(test_df)>0:
    inv.write_log(error_file,msg_dict['neg_inventory'])
    test_df.to_csv(error_file,index=False,sep='\t',mode='a')



