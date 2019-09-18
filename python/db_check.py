#check the database integrity and write to log file
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

# ------------------------------- Checking ------------------------------------
#check product info for duplicated
testDF = product_info.copy()
testDF = testDF[testDF.prod_code.duplicated()][['prod_code','name']]
if (len(testDF)>0):
    inv.write_log(error_file,msg_dict['dup_prod_code'])
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    
# check import_log for missing packaging
testDF = import_log.copy()
testDF = inv.convertToPack(testDF,packaging,'qty','import_pack_qty')
testDF = testDF[testDF.units_per_pack.isnull()][['prod_code','unit',
                   'units_per_pack']]
if (len(testDF)>0):
    inv.write_log(error_file,msg_dict['import_log_check'])
    inv.write_log(error_file,msg_dict['unknown_pkg'])
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    
# check sale_log for missing packaging
testDF = sale_log.copy()
testDF = inv.convertToPack(testDF,packaging,'qty','sale_pack_qty')
testDF = testDF[testDF.units_per_pack.isnull()][['prod_code','unit',
                   'units_per_pack']]
if (len(testDF)>0):
    inv.write_log(error_file,msg_dict['sale_log_check'])
    inv.write_log(error_file,msg_dict['unknown_pkg'])
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    
# check inventory for negatives
inventory_df = inv.build_inv_df(import_log,sale_log,packaging)
testDF = inventory_df.copy()
testDF = testDF[testDF.remaining<0]
if len(testDF)>0:
    inv.write_log(error_file,msg_dict['neg_inventory'])
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')

# check importLog for missing cost
test_df = import_log[import_log.actual_unit_cost.isnull()]    
test_df = test_df.merge(product_info[['prod_code','name']],how='left')
test_df = test_df[['prod_code','name','lot']]
if len(test_df)>0:
    inv.write_log(error_file,msg_dict['missing_import_price'])
    test_df.to_csv(error_file,index=False,sep='\t',mode='a')
    
## open the errorLog if it exists
#if os.path.exists(error_file):
#    inv.launch_file(error_file)
