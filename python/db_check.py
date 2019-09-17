# -*- coding: utf-8 -*-
#check the database integrity and print out a log file
# ---------------------- Setup Block ------------------------------------------
import sys, os, pandas as pd #import
inv_data_path = os.path.join(os.path.expanduser('~'),'invenage_data') 
sys.path.append(inv_data_path)

#build the config_dict
from python_conf import create_config_dict
config_dict = create_config_dict()

# add pythonPath
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv

# ---------------------- Variable Setup ---------------------------------------
# error log location, file removed at start-up
error_file = config_dict['error_log']
if os.path.exists(error_file):
  os.remove(error_file)

# ----------------------------------- Data Read -------------------------------
# read the database
conn = inv.db_open(config_dict)
product_info = pd.read_sql_query('select * from product_info',conn)
packaging = pd.read_sql_query('select * from packaging',conn)
import_log = pd.read_sql_query('select * from import_log',conn)
sale_log = pd.read_sql_query('select * from sale_log',conn)
conn.close()

#check product info for duplicated
testDF = product_info.copy()
testDF = testDF[testDF.prod_code.duplicated()][['prod_code','name']]
if (len(testDF)>0):
    inv.write_log(error_file,'product_info contains duplicated prod_code!')
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    
# check import_log for missing packaging
testDF = import_log.copy()
testDF = inv.convertToPack(testDF,packaging,'qty','import_pack_qty')
testDF = testDF[testDF.units_per_pack.isnull()][['prod_code','unit',
                   'units_per_pack']]
if (len(testDF)>0):
    inv.write_log(error_file,'import_log contains unidentified packaging!')
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    
# check sale_log for missing packaging
testDF = sale_log.copy()
testDF = inv.convertToPack(testDF,packaging,'qty','sale_pack_qty')
testDF = testDF[testDF.units_per_pack.isnull()][['prod_code','unit',
                   'units_per_pack']]
if (len(testDF)>0):
    inv.write_log(error_file,'sale_log contains unidentified packaging!')
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    
inventory_df = inv.build_inv_df(import_log,sale_log,packaging)
testDF = inventory_df.copy()
testDF = testDF[testDF.remaining<0]
if len(testDF)>0:
    inv.write_log(error_file,'inventory summary contains negatives!')
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')

# open the errorLog if it exists
if os.path.exists(error_file):
    inv.launchFile(error_file)
