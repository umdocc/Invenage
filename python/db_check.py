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
product_info = pd.read_sql_query('select * from productInfo',conn)
packaging = pd.read_sql_query('select * from Packaging',conn)
import_log = pd.read_sql_query('select * from importLog',conn)
sale_log = pd.read_sql_query('select * from saleLog',conn)
conn.close()

#check product info for duplicated
testDF = product_info.copy()
testDF = testDF[testDF.prodCode.duplicated()][['prodCode','Name']]
if (len(testDF)>0):
    inv.write_log(error_file,'product_info contains duplicated prod_code!')
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    
# check importLog for missing packaging
testDF = import_log.copy()
testDF = inv.convertToPack(testDF,packaging,'Quantity','importPackAmt')
testDF = testDF[testDF.unitsPerPack.isnull()][['prodCode','Unit',
                   'unitsPerPack']]
if (len(testDF)>0):
    textFile = open(error_file, "a")
    inv.write_log(error_file,'importLog contains unidentified packaging!')
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    
# check saleLog for missing packaging
testDF = sale_log.copy()
testDF = inv.convertToPack(testDF,packaging,'Amount','salePackAmt')
testDF = testDF[testDF.unitsPerPack.isnull()][['prodCode','Unit',
                   'unitsPerPack']]
if (len(testDF)>0):
    textFile = open(error_file, "a")
    textFile.write('\n sale_log contains unidentified packaging! \n \n')
    textFile.close()
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    reportFlag=True
    
inventoryDF = inv.buildInventorySummary(import_log,sale_log,packaging)
testDF = inventoryDF.copy()
testDF = testDF[testDF.remaining<0]
if len(testDF)>0:
    inv.write_log(error_file,'inventory summary contains negatives!')
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    reportFlag=True

# open the errorLog if it exists
if os.path.exists(error_file):
    inv.launchFile(error_file)
