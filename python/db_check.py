# -*- coding: utf-8 -*-
#check the database integrity and print out a log file

# ---------------------- Setup Block ------------------------------------------
import sys, os, pandas as pd #import
# set up invenageDataPath so that we can access config file
inv_data_path = os.path.join(os.path.expanduser('~'),'invenage_data') 
sys.path.append(inv_data_path)

#build the config_dict
from python_conf import create_config_dict
config_dict = create_config_dict()

# add pythonPath
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv

# ---------------------- Variable Setup ---------------------------------------
reportFlag = False

# error log location, start-up script should remove this file
error_file = config_dict['error_log']

def write_log(error_file,message):
    text_file = open(error_file, "a")
    text_file.write('\n'+message+'\n')
    text_file.close()

# writing errorLog startup message
text_file = open(error_file, "w")
text_file.write('Error log:\n')
text_file.close()
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
    write_log(error_file,'product_info contains duplicated prod_code!')
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    reportFlag=True
    
# check importLog for missing packaging
testDF = import_log.copy()
testDF = inv.convertToPack(testDF,packaging,'Quantity','importPackAmt')
testDF = testDF[testDF.unitsPerPack.isnull()][['prodCode','Unit',
                   'unitsPerPack']]
if (len(testDF)>0):
    textFile = open(error_file, "a")
    textFile.write('\n importLog contains unidentified packaging! \n \n')
    textFile.close()
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    reportFlag=True
    
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
    write_log(error_file,'inventory summary contains negatives!')
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    reportFlag=True

# open the errorLog
if reportFlag:
    inv.launchFile(error_file)
