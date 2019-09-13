# -*- coding: utf-8 -*-
#check the database integrity and print out a log file

# ---------------------- Setup Block ------------------------------------------
import sys, os, pandas as pd #import
# set up invenageDataPath so that we can access config file
inv_data_path = os.path.join(os.path.expanduser('~'),'invenage_data') 
sys.path.append(inv_data_path)

#build the configDict
from python_conf import create_config_dict
config_dict = create_config_dict()

# add pythonPath
sys.path.append(os.path.join(config_dict['app_path'],'Python'))
import invenageHelper as inv

# ---------------------- Variable Setup ---------------------------------------
reportFlag = False

# error log location, start-up script should remove this file
errorLogFile = configDict['error_log']

def writeLogMessage(errorLogLoc,message):
# writing errorLog startup message
    text_file = open(errorLogLoc, "a")
    text_file.write('\n'+message+'\n')
    text_file.close()
    
# ----------------------------------- Data Read -------------------------------
# read the database
conn = inv.dbOpen(configDict)
productInfo = pd.read_sql_query('select * from productInfo',conn)
Packaging = pd.read_sql_query('select * from Packaging',conn)
importLog = pd.read_sql_query('select * from importLog',conn)
saleLog = pd.read_sql_query('select * from saleLog',conn)
conn.close()

#check product info for duplicated
testDF = productInfo.copy()
testDF = testDF[testDF.prodCode.duplicated()][['prodCode','Name']]
if (len(testDF)>0):
    writeLogMessage(errorLogLoc,'productInfo contains duplicated prodCode!')
    testDF.to_csv(errorLogLoc,index=False,sep='\t',mode='a')
    reportFlag=True
    
# check importLog for missing packaging
testDF = importLog.copy()
testDF = inv.convertToPack(testDF,Packaging,'Quantity','importPackAmt')
testDF = testDF[testDF.unitsPerPack.isnull()][['prodCode','Unit',
                   'unitsPerPack']]
if (len(testDF)>0):
    textFile = open(errorLogLoc, "a")
    textFile.write('\n importLog contains unidentified packaging! \n \n')
    textFile.close()
    testDF.to_csv(errorLogLoc,index=False,sep='\t',mode='a')
    reportFlag=True
    
# check saleLog for missing packaging
testDF = saleLog.copy()
testDF = inv.convertToPack(testDF,Packaging,'Amount','salePackAmt')
testDF = testDF[testDF.unitsPerPack.isnull()][['prodCode','Unit',
                   'unitsPerPack']]
if (len(testDF)>0):
    textFile = open(errorLogLoc, "a")
    textFile.write('\n saleLog contains unidentified packaging! \n \n')
    textFile.close()
    testDF.to_csv(errorLogLoc,index=False,sep='\t',mode='a')
    reportFlag=True
    
inventoryDF = inv.buildInventorySummary(importLog,saleLog,Packaging)
testDF = inventoryDF.copy()
testDF = testDF[testDF.remaining<0]
if len(testDF)>0:
    writeLogMessage(errorLogLoc,'inventory summary contains negatives!')
    testDF.to_csv(errorLogLoc,index=False,sep='\t',mode='a')
    reportFlag=True

# open the errorLog
if reportFlag:
    inv.launchFile(errorLogLoc)
