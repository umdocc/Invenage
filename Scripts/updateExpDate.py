#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# This script will update Expiry Date from vendor using an excel file

# required libs and path
import pandas as pd, os, sqlite3,sys, datetime

# getting appPath
if sys.path[0]!='':
    appPath = sys.path[0]
else:
    appPath = os.path.join(os.path.expanduser('~'),'Dropbox',
                           'Invenage')
os.chdir(appPath)

# get configuration data
from readConfig import getConfigData
configData = getConfigData()
databasePath = configData['databasePath']

fileName = os.path.join(appPath,'Data','expDate', 'Dia.ED.wk7.xlsx')
expUpdateData = pd.read_excel(fileName, dtype=str)

# vendor-based cleaning
# DiaSorin
expUpdateData = expUpdateData.rename(columns={'CODE':'mfgCode'})
NSX = ''
if 'Dia' in fileName:
    currentNSX = 'DiaSorin'
    expUpdateData = expUpdateData.rename(
            columns={'EXPIRY DATE':'vendorExpDate'})
    expUpdateData.vendorExpDate = pd.to_datetime(
            expUpdateData.vendorExpDate,format='%Y-%m-%d %H:%M:%S',
            errors='coerce')
    expUpdateData = expUpdateData[expUpdateData.vendorExpDate.notnull()]
    expUpdateData.vendorExpDate = expUpdateData.vendorExpDate.dt.strftime(
            '%Y-%m-%d')

# reading information from database
conn = sqlite3.connect(databasePath)
productInfo = pd.read_sql_query('select * from productInfo',conn)
conn.close()

# filter then update the vendorExpDate column
tmp = productInfo[productInfo.NSX.str.contains(currentNSX)]
tmp = tmp.drop(columns='vendorExpDate')
tmp = pd.merge(tmp,expUpdateData[['mfgCode','vendorExpDate']],
               how='left',on='mfgCode')
tmp = tmp[tmp.vendorExpDate.notnull()]

# add the updatedDate
today = datetime.date.today()
tmp.updatedDate = today.strftime('%Y-%m-%d')
tmp = tmp.reset_index(drop=True)

# updating the database
conn = sqlite3.connect(databasePath)
crsr = conn.cursor()
for i in range(0,len(tmp)):
    sqlcommand = "UPDATE productInfo SET updatedDate = "+"'"+tmp.updatedDate[i]+"'"+ \
    ", vendorExpDate = "+ "'"+tmp.vendorExpDate[i]+"'"+ \
    " where prodCode = "+"'"+tmp.prodCode[i]+"'"
    crsr.execute(sqlcommand)
    conn.commit()
conn.close()
