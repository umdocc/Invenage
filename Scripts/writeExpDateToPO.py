#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#Writing exp Date to PO for DiaSorin

# This script will update Expiry Date from vendor using an excel file

# required libs and path
import pandas as pd, os, sqlite3,sys, openpyxl, datetime

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

POLocation = configData['POInvoicesPath']
databasePath = configData['databasePath']

fileName = os.path.join(POLocation,'DiaSorin-Y','310119',
                        'MDS.DiaSorinSpa.PO.310119.R2.xlsx')

if 'Dia' in fileName:
    currentNSX = 'DiaSorin'

data = pd.read_excel(fileName,skiprows = 19,dtype=str)

# drop the columns
if 'vendorExpDate' in data.columns:
    data = data.drop(columns='vendorExpDate')
if 'updatedDate' in data.columns:
    data = data.drop(columns='updatedDate')

conn = sqlite3.connect(databasePath)
productInfo = pd.read_sql_query('select * from productInfo', conn)
conn.close()
productInfo = productInfo[productInfo.NSX.str.contains(currentNSX)]

data = pd.merge(data,productInfo[['mfgCode','vendorExpDate','updatedDate']],
                how = 'left', left_on = 'REF',right_on = 'mfgCode')

data['dayLeft'] = pd.to_datetime(data.vendorExpDate,format='%Y-%m-%d',
    errors='coerce') - datetime.datetime.now()

data['dayLeft'] = data.dayLeft.dt.days
data['less270'] = ''
data.less270[data.dayLeft<270] = 'less9mths'

data = data[~data.REF.str.contains('nan')].reset_index(drop=True)

POObject = openpyxl.load_workbook(fileName)
sheet = POObject.active
sheet.cell(row=20,column=9).value = 'vendorExpDate'
sheet.cell(row=20,column=10).value = 'updatedDate'

for i in range(0,len(data)):
    sheet.cell(row=21+i,column=9).value = data.vendorExpDate[i]
    sheet.cell(row=21+i,column=10).value = data.updatedDate[i]
    sheet.cell(row=21+i,column=11).value = data.less270[i]    
POObject.save(fileName)
