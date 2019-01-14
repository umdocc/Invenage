#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# initialisation script to build the CoreData.sqlite db for MDS Trung Thien
# this script will be customised for each database that we need to init
# Data used:
# salesData from 01-06-2018 to 31-12-2018
# 
import pandas as pd, os, sys, sqlite3
print(sys.path)

#lazy!! should not do this in main app
os.chdir('/Users/umdocc/Dropbox/Invenage/CoreData/InitMDS')

saleData = pd.read_excel('salesData.010618-311218.xls',dtype=str,skiprows=1)
saleData['invoiceDate'] = saleData['Ngày ghi sổ']

saleData.invoiceDate = pd.to_datetime(saleData.invoiceDate,
                                      format='%Y-%m-%d %H:%M:%S',
                                      errors='coerce')

## build the standardised saleLogBase
#saleData['prodCode'] = saleData['Ngày ghi sổ']
#saleData.prodCode = saleData.prodCode.str.replace(' - .*$','')
#saleData.loc[saleData.invoiceDate.notnull(),'prodCode'] = ''
#currentProdCode = saleData.prodCode[0]
#for i in range(0,len(saleData)):
##    print(i)
#    if saleData.prodCode[i]=='':
#        saleData.prodCode[i] = currentProdCode
#    else:
#        currentProdCode = saleData.prodCode[i]
#
#hold = saleData.copy()
#
#saleData = saleData[saleData.invoiceDate.notnull()]
#
#saleData = saleData.rename(columns={'Số phiếu':'invoiceNum',
#                                    'Mã KH':'customerCode',
#                                    'Tên khách hàng':'customerName',
#                                    'ĐVT':'Unit','Đơn giá':'unitPrice',
#                                    'Số lượng':'Amount','Số tiền':'totalPrice'})
#saleData = saleData[['invoiceNum','customerCode','customerName','Unit',
#                     'unitPrice','Amount','totalPrice','invoiceDate',
#                     'prodCode']]
#
# write to dB so we can comment out the above
conn = sqlite3.connect('initMDS.sqlite')
saleData.to_sql('saleLogBase',conn,index=False,if_exists='replace')
conn.commit()
conn.close()

# make the base database enough to follow on HC2/HC1 tender:
saleData = saleData[saleData.customerCode=='CHORAY']
# remove certain prodCode
saleData = saleData[~saleData.prodCode.str.contains('ACI_ADV_')]
saleData = saleData[~saleData.prodCode.str.contains('ACETAMINOPHEN REAGENT')]
