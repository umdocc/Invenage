#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# at the beginning, Manella will update the database using feedback received

# ----------------------------------- Setup ------------------------------------
# required libs and path
import pandas as pd, os, sqlite3,sys, datetime

# getting appPath
if sys.path[0]!='':
    appPath = sys.path[0]
else:
    appPath = os.path.join(os.path.expanduser('~'),'Dropbox','Invenage')
    
# read the config file and configure paths
os.chdir(appPath)
from readConfig import getConfigData
pathDict = getConfigData()
feedbackPath = pathDict['feedbackPath']
coreDBPath = pathDict['coreDBPath']
scriptPath = pathDict['scriptPath']
sys.path.append(scriptPath)

from InvenageHelper import getFilesInfo

# control variables
errorsFree = True

# get a list of files in feedBack folder, excluding the forms folder
feedbackList = getFilesInfo(feedbackPath,'xlsx')
feedbackList = feedbackList[~feedbackList.fileName.str.contains(
        'feedbackForms')]

# all the database tables that needs to be read
conn = sqlite3.connect(coreDBPath)
importPrice = pd.read_sql_query('select * from importPrice',conn)
tenderDetails = pd.read_sql_query('select * from tenderDetails',conn)
orderName = pd.read_sql_query('select * from orderName',conn)
productInfo = pd.read_sql_query('select * from productInfo',conn)
packaging = pd.read_sql_query('select * from packaging',conn)
#smartGuess = pd.read_sql_query('select * from smartGuess',conn)
NSXSeller = pd.read_sql_query('select distinct NSX,Seller from smartGuess where \
                              guessType like "NSXDict"',conn)
#customerCodeLU = pd.read_sql_query(
#        'select customerCode, Customer from customerInfos',conn)
conn.close()

# get a list of files in feedBack folder, excluding the forms folder
feedbackList = getFilesInfo(feedbackPath,'xlsx')
feedbackList = feedbackList[~feedbackList.fileName.str.contains(
        'feedbackForms')]

# --------------------------- priceUpdateRequest ------------------------------

if len(feedbackList[
        feedbackList.fileName.str.contains('priceUpdateRequest')])>0:

    #extract file location
    priceUpdateRequestLoc = feedbackList.fileName[
        feedbackList.fileName.str.contains('priceUpdateRequest')].      \
        reset_index(drop=True)[0]
    # read the ED request form
    priceData = pd.read_excel(priceUpdateRequestLoc,dtype=str)
    
    # use NSX and mfgcode to lookup the prodCode
    priceData = pd.merge(priceData,productInfo[['NSX','mfgCode','prodCode']],
                         how='left', on = ['NSX','mfgCode'])
    
    # clean up the nan NSX
    priceData = priceData[priceData.NSX!='nan']
    priceData = priceData[priceData.NSX.notnull()]
    
    # remove duplicated entries
    priceData = priceData[
        ~priceData[['prodCode','priceType','Vendor','priceType']].duplicated()]
    
    # clean after merge
    priceData = priceData[priceData.prodCode.notnull()]
    priceData['Update'] = True
    
    # remove old price
    importPrice = pd.merge(importPrice,
                           priceData[['prodCode','priceType','Vendor','Update']],
                           how='left',on=['prodCode','priceType','Vendor'])
    importPrice = importPrice[importPrice.Update!=True]
    importPrice = importPrice.drop('Update',axis=1)
    
    # add the date this get updated
    priceData['lastUpdated'] = datetime.date.today().strftime('%d%m%y')
    
    priceData = priceData[['prodCode','importPrice','Currency','priceType',
                           'Vendor', 'lastUpdated']]
    importPrice = importPrice.append(priceData)
    
    
    # checking importPrice for duplication
    if len(importPrice[importPrice[
            ['prodCode','priceType','Vendor']].duplicated()]) > 0:
        raise RuntimeError('importPrice contains duplicated entries')
    else:
        conn = sqlite3.connect(coreDBPath)
        importPrice = importPrice.to_sql('importPrice',conn,index=False,
                                         if_exists='replace')
        conn.commit()
        conn.close()
        print('updating price successfully! Deleting feedback form!')
        os.remove(priceUpdateRequestLoc)

### ----------------------------- misingOrderName --------------------------------
##if len(feedbackList[feedbackList.fileName.str.contains('missingOrderName')])>0:
##    print('missingOrderName found!')
##    missingOrderNameLoc = feedbackList.fileName[
##        feedbackList.fileName.str.contains('missingOrderName')].         \
##        reset_index(drop=True)[0]
##    
##    #read the complaint form
##    missingOrderName = pd.read_excel(missingOrderNameLoc, dtype=str)
##    if ('NSX' in missingOrderName):
##        missingOrderName.drop(columns='NSX')
###    missingOrderName[missingOrderName.duplicated()]
##    # adding info
##    missingOrderName = pd.merge(missingOrderName,productInfo[['prodCode',
##                                                              'NSX']])
##
##    missingOrderName.loc[missingOrderName.customerCode=='CR',
##                         'Mode'] = 'Tender'
##    missingOrderName = pd.merge(missingOrderName,NSXSeller,how='left')
##                         
##    orderName = orderName.append(missingOrderName,sort=False)
##
##    # check integrity then update database
##    if len(orderName[orderName.duplicated()])>0:
##        raise RuntimeError('orderName contains duplicated entries!!!')
##    else:
##        conn = sqlite3.connect(coreDBPath)
##        orderName.to_sql('orderName',conn,index=False,if_exists='replace')
##        conn.commit()
##        conn.close()
##        os.remove(missingOrderNameLoc)
##
### ------------------ prodCodeChangeRequest -------------------------------------
### prodCodeChangeRequest should only change prodCode, if we also want to change 
### packaging we need to create a new prodCode
##if len(feedbackList[
##        feedbackList.fileName.str.contains('prodCodeChangeRequest')])>0:
##    print('prodCodeChageRequest found')
##    prodCodeChangeRequestLoc = feedbackList.fileName[
##            feedbackList.fileName.str.contains('prodCodeChangeRequest')].    \
##            reset_index(drop=True)[0]
##    tableList = ['importLog','importPrice','logXuatKhoBase','orderName',
##                 'packaging','productInfo','saleLog','tenderDetails',
##                 'tonKhoBase','xuatKhoDict']
##    prodCodeChangeRequest = pd.read_excel(prodCodeChangeRequestLoc, dtype=str)
##    
##    # process the request to change prodCode
##    for j in range(0,len(prodCodeChangeRequest)):
##        oldProdCode = prodCodeChangeRequest.oldProdCode[j]
##        newProdCode = prodCodeChangeRequest.newProdCode[j]
##    
##        # read and change the tables from CoreDB
##        conn = sqlite3.connect(coreDBPath)
##        for i in range(0,len(tableList)):
##            tableName = tableList[i]
##            dbTable = pd.read_sql_query('select * from '+tableName,conn)
##            dbTable.loc[dbTable.prodCode==oldProdCode,'prodCode'] = newProdCode
##            dbTable.to_sql(tableName,conn,index=False,if_exists='replace')
##            conn.commit()
##        conn.close()
##    
##    # clear the request log
##    os.remove(prodCodeChangeRequestLoc)
##
### ---------------------- packagingMissing --------------------------------------
##if len(feedbackList[feedbackList.fileName.str.contains('packagingMissing')])>0:
##    print('packagingMissing request found')
##    packagingMissingLoc = feedbackList.fileName[
##            feedbackList.fileName.str.contains('packagingMissing')].    \
##            reset_index(drop=True)[0]
##    
##    packagingMissing = pd.read_excel(packagingMissingLoc, dtype=str)
##    packagingMissing.unitsPerPack = pd.to_numeric(packagingMissing.unitsPerPack,
##                                                  errors='coerce')
##    packagingMissing = packagingMissing[packagingMissing.unitsPerPack.notnull()]
##
##    # verifying packaging table and update the database
##    packaging = packaging.append(packagingMissing,sort=True)    
##    if (len(packaging[packaging.duplicated()])>0):
##        raise RuntimeError('packaging contains duplicated entries')
##    else:
##        conn = sqlite3.connect(coreDBPath)
##        packaging.to_sql('packaging',conn,index=False,if_exists='replace')
##        conn.commit()
##        conn.close()
##        os.remove(packagingMissingLoc)
##
### ---------------------- missingXuatKhoDict ------------------------------------
##if len(feedbackList[feedbackList.fileName.str.contains(
##        'missingXuatKhoDict')])>0:
##    print('missingXuatKhoDict found!')
##    missingXuatKhoDictLoc = feedbackList.fileName[
##            feedbackList.fileName.str.contains('missingXuatKhoDict')].    \
##            reset_index(drop=True)[0]
##    
##    #read the complaint form
##    missingXKName = pd.read_excel(missingXuatKhoDictLoc, dtype=str)
##    if ('prodCode' not in missingXKName.columns) or                          \
##        ('Name' not in missingXKName.columns):
##        print('missingXuatKhoDict not in correct format! Deleting it....')
##        os.remove(missingXuatKhoDictLoc)
##    else:
##        # clean out unfilled sections
##        missingXKName = missingXKName[missingXKName.prodCode!='']
##        
##        # update the smartGuess in coreDB
##        conn = sqlite3.connect(coreDBPath)
##        smartGuess = pd.read_sql_query('select * from smartGuess',conn)
##        conn.close()
##        missingXKName = missingXKName.rename(columns={'Name':'String'})
##        missingXKName['customerCode'] = ''
##        missingXKName['Customer'] = ''
##        missingXKName['NSX'] = ''
##        missingXKName['Vendor'] = ''
##        missingXKName['guessType'] = 'xuatKhoDict'
##                
##        smartGuess = smartGuess.append(missingXKName,sort=False)
##        smartGuess = smartGuess.sort_values(by='guessType')
##        
##        # integrity check and write back
##        if len(smartGuess[smartGuess.duplicated()])>0:
##            raise RuntimeError('smartGuess contains duplicated entries \
##                               after appending complaints')
##        else:
##            conn = sqlite3.connect(coreDBPath)
##            smartGuess.to_sql('smartGuess',conn,index=False,
##                               if_exists='replace')
##            conn.commit()
##            conn.close()
##            os.remove(missingXuatKhoDictLoc)
#    
## ------------------------ appendPriceRequest ----------------------------------
#if len(feedbackList[feedbackList.fileName.str.contains(
#        'appendPriceRequest')])>0:
#
#    missingPriceRequestLoc = feedbackList.fileName[
#        feedbackList.fileName.str.contains('appendPriceRequest')].         \
#        reset_index(drop=True)[0]
#    #read the complaint form
#    missingPrice = pd.read_excel(missingPriceRequestLoc, dtype=str)
#    
#    # adding info to missingPrice
#    importPrice = importPrice.append(missingPrice,sort=False)
#    
#    # check integrity then update database
#    tmp = importPrice.copy()
#    tmp = tmp[['prodCode','Vendor','Currency','priceType']]
#
#    if len(tmp[tmp.duplicated()])>0:
#        raise RuntimeError('importPrice contains duplicated entries!!!')
#    else:
#        conn = sqlite3.connect(coreDBPath)
#        importPrice.to_sql('importPrice',conn,index=False,if_exists='replace')
#        conn.commit()
#        conn.close()
#        os.remove(missingPriceRequestLoc)
#    
#if len(feedbackList[feedbackList.fileName.str.contains(
#        'missingXuatKhoCustomer')])>0:
#    #extract file location
#    missingXuatKhoCustomerLoc = feedbackList.fileName[
#        feedbackList.fileName.str.contains('missingXuatKhoCustomer')].         \
#        reset_index(drop=True)[0]
#    
#    #read the complaint form
#    missingXuatKhoCustomer = pd.read_excel(missingXuatKhoCustomerLoc, dtype=str)
#    
#    # adding info
#    missingXuatKhoCustomer['guessType'] = 'xkCustomer'
#    missingXuatKhoCustomer['prodCode'] = ''
#    missingXuatKhoCustomer['NSX'] = ''
#    missingXuatKhoCustomer['Vendor'] = ''    
#    missingXuatKhoCustomer = pd.merge(missingXuatKhoCustomer,customerCodeLU)
#    missingXuatKhoCustomer = missingXuatKhoCustomer.rename(
#            columns={'xkCustomer':'String'})
#    
#    smartGuess = smartGuess.append(missingXuatKhoCustomer,sort=False)
#    smartGuess = smartGuess.sort_values(by='guessType')
#    
#    # check integrity then update database
#    if len(smartGuess[smartGuess.duplicated()])>0:
#        raise RuntimeError('smartGuess contains duplicated entries \
#                           after appending complaints')
#    else:
#        conn = sqlite3.connect(coreDBPath)
#        smartGuess.to_sql('smartGuess',conn,index=False,
#                           if_exists='replace')
#        conn.commit()
#        conn.close()
#        os.remove(missingXuatKhoCustomerLoc)
#
## --------------------------- process kiem kho ---------------------------------
## when there is a KiemKho, Manella will scan feedback
#if len(feedbackList[feedbackList.fileName.str.contains(
#        'KiemKho')])>0:
#    kiemKhoLoc = feedbackList.fileName[
#        feedbackList.fileName.str.contains('KiemKho')].                        \
#        reset_index(drop=True)[0]
#    
#    #extract the dateStr
#    dateStr = re.sub('.xlsx','',kiemKhoLoc)
#    dateStr = re.sub('^.*\.','',dateStr)
#    
#    # process kiemKho form
#    kiemKho = pd.read_excel(kiemKhoLoc, dtype=str)
#    kiemKho.KiemKho = pd.to_numeric(kiemKho.KiemKho, errors='coerce')
#    kiemKho = kiemKho[kiemKho.KiemKho.notnull()]
#    kiemKho.SLKhoPack = pd.to_numeric(kiemKho.SLKhoPack)
#    kiemKho['Amount'] = kiemKho.SLKhoPack - kiemKho.KiemKho
#
#    #build saleLog Data
#    kiemKho['Unit'] = 'pack'
#    kiemKho['PXK'] = 'KK'+dateStr
#    kiemKho['Customer'] = 'XDCK-XuatDieuChinhKHo'
#    datetime.date.today().strftime('%d-%m-%Y')
#    kiemKho['saleDate'] = datetime.date.today().strftime('%d-%m-%Y')
#    kiemKho['Note'] = 'kiem Kho '+kiemKho.NSX
#    kiemKho = kiemKho[['prodCode','Unit','Amount','PXK','Customer','saleDate',
#                       'Note',]]
#    print('appending KiemKho to saleLog')
#    # read and append sale log
#    conn = sqlite3.connect(coreDBPath)
#    saleLog = pd.read_sql_query('select * from saleLog',conn)
#    conn.close()
#    
#    if len(saleLog[saleLog.PXK=='KK'+dateStr])>0:
#        raise RuntimeError('saleLog already contains this KiemKho!')
#    else:
#        saleLog = saleLog.append(kiemKho)
#        conn = sqlite3.connect(coreDBPath)
#        saleLog.to_sql('saleLog',conn,index=False,if_exists='replace')
#        conn.commit()
#        conn.close()
#        print('KiemKho appended successfully! Deleting feedback form!')
#        os.remove(kiemKhoLoc)


        
        
## ------------------------ ExpiryDateUpdateRequest ----------------------------
#if len(feedbackList[
#        feedbackList.fileName.str.contains('ExpiryDateUpdateRequest')])>0:
#        #extract file location
#    EDUpdateReqLoc = feedbackList.fileName[
#        feedbackList.fileName.str.contains('ExpiryDateUpdateRequest')].      \
#        reset_index(drop=True)[0]
#    # read the ED request form
#    EDData = pd.read_excel(EDUpdateReqLoc,dtype=str)
#    # standard column rename
#    EDData = EDdata.rename(columns = {'CODE':'mfgCode',
#                                  'EXPIRY DATE':'ExpiryDate'})
#    
##    if it contains more than one mfg, raise error
#    if len(EDData.NSX.unique())>1:
#        raise RuntimeError('ExpiryDateUpdateRequest contains more than one \
#                           manufacturer')
#    
#    # DiaSorin-Y wrangling
#    if EDData.NSX.unique()[0] == 'DiaSorin-Y':
#        conn = sqlite3.connect(sqlite_file)
#        productInfo = pd.read_sql_query('select * from productInfo', conn)
#        conn.close()
        