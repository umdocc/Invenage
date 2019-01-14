#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# at the beginning, Manella will update the database using feedback received

# ----------------------------------- Setup ------------------------------------
# required libs and path
import pandas as pd, os, sqlite3,sys, re, datetime

# getting appPath
if sys.path[0]!='':
    appPath = sys.path[0]
else:
    appPath = os.path.join(os.path.expanduser('~'),'Dropbox',
                           'Desktop','Softanage')
    
# read the config file and configure paths
sys.path.append(appPath)
from readConfig import getAppPath
pathDict = getAppPath()
feedbackPath = pathDict['feedbackPath']
coreDBPath = pathDict['coreDBPath']
scriptPath = pathDict['scriptPath']
sys.path.append(scriptPath)

from ManellaHelper import getFilesInfo, createRenameDict, cleanMDSData

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
smartGuess = pd.read_sql_query('select * from smartGuess',conn)
NSXSeller = pd.read_sql_query('select distinct NSX,Seller from smartGuess where \
                              guessType like "NSXDict"',conn)
customerCodeLU = pd.read_sql_query(
        'select customerCode, Customer from customerInfos',conn)
conn.close()
    
# -------------------- tenderUpdateRequest -------------------------------------
if len(feedbackList[
        feedbackList.fileName.str.contains('updateTenderRequest')])>0:
    print('tenderUpdateRequest found')
    tenderUpdateRequestLoc = feedbackList.fileName[
            feedbackList.fileName.str.contains('updateTenderRequest')].    \
            reset_index(drop=True)[0]    
    tenderUpdate = pd.read_excel(tenderUpdateRequestLoc, dtype=str)
    colDict = createRenameDict('colNamesDict')
    tenderUpdate = tenderUpdate.rename(columns = colDict)
    
    # clean up the tenderRequest
    tenderUpdate = tenderUpdate[~(tenderUpdate.Name == 'nan')]
    tenderUpdate = tenderUpdate[tenderUpdate.Name.notnull()]
    
    # clean the tender
    tenderUpdate = cleanMDSData(tenderUpdate)

    tenderUpdate = pd.merge(tenderUpdate,
                            orderName[['prodCode','Name','customerCode']],
                            how='left')
    tenderUpdate[tenderUpdate.duplicated()]
    
    # if we cannot find a prodCode, print out a feed backrequest
    if len(tenderUpdate[tenderUpdate.prodCode.isnull()])>0:
        tmp = tenderUpdate[tenderUpdate.prodCode.isnull()]
        tmp = tmp[['NSX','Name','customerCode','prodCode']]
        tmp['Vendor'] = ''
        outputFileLoc = os.path.join(feedbackPath,'missingOrderName.xlsx')
        tmp.to_excel(outputFileLoc,index=False)
        raise RuntimeError('missing prodCode for orderName, feedback requested')
        
    
    tenderUpdate = tenderUpdate[['Stt','Name','Unit','unitPrice','Amount',
                                 'totalPrice','NSX','prodCode','tenderName',
                                 'customerCode']]
   
    # use the tenderName to check integrity
    currentTenderName = list(set(tenderDetails.tenderName))
    newTenderName = list(set(tenderUpdate.tenderName))
    
    # integrity check
    for i in range(0,len(newTenderName)):
        if newTenderName[i] in currentTenderName:
            raise RuntimeError('New tender name already in database')
            errorsFree = False
        if len(tenderUpdate[tenderUpdate.prodCode.duplicated()])>0:
            raise RuntimeError('tenderUpdate contains duplications, \
                               check source or orderName')
    if errorsFree:
        conn = sqlite3.connect(coreDBPath)
        tenderUpdate.to_sql('tenderDetails',conn,index=False,if_exists='append')
        conn.commit()
        conn.close()
    
    # clear the request file
    os.remove(tenderUpdateRequestLoc)