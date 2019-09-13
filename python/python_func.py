#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#Functions for extension scripts
import pandas as pd, sqlite3
import platform, subprocess, glob, os
from sqlalchemy import create_engine
#from sqlalchemy import NullPool
# create a list of files with locked info, can use exclude    
def getFilesInfo(filePath,extension,exclude):
    if isinstance(exclude,str):
        exclude = exclude.split()
    fileList = pd.DataFrame(glob.glob(os.path.join(filePath,'**','*.'+extension), 
                          recursive=True),columns=['fullPath'])
    # create the locked column
    lockedList = fileList.copy()
    lockedList = lockedList[lockedList.fullPath.str.contains('~\$')]
    lockedList.fullPath =lockedList.fullPath.str.replace('~\$','')
    lockedList['locked'] = True
    fileList = fileList[~fileList.fullPath.str.contains('~\$')]
    fileList = pd.merge(fileList,lockedList,how='left',on='fullPath')
    
    for folderName in exclude:
#        print(folderName)
        fileList = fileList[~fileList.fullPath.str.contains(folderName)]
    fileList = fileList.reset_index(drop=True)
    fileList['fileName'] = fileList.fullPath.apply(os.path.basename)
    return(fileList)

# this function read a PO and detrmine its table location    
def getPODataLoc(inputExcelFile,headerStr):
    MDSDataFrame = pd.read_excel(inputExcelFile)
    exitVar=False
    for cLoc in range(0,len(MDSDataFrame.columns)):
        for rLoc in range(0,len(MDSDataFrame)):
            if (MDSDataFrame.iloc[rLoc,cLoc]==headerStr):
                exitVar = True
                break
        if exitVar:
            break
    return(rLoc,cLoc)

def createRenameDict(conn,dictType):
    renameDF = pd.read_sql_query(
            'select * from guessTable where guessType = "'+dictType+'"',conn)
    # create the rename Dictionary
    renameDict = {}
    # now use relative to user path for all data storage
    for i in range(0,len(renameDF)):
        renameDict[renameDF.inputStr[i]] = renameDF.outputStr[i]
    return(renameDict)

# db_open assume mariaDB, using sqlalchemy we should be able to use
# pandas to_sql and read_sql, remember to call conn.dispose() to close 
# the connection
def db_open(config_dict):
    if (config_dict['db_type']=='MariaDB'):
        conn = create_engine('mysql+mysqlconnector://'+                \
                             config_dict['sql_usr']+                    \
                               ":"+config_dict['sql_pswd']+"@"+         \
                               config_dict['sql_host']+"/invenage")
    if (config_dict['db_type']=='SQLite'):
        db_file = config_dict['db_file']
        conn = sqlite3.connect(db_file)
    return(conn)

# the check exists function take tableA, tableB and check if entries in B exists in A
def checkExists(tableA,tableB,colList):
    tableB = tableB[colList]
        
    tableB['exist'] = True
    tableA = pd.merge(tableA,tableB,how='left',on = colList)
    return(tableA)

def createUnitPackaging(Packaging):
    unitPackaging = Packaging.copy()
    unitPackaging = unitPackaging[unitPackaging.unitsPerPack==1]
    unitPackaging = unitPackaging[unitPackaging.Unit!='pack']
    # if somehow the product has 2 fundamental packaging, use only one
    unitPackaging = unitPackaging[~unitPackaging.prodCode.duplicated()]
    return(unitPackaging)

def updatePOInfo(homePath,configDict,excludeList):
#    appLang = configDict['appLang']
    poInvoicePath = configDict['poInvoicePath']
    # read the current poInfo from database, remove anything that does not
    # have a valid path
    conn = dbOpen(configDict)
    poInfo = pd.read_sql_query('select * from poInfo',conn)
    # verify fileExists status and remove PO that no longer exists
    if (len(poInfo)>0):
        poInfo['fileExist'] = poInfo['fileLocation'].map(os.path.isfile)
        poInfo = poInfo[poInfo.fileExist]
        poInfo = poInfo.drop('fileExist',axis=1)
        poInfo.to_sql('poInfo',conn,index=False,if_exists='replace')
    conn.close()

    
    # update the database with added PO
    poList = getFilesInfo(poInvoicePath,'xlsx',excludeList)
    # we need the PO to contains '.PO." string
    poList = poList[poList.fileName.str.contains('\.PO\.')]
    poList = poList.rename(columns={'fullPath':'fileLocation',
                                    'fileName':'poName'})
    # remove the homePath
#    poList.fileLocation = poList.fileLocation.str.replace(
#            homePath,'')
    poList = checkExists(poList,poInfo,['poName','fileLocation'])
    appendPOList = poList[poList.exist.isnull()]
    appendPOList['poStatusCode'] = 1
    appendPOList['Note'] = 'added by Invenage'
    appendPOList = appendPOList[['poName','poStatusCode','Note','fileLocation']]

    conn = dbOpen(configDict)
    appendPOList.to_sql('poInfo',conn,index=False,if_exists='append')
    conn.commit()
    conn.close()
    
def buildFullPOInfo(configDict,existingFileOnly):
    appLang = configDict['appLang']
    conn = dbOpen(configDict)
    poInfo = pd.read_sql_query('select * from poInfo',conn)
    tlsTbl = pd.read_sql_query('select poStatusCode.poStatusCode,  \
                       localisation.Actual as renderedStatus from  poStatusCode  \
                       inner join localisation on  \
                       poStatusCode.Label = localisation.Label where  \
                       localisation.appLang = "'+appLang+'"',conn)
    conn.close()
    poInfo = poInfo.merge(tlsTbl,how='left')
    poInfo = poInfo[poInfo.fileLocation.notnull()].reset_index(drop=True)
    return(poInfo)

def buildPOData(poInfo,NSXDict,renameDict,productInfo,Packaging,
                dataCleaning):
    for i in range(0,len(poInfo)):
        # as paths are relative we need to append homePath
        inputExcelFile = os.path.join(
                os.path.expanduser('~'),poInfo.fileLocation[i])
#        print(inputExcelFile)
        currentNSX = ''
        for k in range(0,len(NSXDict)):
            if NSXDict.inputStr[k] in inputExcelFile:
                currentNSX = NSXDict.outputStr[k]
        (rLoc,cLoc) = getPODataLoc(inputExcelFile,'Description')
        tmp = pd.read_excel(inputExcelFile, skiprows = rLoc+1,
                               skipcolumns=cLoc+1,dtype=str)
        tmp['NSX'] = currentNSX
    
        tmp['POName'] = os.path.basename(inputExcelFile)
    
        tmp = tmp.rename(columns = renameDict)
        if (i == 0):
            POData = tmp.copy()
        else:
            POData = POData.append(tmp,sort=False)
    POData.Quantity = pd.to_numeric(POData.Quantity, errors='coerce')
    
    # if NSX is still blank raise error
    report = POData.copy()
    report = report[report.NSX=='']
    if (len(report)>0):
        print(report)
        raise RuntimeError('error identifying NSX')
        
    # normally we only want to keep things that make sense    
    # blank Lot should be blank
    if dataCleaning:
        POData = POData[POData.Quantity>0 & POData.Quantity.notnull()]
        POData.loc[POData.Lot.isnull(),'Lot'] = ''
        POData.loc[POData.Lot == 'nan','Lot'] = ''
        POData = POData.merge(
                productInfo[['prodCode','mfgCode','NSX']],how='left')
        # raise error if unknown items found
        if any(POData.prodCode.isnull()):
            print(POData[POData.prodCode.isnull()])
            raise RuntimeError('prodCode not found')
    unitPackaging = createUnitPackaging(Packaging)
    POData = pd.merge(POData,unitPackaging[['Unit','prodCode']],how='left')
    return(POData)
    
def convertToPack(dataFrame,Packaging,columnSL,outputColName):
    dataFrame = pd.merge(dataFrame,
                         Packaging[['prodCode','Unit','unitsPerPack']],
                         how='left')
    dataFrame['packAmt'] = pd.to_numeric(dataFrame[columnSL])/dataFrame.unitsPerPack
    dataFrame = dataFrame.rename(columns={'packAmt':outputColName})
    return(dataFrame)

def buildInventorySummary(importLog,saleLog,Packaging):
    # check import and sale integrity
    importLog = convertToPack(importLog,Packaging,'Quantity','importPackAmt')
    importLog = importLog.groupby(['prodCode','Lot'],
                              as_index=False)['importPackAmt'].sum()

    saleLog = convertToPack(saleLog,Packaging,'Amount','salePackAmt')
    saleLog = saleLog.groupby(['prodCode','Lot'],
                              as_index=False)['salePackAmt'].sum()

    inventoryDF = pd.merge(importLog,saleLog)
    inventoryDF['remaining'] = inventoryDF.importPackAmt - \
    inventoryDF.salePackAmt
    inventoryDF.remaining = inventoryDF.remaining.round(decimals=3)
    return(inventoryDF)    

# launch file using default application regardless of OS
def launchFile(fileName):
    if platform.system() == 'Darwin':       # macOS
        subprocess.call(('open', fileName))
    elif platform.system() == 'Windows':    # Windows
        os.startfile(fileName)
    else:                                   # linux variants
        subprocess.call(('xdg-open', fileName))
