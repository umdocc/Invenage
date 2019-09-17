#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#Functions for extension scripts
import pandas as pd, sqlite3
import platform, subprocess, glob, os
from sqlalchemy import create_engine
#from sqlalchemy import NullPool
# create a list of files with locked info, can use exclude    
def write_log(error_file,message):
    text_file = open(error_file, "a")
    text_file.write('\n'+message+'\n')
    text_file.close()

def get_files_info(config_dict,extension,exclude):
    if isinstance(exclude,str):
        exclude = exclude.split()
    file_list = pd.DataFrame(glob.glob(os.path.join(config_dict['po_path'],
                                                   '**','*.'+extension), 
                          recursive=True),columns=['full_path'])
    # create the locked column
    locked_list = file_list.copy()
    locked_list = locked_list[locked_list.full_path.str.contains('~\$')]
    locked_list.full_path =locked_list.full_path.str.replace('~\$','')
    locked_list['locked'] = True
    file_list = file_list[~file_list.full_path.str.contains('~\$')]
    file_list = pd.merge(file_list,locked_list,how='left',on='full_path')
    
    for folderName in exclude:
#        print(folderName)
        file_list = file_list[~file_list.full_path.str.contains(folderName)]
    file_list = file_list.reset_index(drop=True)
    file_list['file_name'] = file_list.full_path.apply(os.path.basename)
    return(file_list)

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

# current dictType = ['NSX','colRename']
def create_dict(conn,dictType):
    renameDF = pd.read_sql_query(
            'select * from guess_table where guess_type = "'+dictType+'"',conn)
    # create the rename Dictionary
    renameDict = {}
    # now use relative to user path for all data storage
    for i in range(0,len(renameDF)):
        renameDict[renameDF.input_str[i]] = renameDF.output_str[i]
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

def create_unit_packaging(packaging):
    unit_packaging = packaging.copy()
    unit_packaging = unit_packaging[unit_packaging.units_per_pack==1]
    unit_packaging = unit_packaging[unit_packaging.unit!='pack']
    # if somehow the product has 2 fundamental packaging, use only one
    unit_packaging = unit_packaging[~unit_packaging.prod_code.duplicated()]
    return(unit_packaging)

def update_po_info(config_dict,excluded):
#    app_lang = config_dict['app_lang']
    po_path = config_dict['po_path']
    # read the current po_info from database, remove anything that does not
    # have a valid path
    conn = db_open(config_dict)
    po_info = pd.read_sql_query('select * from po_info',conn)
    # verify fileExists status and remove PO that no longer exists
    if (len(po_info)>0):
        po_info['fileExist'] = po_info['fileLocation'].map(os.path.isfile)
        po_info = po_info[po_info.fileExist]
        po_info = po_info.drop('fileExist',axis=1)
        po_info.to_sql('po_info',conn,index=False,if_exists='replace')
    conn.close()

    
    # update the database with added PO
    po_list = getFilesInfo(po_path,'xlsx',excluded)
    # we need the PO to contains '.PO." string
    po_list = po_list[po_list.fileName.str.contains('\.PO\.')]
    po_list = po_list.rename(columns={'full_path':'fileLocation',
                                    'fileName':'poName'})

    po_list = checkExists(po_list,po_info,['poName','fileLocation'])
    po_append = po_list[po_list.exist.isnull()]
    po_append['poStatusCode'] = 1
    po_append['Note'] = 'added by Invenage'
    po_append = po_append[['poName','poStatusCode','Note','fileLocation']]

    conn = db_open(config_dict)
    po_append.to_sql('po_info',conn,index=False,if_exists='append')
    conn.commit()
    conn.close()
    
def buildFullpo_info(config_dict,existingFileOnly):
    app_lang = config_dict['app_lang']
    conn = db_open(config_dict)
    po_info = pd.read_sql_query('select * from po_info',conn)
    tlsTbl = pd.read_sql_query('select poStatusCode.poStatusCode,  \
                       localisation.Actual as renderedStatus from  poStatusCode  \
                       inner join localisation on  \
                       poStatusCode.Label = localisation.Label where  \
                       localisation.app_lang = "'+app_lang+'"',conn)
    conn.close()
    po_info = po_info.merge(tlsTbl,how='left')
    po_info = po_info[po_info.fileLocation.notnull()].reset_index(drop=True)
    return(po_info)

def build_po_data(po_file_list,config_dict, dataCleaning):
    
    # database information
    conn = db_open(config_dict)
    nsx_dict = create_dict(conn,'NSX')
    rename_dict = create_dict(conn,'colRename')
    product_info = pd.read_sql_query('select * from product_info',conn)
    packaging = pd.read_sql_query('select * from packaging',conn)
    
    conn.close()
    for i in range(0,len(po_file_list)):
        # as paths are relative we need to append homePath
        inputExcelFile = po_file_list.full_path[i]
#        print(inputExcelFile)
        currentNSX = ''
        for k in nsx_dict:
            if k in inputExcelFile:
                currentNSX = nsx_dict[k]
        (rLoc,cLoc) = getPODataLoc(inputExcelFile,'Description')
        tmp = pd.read_excel(inputExcelFile, skiprows = rLoc+1,
                               skipcolumns=cLoc+1,dtype=str)
        tmp['vendor'] = currentNSX
    
        tmp['po_name'] = os.path.basename(inputExcelFile)
    
        tmp = tmp.rename(columns = rename_dict)

        tmp = tmp[['name','qty','ref_smn','lot','exp_date',
                   'vendor','actual_unit_cost','po_name']]
        if (i == 0):
            POData = tmp.copy()
        else:
            POData = POData.append(tmp,sort=False)
    POData.qty = pd.to_numeric(POData.qty, errors='coerce')
    
    # if vendor is still blank raise error
    report = POData.copy()
    report = report[report.vendor=='']
    if (len(report)>0):
        print(report)
        raise RuntimeError('error identifying vendor')
        
    # normally we only want to keep things that make sense    
    # blank Lot should be blank
    if dataCleaning:
        POData = POData[POData.qty>0 & POData.qty.notnull()]
        POData.loc[POData.lot.isnull(),'lot'] = ''
        POData.loc[POData.lot == 'nan','lot'] = ''
        POData = POData.merge(
                product_info[['prod_code','ref_smn','vendor']],how='left')
    # raise error if unknown items found
        if any(POData.prod_code.isnull()):
            print(POData[POData.prod_code.isnull()])
            raise RuntimeError('prod_code not found')
    unit_packaging = create_unit_packaging(packaging)
    POData = pd.merge(POData,unit_packaging[['unit','prod_code']],how='left')
    return(POData)
    
def convertToPack(dataFrame,packaging,columnSL,outputColName):
    dataFrame = pd.merge(dataFrame,
                         packaging[['prod_code','unit','units_per_pack']],
                         how='left')
    dataFrame['packAmt'] = pd.to_numeric(dataFrame[columnSL])/      \
                            dataFrame.units_per_pack
    dataFrame = dataFrame.rename(columns={'packAmt':outputColName})
    return(dataFrame)

def build_inv_df(import_log,sale_log,packaging):
    # check import and sale integrity
    import_log = convertToPack(import_log,packaging,'qty','import_pack_qty')
    import_log = import_log.groupby(['prod_code','lot'],
                              as_index=False)['import_pack_qty'].sum()

    sale_log = convertToPack(sale_log,packaging,'qty','sale_pack_qty')
    sale_log = sale_log.groupby(['prod_code','lot'],
                              as_index=False)['sale_pack_qty'].sum()

    output_df = pd.merge(import_log,sale_log)
    output_df['remaining'] = output_df.import_pack_qty - \
    output_df.sale_pack_qty
    output_df.remaining = output_df.remaining.round(decimals=3)
    return(output_df)    

# launch file using default application regardless of OS
def launch_file(fileName):
    if platform.system() == 'Darwin':       # macOS
        subprocess.call(('open', fileName))
    elif platform.system() == 'Windows':    # Windows
        os.startfile(fileName)
    else:                                   # linux variants
        subprocess.call(('xdg-open', fileName))
