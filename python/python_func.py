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

def get_files_info(config_dict,extension,exclude=''):
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
    
    # if exclude is not blank
    if (exclude!=''):
        for folderName in exclude:
    #        print(folderName)
            file_list = file_list[
                    ~file_list.full_path.str.contains(folderName)]
    
    file_list = file_list.reset_index(drop=True)
    file_list['file_name'] = file_list.full_path.apply(os.path.basename)
    return(file_list)

# this function read a PO and detrmine its table location    
def get_po_data_loc(inputExcelFile,headerStr):
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
def create_dict(config_dict, dict_name):
    conn = db_open(config_dict)
    # rename dictionary
    if ((dict_name=='rename_dict') or (dict_name=='vendor_dict')):
        renameDF = pd.read_sql_query(
            'select * from guess_table where guess_type = "'+dict_name+'"',
            conn)
        output_dict = {}
        for i in range(0,len(renameDF)):
            output_dict[renameDF.input_str[i]] = renameDF.output_str[i]

    if (dict_name=='msg_dict'):
        localisation = pd.read_sql_query('select * from localisation',conn)
        localisation = localisation[localisation.app_lang==config_dict['app_lang']]
        output_dict = localisation[localisation.group=='message']
        output_dict = df_to_dict(output_dict,'label','actual')
    conn.close()
    return(output_dict)

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

# the check exists function check if entries in table A already exist in
# table B, it return table A with a column "exists" that mark entries
# already exist in table B
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

def build_po_list(config_dict):
    po_file_list = get_files_info(config_dict,
                                  config_dict['po_file_ext'],
                                  config_dict['po_path_exclude'].split(';')
                                  )
    po_file_list = po_file_list[po_file_list.file_name.str.contains(
        config_dict['po_file_include'])]
    po_file_list = po_file_list.reset_index(drop=True)
    return(po_file_list)
    
def build_po_data(config_dict, data_cleaning=True):
    # create the list of po first
    po_file_list = build_po_list(config_dict)
    
    error_file = config_dict['error_log']
    nsx_dict = create_dict(config_dict,'vendor_dict')
    rename_dict = create_dict(config_dict,'rename_dict')
    msg_dict = create_dict(config_dict,'msg_dict')
    
    # database information
    conn = db_open(config_dict)
    product_info = pd.read_sql_query('select * from product_info',conn)
    packaging = pd.read_sql_query('select * from packaging',conn)    
    conn.close()

    for i in range(0,len(po_file_list)):
        inputExcelFile = po_file_list.full_path[i]
#        print(inputExcelFile)
        currentNSX = ''
        for k in nsx_dict:
            if k in inputExcelFile:
                currentNSX = nsx_dict[k]
        (rLoc,cLoc) = get_po_data_loc(inputExcelFile,'Description')
        tmp = pd.read_excel(inputExcelFile, skiprows = rLoc+1,
                               skipcolumns=cLoc+1,dtype=str)
        tmp['vendor'] = currentNSX
    
        tmp['po_name'] = os.path.basename(inputExcelFile)
    
        tmp = tmp.rename(columns = rename_dict)
        # if we cannot find the actual_unit_cost, set it to ''
        if 'actual_unit_cost' not in tmp.columns:
            tmp['actual_unit_cost'] = ''
            
        tmp = tmp[['name','qty','ref_smn','lot','exp_date',
                   'vendor','actual_unit_cost','po_name']]
        if (i == 0):
            po_data = tmp.copy()
        else:
            po_data = po_data.append(tmp,sort=False)
    po_data.qty = pd.to_numeric(po_data.qty, errors='coerce')
    
    # if vendor is still blank raise error
    report = po_data.copy()
    report = report[report.vendor=='']
    if (len(report)>0):
        print(report)
        raise RuntimeError('error identifying vendor')
        
    # normally we only want to keep things that make sense    
    # blank Lot should be blank
    if data_cleaning:
        po_data = po_data[po_data.qty>0 & po_data.qty.notnull()]
        po_data.loc[po_data.lot.isnull(),'lot'] = ''
        po_data.loc[po_data.lot == 'nan','lot'] = ''
        po_data = po_data.merge(
                product_info[['prod_code','ref_smn','vendor']],how='left')
    # raise error if unknown items found
        test_df = po_data.copy()
        test_df = test_df[test_df.prod_code.isnull()]
        if len(test_df):
            write_log(error_file,msg_dict['process_excel_po'])
            write_log(error_file,msg_dict['unknown_prod'])
            write_log(error_file,msg_dict['data_not_added'])
            test_df.to_csv(error_file,index=False,sep='\t',mode='a')
            po_data = po_data[po_data.prod_code.notnull()]
    
    # add fundamental packaging
    unit_packaging = create_unit_packaging(packaging)
    po_data = pd.merge(po_data,unit_packaging[['unit','prod_code']],how='left')
    # check and remove unknown packaging
    if (len(po_data[po_data.unit.isnull()])>0):
        write_log(error_file,msg_dict['process_excel_po'])
        write_log(error_file,msg_dict['unknown_fund_pkg'])
        write_log(error_file,msg_dict['data_not_added'])
        po_data[po_data.unit.isnull()].to_csv(
                    error_file,index=False,sep='\t',mode='a')
        po_data = po_data[po_data.unit.notnull()]
        
    return(po_data)
    
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
        
# convert a data frame to dictionary, use with caution
def df_to_dict(data_df,key_col,value_col):
    # check the df integirty
    if (len(data_df[data_df[key_col].duplicated()])>0):
        print(data_df[data_df[key_col].duplicated()])
        raise RuntimeError('duplication found in key_col')
    
    # reset data frame index
    data_df = data_df.reset_index(drop=True)
    
    # create the configuration dictionary
    data_dict = {}
    for i in range(0,len(data_df)):
        data_dict[data_df.loc[i,key_col]] = data_df.loc[i,value_col]
    
    return(data_dict)