# if we have a local import excel file, we handle it here
# ---------------------- Setup Block ------------------------------------------
import sys, os, pandas as pd
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv

# ----------------------------- Data Read -------------------------------------
conn = inv.db_open(config_dict)
productInfo = pd.read_sql_query('select * from product_info',conn)
importLog = pd.read_sql_query('select * from import_log',conn)
rename_dict = inv.create_dict(conn,'colRename')
localisation = pd.read_sql_query('select * from localisation',conn)
conn.close()
error_file = config_dict['error_log']
msg_dict = inv.get_msg_dict(config_dict)
# ---------------------------- Invenage-Python --------------------------------
local_import_str = config_dict['local_import_str'] # the special PO
po_file_list = inv.get_files_info(config_dict,
                                  config_dict['po_file_ext'])
local_import_file = po_file_list.full_path[
        po_file_list.file_name.str.contains(local_import_str)].reset_index(
        drop=True)[0]

start_rows = pd.to_numeric(config_dict['local_import_start_row'])
localImportData = pd.read_excel(local_import_file,skiprows=(start_rows-1),
                                dtype=str)
localImportData = localImportData.rename(columns=rename_dict)
# hack to convert deliveryDate to POName
localImportData['po_name'] = 'localImport.'+                                \
    localImportData.delivery_date.str.replace(' .*$','')

# data cleaning
localImportData['actual_currency_code'] = 1
localImportData.exp_date = localImportData.exp_date.str.replace(' .*$','') 

localImportData.unit = localImportData.unit.str.lower()

# if we compare in non-case sensitive manner, convert to lower case
localImportData.ref_smn = localImportData.ref_smn.str.lower()
localImportData.vendor = localImportData.vendor.str.lower()
productInfo.ref_smn = productInfo.ref_smn.str.lower()
productInfo.vendor = productInfo.vendor.str.lower()

localImportData = pd.merge(localImportData,product_info[
        ['prod_code','vendor','ref_smn']],how='left')
localImportData.unit = localImportData.unit.str.lower()
localImportData.qty = pd.to_numeric(localImportData.qty)

testDF = localImportData[localImportData.prod_code.isnull()] 
if (len(testDF)>0):
    message = 'muaTrongNuoc khong the xac dinh duoc cac sp sau day nen se khong nhap kho!'
    reportFlag = True
    inv.write_log(error_file,message)
    testDF.to_csv(errorLogLoc,index=False,sep='\t',mode='a')
localImportData = localImportData[localImportData.prodCode.notnull()]

# check for database overlap
localImportData = mds.checkExists(localImportData,importLog,['prodCode','Quantity','POName'])
localImportData = localImportData[localImportData.exist.isnull()]

# keeping relevant column
localImportData = localImportData[['prodCode','Unit','Quantity','POName',
                                     'Lot','expDate','actualUnitImportCost',
                                     'importCurrencyCode']]

# writing to database
if len(localImportData)>0:
    conn = mds.dbOpen(config_dict)
    print('adding following items to local import')
    print(localImportData)
    localImportData.to_sql('importLog',conn,index=False,if_exists='append')
    conn.commit()
    conn.close()

if reportFlag:
    mds.launchFile(errorLogLoc)
