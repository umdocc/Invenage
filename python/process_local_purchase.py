# if we have a local import excel file, we handle it here
# ---------------------- Setup Block ------------------------------------------
import sys, os, pandas as pd
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv

# ----------------------------- Data Read -------------------------------------
# database
conn = inv.db_open(config_dict)
product_info = pd.read_sql_query('select * from product_info',conn)
import_log = pd.read_sql_query('select * from import_log',conn)
localisation = pd.read_sql_query('select * from localisation',conn)
packaging = pd.read_sql_query('select * from packaging',conn)
conn.close()

#other
error_file = config_dict['error_log']
msg_dict = inv.create_dict(config_dict,'msg_dict')
rename_dict = inv.create_dict(config_dict,'rename_dict')

# -------------------------- processing ---------------------------------------
local_import_str = config_dict['local_import_str'] # the special PO
po_file_list = inv.get_files_info(config_dict,
                                  config_dict['po_file_ext'])
local_import_file = po_file_list.full_path[
        po_file_list.file_name.str.contains(local_import_str)].reset_index(
        drop=True)[0]

start_rows = pd.to_numeric(config_dict['local_import_start_row'])
local_import_data = pd.read_excel(local_import_file,skiprows=(start_rows-1),
                                dtype=str)
local_import_data = local_import_data.rename(columns=rename_dict)
# hack to convert deliveryDate to POName
local_import_data['po_name'] = 'localImport.'+                                \
    local_import_data.delivery_date.str.replace(' .*$','')

# data cleaning
local_import_data['actual_currency_code'] = 1
local_import_data.exp_date = local_import_data.exp_date.str.replace(' .*$','') 

local_import_data.unit = local_import_data.unit.str.lower()

# if we compare in non-case sensitive manner, convert to lower case
local_import_data.ref_smn = local_import_data.ref_smn.str.lower()
local_import_data.vendor = local_import_data.vendor.str.lower()
product_info.ref_smn = product_info.ref_smn.str.lower()
product_info.vendor = product_info.vendor.str.lower()

local_import_data = pd.merge(local_import_data,product_info[
        ['prod_code','vendor','ref_smn']],how='left')
local_import_data.unit = local_import_data.unit.str.lower()
local_import_data.qty = pd.to_numeric(local_import_data.qty)

# -------------------------- logic checks -------------------------------------
# check for null prod_code
testDF = local_import_data.copy()
testDF = testDF[testDF.prod_code.isnull()] 
if (len(testDF)>0):
    inv.write_log(error_file,msg_dict['process_local_po'])
    inv.write_log(error_file,msg_dict['unknown_prod'])    
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
    local_import_data = local_import_data[
            local_import_data.prod_code.notnull()]

# check for empty delivery date
testDF = local_import_data.copy()
testDF = testDF[testDF.delivery_date.str.contains('nan')] 
if (len(testDF)>0):
    inv.write_log(error_file,msg_dict['process_local_po'])
    inv.write_log(error_file,msg_dict['unknown_delivery_date'])    
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')
local_import_data = local_import_data[
        ~local_import_data.delivery_date.str.contains('nan')]

# check for database existence
local_import_data = inv.checkExists(
        local_import_data,import_log,['prod_code','qty','po_name'])
# keep only non-exist entries
local_import_data = local_import_data[local_import_data.exist.isnull()]

# keeping relevant column
local_import_data = local_import_data[['prod_code','unit','qty','po_name',
                                     'lot','exp_date','actual_unit_cost',
                                     'actual_currency_code']]

# check appending data for unknown packaging, remove unknown packaging
testDF = local_import_data.copy()
testDF = testDF[['prod_code','unit']].drop_duplicates()
testDF = pd.merge(testDF,packaging,how='left')
testDF = testDF[testDF.units_per_pack.isnull()]
if len(testDF)>0:
    inv.write_log(error_file,msg_dict['process_local_po'])
    inv.write_log(error_file,msg_dict['unknown_pkg'])
    testDF.to_csv(error_file,index=False,sep='\t',mode='a')

#remove unknown packaging
testDF['remove'] = True
local_import_data = pd.merge(
        local_import_data,testDF[['prod_code','unit','remove']],
        how='left')
local_import_data = local_import_data[local_import_data.remove.isnull()]
local_import_data = local_import_data.drop(columns='remove')

# writing to database
if len(local_import_data)>0:
    print(msg_dict['add_import'])
    print(local_import_data)
    conn = inv.db_open(config_dict)
    local_import_data.to_sql('import_log',conn,index=False,
                           if_exists='append')
    conn.commit()
    conn.close()
