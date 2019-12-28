# po_tools is a single file to do certain task for each PO
import sys, os, dateutil, pandas as pd, datetime

# ---------------------- Setup Block ------------------------------------------
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
# add pythonPath and import all functions
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv

# --------------------- Control Variables -------------------------------------
po_name = 'MDS.DiaSorinSpA.PO.211019.xlsx'
exp_filename = 'expiry dates wk 43 less than 6 months.xlsx'
alt_dict_path = os.path.join(
        os.path.dirname(config_dict['db_file']),'rename_dict.xlsx')

# ------------------------------ Data Read ------------------------------------
#database
db_engine = inv.create_db_engine(config_dict)
conn = inv.db_open(config_dict,db_engine)
import_log = pd.read_sql_query('select * from import_log',conn)
product_info = pd.read_sql_query('select * from product_info',conn)
rename_dict = pd.read_sql_query(
        'select input_str,output_str from guess_table \
        where guess_type like "rename_dict"',conn)
direct_import_price = pd.read_sql_query('select prod_code, import_price,  \
                                        min_order from import_price \
                                        where currency_code > 1',conn)
conn.close()
rename_dict = inv.convert_to_dict(rename_dict)
#paths
po_path = config_dict['po_path']
po_list = inv.get_files_info(po_path,'xlsx',['Unused','Draft'])
po_file = po_list[po_list.full_path.str.contains(po_name)].reset_index(
        drop=True).full_path[0]
po_dir = os.path.dirname(po_file)

# logs, dicts
error_file = config_dict['error_log']
msg_dict = inv.create_dict(config_dict,db_engine,'msg_dict')

# # alt dict
# tmp = pd.read_excel(alt_dict_path)
# if (len(tmp)>0):
#     rename_dict = tmp.copy()
# if len(rename_dict[rename_dict.duplicated()])>0:
#     raise RuntimeError('rename_dict contains duplicates')


# excel data
po_data = inv.read_po_data(po_file,config_dict,db_engine)
exp_data = pd.read_excel(
        os.path.join(po_dir,'expiry dates wk 43 less than 6 months.xlsx'))
exp_data = exp_data.rename(columns = rename_dict)

# --------------------------- data cleaning -----------------------------------
# ensure the ref_smn is a string
exp_data.ref_smn = exp_data.ref_smn.astype(str).str.strip()
po_data.ref_smn = po_data.ref_smn.astype(str).str.strip()

# clear the name on exp_data, exp_date on po_data
if 'name' in exp_data.columns:
    exp_data = exp_data.drop(columns='name')

# attach shelf_life_date to po_data and clean exp_date
po_data = pd.merge(po_data,exp_data,how='left')
po_data.shelf_life_date = po_data.shelf_life_date.str.replace('\.','-')
po_data.shelf_life_date = po_data.shelf_life_date.astype(str)

# make the 'nan' date as 07-07-9999, then parse
po_data.shelf_life_date[po_data.shelf_life_date=='nan'] = '07-07-2099'
po_data.shelf_life_date = po_data.shelf_life_date.apply(dateutil.parser.parse)

now = datetime.datetime.now()
po_data['remaining_shelf_days']= (po_data.shelf_life_date - now).dt.days
po_data['remarks'] = 'date_ok'
po_data.loc[po_data.remaining_shelf_days<=183,'remarks'] = 'check_exp_date'

# add import price using min_order
po_data = po_data.merge(
        product_info[['ref_smn','vendor','prod_code']],how='left')
po_data = po_data.merge(direct_import_price,how='left')

# calculate the import_price using min_order
tmp = po_data.copy()
tmp['order_ratio'] = pd.to_numeric(tmp.qty) /           \
pd.to_numeric(tmp.min_order)
tmp = tmp[tmp.order_ratio>1]

# min order_ratio is when qty fit min_order the best
tmp = tmp[['prod_code','order_ratio']].groupby(
        'prod_code',as_index=False).min()
tmp = tmp.rename(columns={'order_ratio':'min_order_ratio'})
po_data = pd.merge(po_data,tmp,how='left')

# --------------------------- creating report ---------------------------------
po_data.shelf_life_date = po_data.shelf_life_date.dt.strftime('%d-%m-%Y')
output_file_name = os.path.join(po_dir,'po_data_report.xlsx')
# po_data.to_excel(output_file_name,index=False)

# clean up
if (config_dict['db_type']=='MariaDB'):
    db_engine.dispose()