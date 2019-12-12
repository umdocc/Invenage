# po_tools is a single file to do certain task for each PO
# it can add price to PO, or check exp date
# --------------------- Control Variables -------------------------------------
po_name = 'MDS.DiaSorinSpA.PO.031219.xlsx'
exp_filename = 'Copy of SL_WEEK_49.xlsx'
ext_draft_source = 'dat hang liaison 031219.xlsx'
inventory = 'tonKho031219.xlsx'
if ('DiaSorinInc' in po_name): #currency to filter import_price
    currency_code = 2
if ('DiaSorinSpA' in po_name): #currency to filter import_price
    currency_code = 3
# ---------------------- Setup Block ------------------------------------------
import sys, os, dateutil, pandas as pd, datetime
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
# add pythonPath and import all functions
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv


# ------------------------ path configuration ---------------------------------
#paths
po_path = config_dict['po_path']
po_list = inv.get_files_info(po_path,'xlsx',['Unused','Draft'])
po_file = po_list[po_list.full_path.str.contains(po_name)].reset_index(
        drop=True).full_path[0]
po_dir = os.path.dirname(po_file)
# logs, dicts
error_file = config_dict['error_log']
msg_dict = inv.create_dict(config_dict,'msg_dict')
rename_dict = inv.create_dict(config_dict,'rename_dict')


# ------------------------------ Data Read ------------------------------------
#database
conn = inv.db_open(config_dict)
import_log = pd.read_sql_query('select * from import_log',conn)
product_info = pd.read_sql_query('select * from product_info',conn)
tender_detail = pd.read_sql_query('select * from tender_detail',conn)
tender_name = pd.read_sql_query('select * from tender_name',conn)
import_price = pd.read_sql_query(
        'select * from import_price where currency_code = '+ 
        str(currency_code),conn)
packaging = pd.read_sql_query('select * from packaging',conn)
conn.close()
# po_data
po_data = inv.read_po_data(po_file,config_dict)
po_data = pd.merge(po_data,product_info[['name','ref_smn','prod_code']])
#exp_data
exp_data = pd.read_excel(
        os.path.join(po_dir,exp_filename))
exp_data = exp_data.rename(columns = rename_dict)


# --------------------- add order_price and exp_date to PO --------------------

po_data = inv.add_import_price(po_data,import_price)
po_data[po_data.import_price.isnull()]
po_data = po_data[['prod_code','name','qty','ref_smn','import_price']]

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

# --------------------------- creating report ---------------------------------
po_data.shelf_life_date = po_data.shelf_life_date.dt.strftime('%d-%m-%Y')
output_file_name = os.path.join(po_dir,'po_data_report.xlsx')
po_data.to_excel(os.path.join(po_dir,'order_summary.xlsx'),index=False)
#
## --------------- build draft_po from external source -------------------------
## convert the tender_remain_qty to pack
#tender_detail['tender_remain_qty'] = tender_detail.tender_qty- \
#    tender_detail.sold_qty
#tender_detail = inv.convert_to_pack(
#        tender_detail,packaging,'tender_remain_qty')
#
## read inventory (invenage generated excel)
#inventory = pd.read_excel(os.path.join(po_dir,inventory))
#inventory = pd.merge(
#        inventory, product_info[['prod_code','vendor','ref_smn']], how='left')
#
## read draft_data request and merge
#draft_data = pd.read_excel(os.path.join(po_dir,ext_draft_source),skiprows=2)
#
#draft_data.tender_name = draft_data.tender_name.str.replace('  ',' ')
#draft_data = pd.merge(draft_data,tender_name,how='left')
#draft_data = draft_data[['prod_code','cr_order_pack']]
#draft_data = pd.merge(draft_data,tender_detail,how='left')
#draft_data = pd.merge(
#        draft_data,product_info[['prod_code','name','ref_smn']],how='left')
#draft_data = pd.merge(
#        draft_data,inventory[['prod_code','remaining_qty']],how='left')
##draft_data.to_excel(os.path.join(po_dir,'draft_order.xlsx'),index=False)
