# this script handle import tasks when the excel PO is used for import
# ---------------------- Setup Block ------------------------------------------
import sys, os, datetime, pandas as pd
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
# add pythonPath and import all functions
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv

# ------------------------------ Data Read ------------------------------------
# variables configuration
error_file = config_dict['error_log']
#database read
conn = inv.db_open(config_dict)
import_log = pd.read_sql_query('select * from import_log',conn)
conn.close()
msg_dict = inv.get_msg_dict(config_dict)

# ------------------------ process PO _data -----------------------------------
po_file_list = inv.get_files_info(config_dict,
                                  config_dict['po_file_ext'],
                                  config_dict['po_path_exclude'].split(';')
                                  )
po_file_list = po_file_list[po_file_list.file_name.str.contains(
        config_dict['po_file_include'])]
po_file_list = po_file_list.reset_index(drop=True)
po_data = inv.build_po_data(po_file_list, config_dict, dataCleaning=True)

# check po_data for integrity
if (len(po_data[po_data.prod_code.isnull()])>0):
    inv.write_log(error_file,msg_dict['process_excel_po'])
    inv.write_log(error_file,msg_dict['unknown_prod'])
    po_data[po_data.prod_code.isnull()].to_csv(
            error_file,index=False,sep='\t',mode='a')
    
if (len(po_data[po_data.unit.isnull()])>0):
    inv.write_log(error_file,msg_dict['process_excel_po'])
    inv.write_log(error_file,msg_dict['unknown_fund_pkg'])
    po_data[po_data.unit.isnull()].to_csv(
            error_file,index=False,sep='\t',mode='a')

# split po_data into 2 parts
# those with Lot gets written into importLog
appendLog = po_data.copy()
appendLog = appendLog[appendLog.lot!='']
appendLog = inv.checkExists(appendLog,import_log,
                       ['prod_code','lot','po_name','qty'])
appendLog = appendLog[appendLog.exist.isnull()]

#at this stage we leave actual import cost blank
appendLog['actual_currency_code'] = 1

appendLog = appendLog[['prod_code','unit','qty','po_name','lot','exp_date',
                       'actual_unit_cost','actual_currency_code']]

# check that Unit is lower case
appendLog.unit = appendLog.unit.str.lower()

# add deliveryDate
appendLog['delivery_date'] = datetime.date.today().strftime('%d%m%y')

# clean up
appendLog.exp_date = appendLog.exp_date.str.replace(' .*$','')

# append to _database
if len(appendLog)>0:
    print('appending to database:')
    print(appendLog)
    conn = inv.db_open(config_dict)
    appendLog.to_sql('import_log',conn,index=False,if_exists='append')
    conn.commit()
    conn.close()

# the remaining of po_data gets written to comingList
comingList = po_data[po_data.lot=='']
comingList = comingList[['name','ref_smn','vendor','qty','po_name']]
conn = inv.db_open(config_dict)
comingList.to_sql('coming_list',conn,index=False,if_exists='replace')
conn.commit()
conn.close()
