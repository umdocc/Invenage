# check the database for missing actual cost price and attemp to fill
# ---------------------- Setup Block ------------------------------------------
import sys, os, pandas as pd
from shutil import copy
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
sys.path.append(os.path.join(config_dict['app_path'],'python'))
home_path = os.path.expanduser('~')
inv_db = os.path.join(home_path,'Dropbox','MDSCloud','SQLite',
                                           'Daov2.sqlite')
master_db = os.path.join(home_path,'Documents','MDS',
                                           'mds-master.sqlite')

copy(inv_db,master_db)

extra_data_path = os.path.join(home_path,'Documents','MDS','extra_data')
invoice_1518 = pd.read_excel(os.path.join(extra_data_path,
                                           'thong_ke_doanh_so_2015-2018.xlsx'))

mconn = sqlite3.connect(master_db)
mconn.close()

# spn
tmp = invoice_1518.copy()
tmp = tmp[tmp.vendor_id==7]
