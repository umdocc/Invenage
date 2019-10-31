# check the database for missing actual cost price and attemp to fill
# ---------------------- Setup Block ------------------------------------------
import sys, os, pandas as pd, sqlite3
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

# a dictionary to figure out where to find what
extra_df = pd.read_csv(os.path.join(
        home_path,'Documents','MDS','extra_data_info.csv'))
ext_data_dict = {}
for i in range(0,len(extra_df)):
    ext_data_dict[extra_df.name[i]] = extra_df.value[i]
    
invoice_1518 = pd.read_excel(ext_data_dict['invoice_1518'])
invoice_19 = pd.read_excel(ext_data_dict['invoice_19'], skiprows=2, dtype=str)
mconn = sqlite3.connect(master_db)
mconn.close()

# spn
tmp = invoice_1518.copy()
tmp = tmp[tmp.vendor_id==7]
