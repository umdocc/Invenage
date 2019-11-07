# check the database for missing actual cost price and attemp to fill
# ---------------------- Setup Block ------------------------------------------
import sys, os, pandas as pd, sqlite3
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
sys.path.append(os.path.join(config_dict['app_path'],'python'))
home_path = os.path.expanduser('~')
inv_db = os.path.join(home_path,'Dropbox','MDSCloud','SQLite',
                                           'Daov2.sqlite')
master_db = os.path.join(home_path,'Documents','MDS',
                                           'mds-master.sqlite')

# a dictionary to figure out where to find what
extra_df = pd.read_csv(os.path.join(
        home_path,'Documents','MDS','extra_data_info.csv'))
ext_data_dict = {}
for i in range(0,len(extra_df)):
    ext_data_dict[extra_df.name[i]] = extra_df.value[i]
invoice_19 = pd.read_excel(ext_data_dict['invoice_19'], skiprows=2, dtype=str)

# this section read tables from various databases and writing to master

# ------------------ read tables from inventory -------------------------------
iconn = sqlite3.connect(inv_db)
sale_log = pd.read_sql_query('select * from sale_log',iconn)
import_log = pd.read_sql_query('select * from import_log',iconn)
product_info = pd.read_sql_query('select * from product_info',iconn)
iconn.close()

mconn = sqlite3.connect(master_db)
sale_log.to_sql('sale_log',mconn,index=False,if_exists='replace')
import_log.to_sql('import_log',mconn,index=False,if_exists='replace')
mconn.close()


# these get written as one-off
#invoice_1518 = pd.read_excel(ext_data_dict['invoice_1518'])
#mconn = sqlite3.connect(master_db)
#mconn.close()
