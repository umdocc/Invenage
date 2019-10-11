# at the end of the boot sequence, open the error log

# ---------------------------- Setup ------------------------------------------
import sys, os #import
inv_data_path = os.path.join(os.path.expanduser('~'),'invenage_data') 
sys.path.append(inv_data_path)
#build the config_dict
from python_conf import create_config_dict
config_dict = create_config_dict()
# add pythonPath
sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv

error_file = config_dict['error_log']

# open the errorLog if it exists
if os.path.exists(error_file):
    inv.launch_file(error_file)
