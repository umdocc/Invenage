# invenage start-up script
# it should be placed in ~/invenageData in both unix and windows
import pandas as pd, os
# ------------------------------- Setup ---------------------------------------
def create_path(path_str,sep=';'):
    output_path = os.path.join(*path_str.split(sep))
    return(output_path)

def rel_to_abs(rel_path,appPath):
    output_path = os.path.join(appPath,rel_path)
    return(output_path)

def create_config_dict():
    config_dict_path = os.path.join(os.path.expanduser('~'),'invenage_data',
                              'invenage_conf.csv')
    if os.path.isfile(config_dict_path): 
        configDF = pd.read_csv(config_dict_path,dtype=str)
    else:
        raise RuntimeError('invenageConf.csv not found!')
    
    # build the paths
    configDF.value[configDF.name.str.contains('path')] = \
    configDF.value[configDF.name.str.contains('path')].apply(create_path)
    app_path = configDF.value[configDF.name=='app_path'].reset_index(
            drop=True)[0]
    configDF.value[configDF.type.str.contains('relative')] = \
    configDF.value[configDF.type.str.contains('relative')].apply(
            rel_to_abs,appPath=app_path)
    
    # create the configuration dictionary
    config_dict = {}
    for i in range(0,len(configDF)):
        config_dict[configDF.name[i]] = configDF.value[i]
    
    return(config_dict)
