#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# basic configuration functions
import pandas as pd, os

# the getAppPath function, read the config file and build a dictionary
def getConfigData():
    homePath = os.path.expanduser('~')
    configFilePath = os.path.join(homePath,'invenageConf.xlsx')
    if (os.path.exists(configFilePath)):
        # read the config file
        configTable = pd.read_excel(configFilePath,dtype=str)
        configurationData = {}
    # now use relative to user path for all data storage
    
        for i in range(0,len(configTable)):
            configurationData[configTable.Name[i]] = os.path.join(homePath,
                    configTable.Value[i])
    else:
        raise RuntimeError('invenageConf.csv file not found in \
                           home directory!')
        configurationData = {}
    
    return(configurationData)


