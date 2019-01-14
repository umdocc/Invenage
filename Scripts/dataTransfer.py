#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jan 11 14:12:21 2019

@author: umdocc
"""

# transferring old to new coreDB
import sqlite3, pandas as pd

# read old database
connSoft = sqlite3.connect('/Users/umdocc/Dropbox/Desktop/Softanage/CoreData/CoreData.sqlite')
smartGuess = pd.read_sql_query('select * from smartGuess',connSoft)
connSoft.close()

connInv = sqlite3.connect('/Users/umdocc/Dropbox/Invenage/CoreData/CoreData.sqlite')
smartGuess.to_sql('smartGuess',connInv,index=False)
connInv.close()
