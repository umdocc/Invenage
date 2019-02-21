#!/usr/bin/env python3
# The purpose of Ivenage is to build a full feature Inventory/Logistics
# Data Management system using python, R, and sqlite
# Main.py used primarily for data processing
import os, sys

# ------------------------------- Setup ----------------------------------------
# getting appPath
if sys.path[0]!='':
    appPath = sys.path[0]
else:
    appPath = os.path.join(os.path.expanduser('~'),'Dropbox', 'Invenage')

# read the config file and configure paths
sys.path.append(appPath)
from readConfig import getConfigData
configurationData = getConfigData()
scriptPath = configurationData['scriptPath']
coreDBPath = configurationData['coreDBPath']
sys.path.append(scriptPath)

## ---------------------- Main program ------------------------------------------
#
## check and resolve conplaints
#print('Checking for feedback forms')
#import feedbackHandler
#import complaintHandler
#
## process Nhap Kho
#print('Updating inventory import log ', end=' ')
#import processNhapKho
#
## process xuat Kho
#print('processing xuat kho form.............')
#import processXuatKho
#
## checking customer orders
#print('checking customer order.............')
#import checkCustomerOrders
#
## building all View database
#print('basic management completed, distributing databases......')
#import buildOutputDB
#
## check the integrity of masterView
#print('checking system data integrity...........')
#import checkInventory

# process new order if found

# check stock for near zero items and build a createPORequest

# process createPORequest

# process PO that are marked approved


