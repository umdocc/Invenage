# this script handle import tasks when the excel PO is used for import
# ---------------------- Setup Block ------------------------------------------
import sys, os, numpy as np,  datetime
sys.path.append(os.path.join(os.path.expanduser('~'),'invenage_data'))
from python_conf import create_config_dict
config_dict = create_config_dict()
# add pythonPath

sys.path.append(os.path.join(config_dict['app_path'],'python'))
import python_func as inv
# -----------------------------------------------------------------------------

# process PO _data
inv.update_po_info(config_dict,excluded=['Unused','MuaTrongNuoc'])
poInfo = inv.buildFullPOInfo(configDict,True)

po_data = inv.buildPO_data(poInfo,NSXDict,renameDict,productInfo,Packaging,
                         _dataCleaning=True)

# if po_data contains unrecognised prodcode or Unit, we fix it here
if (len(po_data[po_data.prodCode.isnull()])>0):
    print(po_data[po_data.prodCode.isnull()])
    raise RuntimeError('PO _data contains unrecognised prodCode')
if (len(po_data[po_data.Unit.isnull()])>0):
    print(po_data[po_data.Unit.isnull()])
    po_data[po_data.Unit.isnull()].to_excel('invenageErrorReport.xlsx')
    raise RuntimeError('PO _data contains products without \
                       fundamental packaging information')

# split po_data into 2 parts
# those with Lot gets written into importLog
appendLog = po_data.copy()
appendLog = appendLog[appendLog.Lot!='']
appendLog = inv.checkExists(appendLog,importLog,
                       ['prodCode','Lot','POName','Quantity'])
appendLog = appendLog[appendLog.exist.isnull()]

#at this stage we leave actual import cost blank
appendLog['actualUnitImportCost'] = np.nan
appendLog['importCurrencyCode'] = 1

appendLog = appendLog[['prodCode','Unit','Quantity','POName','Lot','expDate',
                       'actualUnitImportCost','importCurrencyCode']]
# check that Unit is lower case
appendLog.Unit = appendLog.Unit.str.lower()

# add deliveryDate
appendLog['deliveryDate'] = datetime.date.today().strftime('%d%m%y')
# clean up
appendLog.expDate = appendLog.expDate.str.replace(' .*$','')
# append to _database
if len(appendLog)>0:
    print('appending to _database:')
    print(appendLog)
    conn = inv.dbOpen(configDict)
    appendLog.to_sql('importLog',conn,index=False,if_exists='append')
    conn.commit()
    conn.close()

# the remaining of po_data gets written to comingList
comingList = po_data[po_data.Lot=='']
comingList = comingList[['Name','mfgCode','NSX','Quantity','POName']]
conn = inv.dbOpen(configDict)
comingList.to_sql('comingList',conn,index=False,if_exists='replace')
conn.commit()
conn.close()
