# use the base inventory db to build master db
import sqlite3, pandas as pd
from shutil import copyfile

#copy old db to new db
db_orig_file = '/Users/cuongdo/Dropbox/MDSCloud/SQLite/Daov2.sqlite'
db_dest_file = '/Users/cuongdo/Documents/MDS/mds-master.sqlite'
copyfile(db_orig_file,db_dest_file)


conn = sqlite3.connect(db_dest_file)
##rename the tables
#cur = conn.cursor()
#cur.execute('alter table productInfo rename to product_info')
#cur.execute('alter table PXKInfo rename to pxk_info')
#cur.execute('alter table saleLog rename to sale_log')
#cur.execute('alter table guessTable rename to guess_table')
#cur.execute('alter table importLog rename to import_log')
#cur.execute('alter table customerInfo rename to customer_info')
#cur.execute('alter table outputInfo rename to output_info')
#cur.execute('alter table importPrice rename to import_price')
#cur.execute('alter table tenderDetail rename to tender_detail')
#cur.execute('alter table tenderInfo rename to tender_info')
#cur.execute('alter table tenderName rename to tender_name')
#cur.execute('alter table warehouseInfo rename to warehouse_info')
#cur.execute('alter table invoiceType rename to drop_sale_type')
#
#conn.commit()
##rename the columns
#cur.execute('alter table product_info rename column prodCode to prod_code')
#cur.execute('alter table product_info rename column Name to name')
#cur.execute('alter table product_info rename column NSX to vendor')
#cur.execute('alter table product_info rename column mfgCode to ref_smn')
#cur.execute('alter table product_info rename column Type to type')
#cur.execute('alter table product_info rename column packagingStr to packaging_str')
#cur.execute('alter table product_info rename column importLicenseExp to import_license_exp')
#cur.execute('alter table product_info rename column updatedDate to updated_date')
#cur.execute('alter table product_info rename column tenderGroup to prod_group')
#cur.execute('alter table product_info rename column warehouseID to warehouse_id')
#
#cur.execute('alter table pxk_info rename column PXKNum to pxk_num')
#cur.execute('alter table pxk_info rename column saleDate to sale_date')
#cur.execute('alter table pxk_info rename column customerID to customer_id')
#cur.execute('alter table pxk_info rename column PXKType to pxk_type')
#cur.execute('alter table pxk_info rename column Warehouse to warehouse')
#cur.execute('alter table pxk_info rename column completionCode to completed')
#
#cur.execute('alter table sale_log rename column Stt to stt')
#cur.execute('alter table sale_log rename column prodCode to prod_code')
#cur.execute('alter table sale_log rename column Unit to unit')
#cur.execute('alter table sale_log rename column unitPrice to unit_price')
#cur.execute('alter table sale_log rename column Amount to qty')
#cur.execute('alter table sale_log rename column Lot to lot')
#cur.execute('alter table sale_log rename column PXKNum to pxk_num')
#cur.execute('alter table sale_log rename column Note to note')
#cur.execute('alter table sale_log rename column invoiceTypeCode to invoice_type_code')
#
#cur.execute('alter table packaging rename column Unit to unit')
#cur.execute('alter table packaging rename column unitsPerPack to units_per_pack')
#cur.execute('alter table packaging rename column prodCode to prod_code')
#cur.execute('alter table packaging rename column lastUpdated to last_updated')
#
#cur.execute('alter table customer_info rename column customerName to customer_name')
#cur.execute('alter table customer_info rename column customerID to customer_id')
#cur.execute('alter table customer_info rename column Email to customer_email')
#cur.execute('alter table customer_info rename column Address to customer_address')
#cur.execute('alter table customer_info rename column Phone to customer_phone')
#
#cur.execute('alter table import_log rename column prodCode to prod_code')
#cur.execute('alter table import_log rename column Unit to unit')
#cur.execute('alter table import_log rename column Quantity to qty')
#cur.execute('alter table import_log rename column POName to po_name')
#cur.execute('alter table import_log rename column Lot to lot')
#cur.execute('alter table import_log rename column expDate to exp_date')
#cur.execute('alter table import_log rename column actualUnitImportCost to actual_unit_cost')
#cur.execute('alter table import_log rename column importCurrencyCode to actual_currency_code')
#cur.execute('alter table import_log rename column deliveryDate to delivery_date')
#
#cur.execute('alter table import_price rename column prodCode to prod_code')
#cur.execute('alter table import_price rename column importPrice to import_price')
#cur.execute('alter table import_price rename column currencyCode to currency_code')
#cur.execute('alter table import_price rename column minOrder to min_order')
#cur.execute('alter table import_price rename column lastUpdated to last_updated')
#
#cur.execute('alter table tender_detail rename column prodCode to prod_code')
#cur.execute('alter table tender_detail rename column Unit to unit')
#cur.execute('alter table tender_detail rename column unitPrice to unit_price')
#cur.execute('alter table tender_detail rename column tenderAmount to tender_qty')
#cur.execute('alter table tender_detail rename column tenderID to tender_id')
#
#cur.execute('alter table tender_info rename column tenderID to tender_id')
#cur.execute('alter table tender_info rename column tenderName to tender_name')
#cur.execute('alter table tender_info rename column startDate to start_date')
#cur.execute('alter table tender_info rename column endDate to end_date')
#
#cur.execute('alter table tender_name rename column tenderName to tender_name')
#cur.execute('alter table tender_name rename column prodCode to prod_code')
#
#cur.execute('alter table warehouse_info rename column warehouseID to warehouse_id')
#cur.execute('alter table warehouse_info rename column Warehouse to warehouse')
#cur.execute('alter table warehouse_info rename column Description to description')
#cur.execute('alter table warehouse_info rename column PXKName to pxk_name')
#cur.execute('alter table warehouse_info rename column PXKPhone to pxk_phone')
#cur.execute('alter table warehouse_info rename column PXKAddress to pxk_address')
#conn.commit()
#
#
## format the pxk_info
#pxk_info = pd.read_sql_query('select * from pxk_info',conn)
#pxk_info['sale_datetime'] = pd.to_datetime(pxk_info.sale_date.str.zfill(6),
#                                        format='%d%m%y')
#pxk_info = pxk_info.drop(columns='sale_date')
#pxk_info['payment_code'] = 0
#pxk_info = pxk_info.drop(columns=['pxk_type','warehouse'])
#
## update import_log and sale_log to contains warehouse_id
#import_log = pd.read_sql_query('select * from import_log',conn)
#product_info = pd.read_sql_query('select * from product_info',conn)
#sale_log = pd.read_sql_query('select * from sale_log',conn)
#import_log = pd.merge(import_log,product_info[['prod_code','warehouse_id']],
#                      how='left')
#
#sale_log = sale_log.drop(columns=['invoice_type_code'])
#
#### adding warehouse_id to sale_log
#sale_log = pd.merge(sale_log,product_info[['prod_code','warehouse_id']], 
#                    how='left')
#
## remove customerCode from customer_info
#customer_info = pd.read_sql_query('select * from customer_info',conn)
#customer_info = customer_info.drop(columns='customerCode')
#
## writing tables to database
#pxk_info.to_sql('pxk_info',conn,index=False,if_exists='replace')
#sale_log.to_sql('sale_log',conn,index=False,if_exists='replace')
#import_log.to_sql('import_log',conn,index=False,if_exists='replace')
#output_info.to_sql('output_info',conn,index=False,if_exists='replace')
#localisation.to_sql('localisation',conn,index=False,if_exists='replace')
#guess_table.to_sql('guess_table',conn,index=False,if_exists='replace')
#payment_type.to_sql('payment_type',conn,index=False,if_exists='replace')
#customer_info.to_sql('customer_info',conn,index=False,if_exists='replace')

conn.commit()
conn.close()

