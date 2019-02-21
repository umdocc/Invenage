 # -*- coding: utf-8 -*-
import datetime, os, glob,openpyxl,re, ntpath
import pandas as pd, numpy as np, smtplib

# used for smtplib email
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.mime.base import MIMEBase
from email import encoders

# this function will read and format various LU
def readMDSLU(LUName,masterDataPath):
    #MasterLU data file location
    dataLocation = os.path.join(masterDataPath, 'masterLookups',
                            'MDSMasterLU.xlsx')
    
    #check for correct dataName
    validLU = ['MDSInventoryLU','MDSQuyCachLU','tenderInfo',
               'MDSOrderNameLU','MDSPriceLU','QuyCachNgoaiLU']
    if LUName not in validLU:
        raise ValueError('unknown LUName, valid options are: ' 
                          + ', '.join(validLU))

    # processing the LU read
    if LUName=="MDSInventoryLU":
        Inv_DF = pd.read_excel(dataLocation,sheet_name='InventoryLU',dtype=str)
        Inv_DF = Inv_DF.replace('nan','')
        Inv_DF = cleanMDSData(Inv_DF)
        Inv_DF.importLicenceExp = Inv_DF.importLicenceExp.str.replace(
                ' .*$', '')
        return(Inv_DF)
        
    if LUName=="MDSQuyCachLU":
        LU_DF = pd.read_excel(dataLocation,sheet_name='QuyCachLU')
        LU_DF.Unit = LU_DF.Unit.str.lower()
        return(LU_DF)
    
    if LUName=='MDSOrderNameLU':
        OrderNameDF = pd.read_excel(dataLocation,sheet_name='OrderNameLU')
        OrderNameDF['strLgth'] = OrderNameDF.Name.str.len()
        OrderNameDF = OrderNameDF.sort_values(['strLgth'], ascending=False)
        OrderNameDF = OrderNameDF.drop('strLgth',axis=1)
        return(OrderNameDF)
    
    if LUName=='MDSPriceLU':
        price_DF = pd.read_excel(dataLocation,sheet_name='PriceLU')
        price_DF = cleanMDSData(price_DF)
        return(price_DF)

    if LUName=='tenderInfo':
        tInfo = pd.read_excel(dataLocation,sheet_name='tenderInfo')
        tInfo = cleanMDSData(tInfo)
        return(tInfo)        

def filterSalesData(salesData,rollingMth):
    endDate = salesData.saleDate.max().date()
    begin = endDate - datetime.timedelta(days=(rollingMth*30))
#    beginDate = np.datetime64(datetime.date(begin.year,begin.month,1))
    beginDate = np.datetime64(begin)    
    endDate = np.datetime64(endDate)
    
    # filter date
    salesData = salesData[salesData.saleDate >= beginDate]    
    return(salesData)
        
# the productNameSearch function search for a product using MDSInventory Data and 
# return the single row containing all information related to that product
# its primary use is for adding prodCode or mfgCode to a data frame
def productNameSearch(searchStr,MDSInventoryLU):
    stringList = searchStr.split()
    result = MDSInventoryLU
    for word in stringList:
        tmp = result[result.Name.str.contains(word,case=False)]
        if not tmp.empty:
            result = tmp
    if len(result)==1:
        return(result.prodCode.values[0])
    else:
        return('')
        
# the convertToBox function use QuyCach and a stringSL var to convert a data frame
#   to box
def convertToBox(data_df,MDSQuyCachLU,stringSL):
    data_df = pd.merge(data_df,MDSQuyCachLU,how='left',on=['prodCode','Unit'])
    data_df[stringSL+'Pack'] = data_df[stringSL]/data_df.unitsPerPack
    data_df['packUnit'] = 'pack'
    return(data_df)
    
def writeDataFrameToExcel(dataFrame,excelName):
    writer = pd.ExcelWriter(excelName, engine='xlsxwriter')
    dataFrame.to_excel(writer, sheet_name='Sheet1',index=False)
    writer.save()


# the purpose of this function is to create a data frame of how much
# hospital has bought since tenderStartDate using salesData
# the tenderStartDate needs to be in YYYY-MM-DD
def filterTenderSales(salesData,customerName,tenderStartDate):

    salesDataHospital = salesData[salesData.Customer.str.contains(customerName)]
    tenderStartDate = datetime.datetime.strptime(tenderStartDate, '%d-%m-%Y')

    salesDataHospital = salesDataHospital[
        salesDataHospital.saleDate > tenderStartDate]
  
    return(salesDataHospital)

# the purpose of this function is to remove certain items from PO due to:
#    no longer selling or using different packaging
def removePOItems(PODataFrame):
    # no longer need DiaSorin Starting Kit as we use the XL one
    PODataFrame = PODataFrame[PODataFrame.prodCode!='STA_DIA']
    
    # we dont use this any more    
    PODataFrame = PODataFrame[PODataFrame.prodCode!='FRT_ADV_10309969']
    
    # Kryptor BVCR no longer use Cyfra kit
    PODataFrame = PODataFrame[PODataFrame.prodCode!='CYF_BRA']
    PODataFrame = PODataFrame[PODataFrame.prodCode!='CYF_CAL_BRA']
    PODataFrame = PODataFrame[PODataFrame.prodCode!='CYF_QC_BRA']
    
    # Only CRPP & CHURIN use these and we no longer serve them
    PODataFrame = PODataFrame[PODataFrame.prodCode!='ANT_HBE_DIA']
    PODataFrame = PODataFrame[PODataFrame.prodCode!='HBE_DIA']
    
    # Siemens PCT is very expensive
    PODataFrame = PODataFrame[PODataFrame.prodCode!='CEN_PCT_10378883']
    PODataFrame = PODataFrame[PODataFrame.prodCode!='CEN_ PCT_QC_10378884']
    
    # No longer need these from Spinreact
    # Not needed anymore
    PODataFrame = PODataFrame[PODataFrame.prodCode!='CRP100_SPIN']
    PODataFrame = PODataFrame[PODataFrame.prodCode!='ANTAB_SPIN']
    
    # these from NovaTec are special order only
    PODataFrame = PODataFrame[PODataFrame.prodCode!='TOXPLA_IGG_NOV']
    PODataFrame = PODataFrame[PODataFrame.prodCode!='TOXPLA_IGM_NOV']
    
    #remove special order for Biolabo
    PODataFrame = PODataFrame[PODataFrame.prodCode!='PROU_BIO']
    PODataFrame = PODataFrame[PODataFrame.prodCode!='CRE_BIO']
    PODataFrame = PODataFrame[PODataFrame.prodCode!='AMY20_BIO']
   
    return(PODataFrame)

def cleanColNames(data_df):
    # clean column names, python is much better with unicode        
    colNameDict = {
            'Tên hàng':'Name', 'Tên':'Name', 'Số lượng':'Amount', 'DVT':'Unit',
            'ĐVT':'Unit','Đơn vị':'Unit', 'Số Quyết Định':'QuyetDinh',
            'Ghi Chú':'Note',
            'Giá bán có VAT \(đơn hàng >10000 EUR\)':'priceMita10000EUR',
            'Giá':'Price','Thành tiền':'totalPrice', 'Nhà SX':'NSX','STT':'Stt',
            'Mã hàng':'mfgCode','Đóng gói':'QuyCach','Reference':'mfgCode',
            'Quantity':'Amount'
            }
    for key in colNameDict:
#        print(key)
        data_df.columns.values[pd.Series(data_df.columns).str.contains(
                key, case=False)] = colNameDict[key]
    return(data_df)

def cleanMDSData(data_df):
    
    data_df = cleanColNames(data_df)
    
    # clean the NSX column
    if 'NSX' in data_df.columns.values:
        data_df.NSX = data_df.NSX.replace(np.nan,'',regex=True) #set the nan to ''
        data_df.NSX = data_df.NSX.str.replace('Diasoin','DiaSorin')
        NSXDict = {
                'DRG':'DRG-My','NovaTec':'NovaTec-Duc','Novatec':'NovaTec-Duc',
                'Randox':'Randox-Anh',
                'Brahm':'Brahms-Duc','Far':'Far-Y','Spinreact':'Spinreact-TBN',
                'Biolabo':'Biolabo-Phap','Medica':'Medica-My',
                '77 Elektronika':'77Electronika-EU','Abbott':'Abbott-EU',
                'Siemen':'Siemens-EU','Akray':'Akray-Nhat',
                'Chungdo':'Chungdo-Korea','Sysmex':'Sysmex-Sing',
                '77ele':'77Electronika-EU','Atlas':'Atlas-My',
                'Biome':'Biomedica','Biosu':'Biosup',
                'Mindray':'Mindray-TQ','Audicom':'Audicom-TQ',
                'Fortress':'Fortress-Y','QIAGEN':'Qiagen-EU',
                'Interlab':'Interlab-Y'
                }
        for key in NSXDict:
            data_df.NSX[data_df.NSX.str.contains(key,case= False)] = NSXDict[key]
        
        # DiaSorin need to be handled separately
        # or is there a more elegant way to do this
        data_df.NSX[data_df.NSX.str.contains('Diasorin',case= False)&
                    data_df.NSX.str.contains('Ý|Y|Italy',case= True)] = 'DiaSorin-Y'
        data_df.NSX[data_df.NSX.str.contains('Diasorin',case= False)&
                    data_df.NSX.str.contains('USA|Mỹ|My|US',case= False)] = 'DiaSorin-My'
        data_df.NSX[data_df.NSX.str.contains('Diasorin',case= False)&
                    data_df.NSX.str.contains('Đức|Duc',case= False)] = 'DiaSorin-Duc'
        
    # clean the Name column
    if 'Name' in data_df.columns.values:
        # replace all special characters and double spaces
        data_df['Name'] = data_df['Name'].str.replace('\\(|\\)|\\/',' ')
        data_df['Name'] = data_df['Name'].str.replace('\\n|@|®|™|\.',' ')
        data_df['Name'] = data_df['Name'].str.replace('-|&|,|"',' ')
        data_df['Name'] = data_df['Name'].str.strip()
        data_df.Name = data_df.Name.str.replace('ARC|Architect','Arc',case=False)
        data_df.Name = data_df.Name.str.replace(' xét nghiệm | XN ',' xn ')
     
        
        for i in range(1,5):
            data_df.Name = data_df.Name.str.replace('  ',' ')
        
        #common spelling fix
        spellDict = {
                'Eliisa':'Elisa', 'Chlamydia pneumoniae':'Chlamydia',
                'Amoniac':'Ammoniac', 'Chlorures':'Cholorua',
                'Phosphatse':'Phosphatase', 'Trilyceride':'Triglycerides'}
        
        for key in spellDict:
            data_df.Name = data_df.Name.str.replace(key,spellDict[key])

    # force Unit to lower case
    if 'Unit' in data_df.columns.values:
        data_df['Unit'] = data_df['Unit'].str.lower()
        
    return(data_df)
    
def convertPriceToVND(dataDF,priceString):
    dataDF['exchangeRate'] = np.nan
    
    #echange Rate
    dataDF.loc[dataDF.Currency=='USD','exchangeRate'] = 22800
    dataDF.loc[dataDF.Currency=='EUR','exchangeRate'] = 26800
    dataDF.loc[dataDF.Currency=='GBP','exchangeRate'] = 30500
    dataDF.loc[dataDF.Currency=='VND','exchangeRate'] = 1
    
    dataDF[priceString+'VND'] = dataDF[priceString]*dataDF.exchangeRate
    
    return(dataDF)
    
def formatCustomerData(CustomersDF):

#CustomersDF = CustomersLU.copy()
    CustomersDF = pd.melt(CustomersDF, 
                      value_vars=['xkCustomer1','xkCustomer2','xkCustomer3'],
                      id_vars=['Customer'])
    CustomersDF = CustomersDF[CustomersDF.value!='nan']
    CustomersDF = CustomersDF.rename(columns={'value':'xkCustomer'})
    
    return(CustomersDF)

# the function extract the table location based on header string

def getHeaderLoc(MDSDataFrame,headerStr):
    exitVar=False
    for cLoc in range(0,len(MDSDataFrame.columns)):
        for rLoc in range(0,len(MDSDataFrame)):
            if (MDSDataFrame.iloc[rLoc,cLoc]==headerStr):
                exitVar = True
                break
        if exitVar:
            break
    return(rLoc,cLoc)

# the mergeBack function merge line at index i back to line at index i-1 and 
# reset the index
def mergeBack(pdfDF,i,sep=' '):
    for cIdx in range(0,len(pdfDF.columns)):
        pdfDF.iloc[(i-1),cIdx] = pdfDF.iloc[(i-1),cIdx]+sep+pdfDF.iloc[i,cIdx]
    pdfDF = pdfDF.drop(i)
    pdfDF = pdfDF.reset_index(drop=True)
    return(pdfDF)

def readCustomerOrder(fileName):
    orderDF = pd.read_excel(fileName,dtype=str)
#    if there is no STT or Stt we will attemp to read again
    if not any(orderDF.columns.str.contains('Stt|STT')):
        (rLoc,cLoc) = getHeaderLoc(orderDF,'STT')
        orderDF = pd.read_excel(fileName,dtype=str,skiprows=rLoc+1)
        orderDF = orderDF.iloc[:,cLoc:len(orderDF.columns)]
    
    orderDF[orderDF=='nan'] = ''
    
    #CN-specific cleaning
    orderDF = orderDF[
            orderDF.columns.drop(list(orderDF.filter(
                    regex='Unnamed|tồn|định mức')))]
    orderDF = cleanColNames(orderDF)
    i = 0
    if 'CN' in fileName:
        orderDF['Note'] = 'Anh Tâm'
        orderDF['QuyetDinh'] = 'Chi Nguyet'
        
    # standard MDS cleaning    
    orderDF = cleanMDSData(orderDF)
    
    # PDF-Excel Specific Cleaning
    while (i < len(orderDF)):
        if orderDF.Name[i] == '':
            orderDF = mergeBack(orderDF,i)
        else:
            i = i+1
    orderDF = orderDF[~orderDF.Stt.str.contains('STT')]
    
    # clean Amount column
    orderDF.Amount = orderDF.Amount.str.strip()
    #remove last 2 digits after commas or periods
    orderDF.Amount = orderDF.Amount.str.replace('\.[0-9]{2}$','')  
    orderDF.Amount = orderDF.Amount.str.replace(',[0-9]{2}$','')        
    # remove commas and periods
    orderDF.Amount = orderDF.Amount.str.replace('\.|,','')
    orderDF.Amount = pd.to_numeric(orderDF.Amount)
    
    # if there is a 'LẤY' in note, set LHG to True
    orderDF['LHG'] = False
    orderDF.loc[orderDF.Note.str.contains('LẤY|lấy|Lấy'),'LHG'] = True
    
    # BVCR - identify the area
    orderDF.Note = orderDF.Note.str.lower()
    orderDF['Khu'] = 'SH'
    orderDF.loc[orderDF.Note.str.contains('xnd'),'Khu']='XND'
    orderDF.loc[orderDF.Note.str.contains('hh'),'Khu']='HH'

    orderDF.Unit = orderDF.Unit.str.strip()
    orderDF.Name = orderDF.Name.str.strip()
    
    orderDF.Name = orderDF.Name.str.replace('Architect','Arc', case=False)
    
    #convert Abbott if needed
    abbottDict = {'Amylase':'Amylase 500T', 'Lipase':'Arc Lipase',
                 'Magnesium':'Arc Magnesium', 'Glucose':'Arc Glucose',
                 'Prealbumin':'Arc Prealbumin',
                 'Uric Acid':'Arc Uric Acid',
                 'Creatinine':'CREATININE Reagent kit',
                 'Prealbumin PAlb calibrator':'Arc Prealbumin PAlb calibrator',
                 'CK MB Controls':'Arc CK MB Controls',
                 'Complement 3':'Arc Complement 3',
                 'Complement 4':'Arc Complement 4',
                 'CRP Vario':'Arc CRP Vario',
                 'CRP Calibrator Set':'Arc CRP Calibrator Set'}
    orderDF = orderDF.reset_index(drop=True)
    for i in range(0,len(orderDF)):
#        print(i)
        if (orderDF.Khu[i]=='XND'):
            for key in abbottDict:
                if (orderDF.Name[i]==key):
                    orderDF.loc[i,'Name'] = abbottDict[key]
    
    orderDF = orderDF[orderDF.Name!='']
    
    return(orderDF[['Stt','Name','Unit','Amount','Note','LHG','QuyetDinh',
                    'Khu']])

# in case we need to read sales data directly from old system excel output
def readExcelSalesData(fileName):
    salesDF = pd.read_excel(fileName,dtype=str,skiprows=1)
    salesDF = cleanMDSData(salesDF)
    
# custom clean up for Abbott Data
    salesDF = salesDF.rename({'Số phiếu':'Contract','Mã KH':'Customer',
                              'Ngày ghi sổ':'fullProdName'},axis=1)
    salesDF.loc[salesDF.Customer=='CHORAY','Customer'] = 'CR-BV Chợ Rẫy'
    salesDF['saleDate'] = salesDF.fullProdName
# create and fill the fullProdName column
    for i in range(0,len(salesDF)):
        if salesDF.Customer[i] == 'nan':
            currentName = salesDF.fullProdName[i]
        else:
            salesDF.fullProdName[i] = currentName
    # filter on Customer
    salesDF = salesDF[salesDF.Customer!='nan']
    
    # we will need to perform custom action to get the prodCode
    # return for now
    salesDF = salesDF[['fullProdName','Contract','Customer','Unit','Amount',
                       'saleDate']]
    return(salesDF)

def createPOList(POListPath):
    # create PO List
    POList = pd.Series(glob.glob(os.path.join(POListPath,'**','*.xlsx'), 
                          recursive=True))
    # remove files that have 'Unused' or 'POList' in their path
    POList = POList[~POList.str.contains('Unused|POList')]
    POList = POList[POList.str.contains('\.PO\.')]
    # remove temp files that start with ~$
    POList = POList[~POList.str.contains('~\$')]
    POList = POList.reset_index(drop=True)
    return(POList)

# this fuction guess the NSX and output the standard format
    # it will only take a string and scan for known
def guessFromStr(FullString,NSXDict,guessType):
    outputStr = ''
    for j in range(0,len(NSXDict)):
        if NSXDict.String[j] in FullString:
            outputStr = NSXDict[guessType][j]
    
    # if all is good return the data
    if outputStr == '':
        raise RuntimeError('cannot guess the Vendor or NSX from '+FullString)
    else:
        return(outputStr)

# we read from the dB as strings and format them independently
def formatDBData(dataDF,dataName):
#    the salesData requires the saleDate and amount column
    if dataName == 'salesData':
        dataDF.saleDate = dataDF.saleDate.str.replace(' 00:00:00','')
        dataDF.saleDate = pd.to_datetime(dataDF.saleDate,format = '%Y-%m-%d')
        dataDF.unitAmt = pd.to_numeric(dataDF.unitAmt)
    if dataName == 'tenderData':
        dataDF.Amount = pd.to_numeric(dataDF.Amount)        
    return(dataDF)

#this function calculate how many packs sold from fromDate
def summariseSalesData(salesDataSum,MDSQuyCachLU,fromDate,customer):
    fromDate = np.datetime64(pd.to_datetime(fromDate,format = '%d-%m-%Y'))
    salesDataSum = salesDataSum[salesDataSum.saleDate >= fromDate] 
    salesDataSum = convertToBox(salesDataSum,MDSQuyCachLU,'unitAmt')
    salesDataSum = salesDataSum[salesDataSum.Customer.str.contains(customer)]
    salesDataSum = salesDataSum.groupby(
            ['prodCode','packUnit'])['unitAmtPack'].sum().reset_index()
    return(salesDataSum)
    
def getMonthlySales(salesData,MDSQuyCachLU,rollingMth,method):
    # filter on time
    salesDataFull = salesData.copy()
    salesDataFull.unitAmt = pd.to_numeric(salesDataFull.unitAmt)
    salesDataFull = convertToBox(salesDataFull,MDSQuyCachLU,'unitAmt')
    salesData = filterSalesData(salesDataFull,rollingMth)
    #convert to box and summarise
    salesData = salesData.groupby(['prodCode','packUnit'],as_index=False)[
        ['prodCode','packUnit','unitAmtPack']].sum()
    if (method=='average'):
        salesData['aveSalesMth'] = salesData.unitAmtPack/rollingMth
        
    return(salesData[['prodCode','packUnit','aveSalesMth']])

# this function check a dataFrame and issue any complaints that require feedback
def requestFeedback(dataFrame,feedbackPath):
    complaint = dataFrame.copy()

    # guess the type of feedback required    
    # missing packaging data
    if len(complaint[complaint.unitsPerPack.isnull()])>0:
        complaint = complaint[complaint.unitsPerPack.isnull()][['prodCode',
                    'Unit','unitsPerPack']]
        complaint = complaint.drop_duplicates()
        complaint.to_excel(os.path.join(feedbackPath,
                                    'packagingMissing.xlsx'),index=False)
        raise RuntimeError('convert to box failed, check packaging data!!!')

def createSalesReport(salesFrame,packaging,rollingMth):
    salesFrame.unitAmt = pd.to_numeric(salesFrame.unitAmt)
    salesFrame = convertToBox(salesFrame,packaging,'unitAmt')
    salesFrame = filterSalesData(salesFrame,rollingMth)

    #convert to box and summarise
    salesFrame = salesFrame.groupby(['prodCode','packUnit'],as_index=False)[
        ['prodCode','packUnit','unitAmtPack']].sum()
    return(salesFrame)

# writing the xuatKhoDict to output
def writeXuatKhoDict(xuatKhoDict,xkCustomers,xuatKhoFormPath):
    xuatKhoDict[xuatKhoDict=='nan'] = ''
    xuatKhoDict['PXK'] = 'pxk'
    xuatKhoDict['saleDate'] = ''
    xuatKhoDict['Unit'] = ''
    xuatKhoDict['Amount'] = ''
    xuatKhoDict['Note'] = ''
    xuatKhoDict['xkCustomer'] = ''
    xuatKhoDict.loc[0:len(xkCustomers),'xkCustomer'] = xkCustomers.xkCustomer[
            0:len(xkCustomers)]
    
    lastLine = xuatKhoDict.copy()
    lastLine = xuatKhoDict[xuatKhoDict.Name.str.contains(
            'ten-ghi-tren-phieu')].reset_index(drop=True)
    lastLine.loc[0,'saleDate'] = 'ngay-xuat'
    lastLine.loc[0,'Unit'] = 'don-vi'
    lastLine.loc[0,'Amount'] = 'so-luong'
    lastLine.loc[0,'Note'] = 'ghi-chu'        
    lastLine.loc[0,'xkCustomer'] = 'ten-khach-hang' 
    
    xuatKhoDict = xuatKhoDict[~xuatKhoDict.Name.str.contains(
            'ten-ghi-tren-phieu')]
    xuatKhoDict = xuatKhoDict.append(lastLine,sort=False)
    xuatKhoDict = xuatKhoDict[['PXK','saleDate','xkCustomer','Name','Unit',
                               'Amount','Note']].reset_index()
#    xuatKhoForm = openpyxl.load_workbook(xuatKhoFormPath)
#    sheet = xuatKhoForm.active
#    for i in range(0:len(xuatKhoDict)):
#        for j in 
#    sheet.cell(row=[2:3],column=[2:3]).value = 1
    
    xuatKhoDict.to_excel(xuatKhoFormPath,index=False)

# recursively build a list of files with info
def getFilesInfo(filePath,extension):
    fileList = pd.DataFrame(glob.glob(os.path.join(filePath,'**','*.'+extension), 
                          recursive=True),columns=['fileName'])
    # create the locked column
    lockedList = fileList.copy()
    lockedList = lockedList[lockedList.fileName.str.contains('~\$')]
    lockedList.fileName =lockedList.fileName.str.replace('~\$','')
    lockedList['locked'] = True
    fileList = fileList[~fileList.fileName.str.contains('~\$')]
    fileList = pd.merge(fileList,lockedList,how='left',on='fileName')
    
    return(fileList)

#create inventory report using masterViewDatabase
def createInventoryReport(masterViewPath,rollingMth):
    conn = sqlite3.connect(masterViewPath)

    # import required LU
    packaging = pd.read_sql_query(
            'select prodCode,Unit,unitsPerPack from bangQuyCach',conn)
    
    # read sales data and tenderData
    salesData = pd.read_sql_query('select * from chiTietXuatKho',conn,
                                  parse_dates={'saleDate':"%Y-%m-%d %H:%M:%S"})
   
    # read coming list
    comingList = pd.read_sql_query(
            'select prodCode, Name, sum(comingQty) as comingQty             \
            from hangSapVe group by prodCode', conn)
    tonKhoData = pd.read_sql_query('select * from tonKho',conn)
    conn.close()
    
    # remove customers that we no longer serve
    salesData = salesData[~salesData.Customer.str.contains(
            'CHURIN-|CRPP-|NANO-|DELTA-')]
    
    # create new banLe data
    banLe = salesData.copy()
    banLe = banLe[~banLe.Customer.str.contains('CR-BV')]
    banLe = createSalesReport(salesData,packaging,rollingMth)
    
    # filter on time
    salesData = createSalesReport(salesData,packaging,rollingMth)
    
    # merge tonKho with salesData
    tonKhoForecast = tonKhoData.merge(
            salesData[['prodCode','packUnit','unitAmtPack']],how='outer',
                                      on=['prodCode','packUnit'])
    tonKhoForecast['aveSalesMth'] = tonKhoForecast.unitAmtPack/rollingMth
    tonKhoForecast['mthSupplyLeft'] = tonKhoForecast.SLKhoPack/              \
                                        tonKhoForecast.aveSalesMth
    return(tonKhoForecast)

# writing PO Informations
def writePOInfo(orderDataFrame,requiredStockLevels,intRef):

    # create a PO Info Data Frame that match the order frame
    POInfo = orderDataFrame.copy()
    while len(POInfo)<5:
        POInfo = POInfo.append(POInfo).reset_index(drop=True)
    POInfo = POInfo.iloc[0:5,].reset_index(drop=True) 
    POInfo.iloc[:,:] = ''
    
    # writing codes
    POInfo.Name[1] = 'requiredStockLevelMths'
    POInfo.SLMua[1] = requiredStockLevels
    POInfo.Name[2] = 'intRef'
    POInfo.SLMua[2] = intRef
    POInfo.Name[3] = 'PODate'
    POInfo.SLMua[3] = 'dd-mm-yyyy'
    POInfo.Name[4] = 'approvalStatus'
    POInfo.SLMua[4] = 'waiting'

    
    orderDataFrame = orderDataFrame.append(POInfo)
    return(orderDataFrame)

def writePOForm(POFrame,vendorName,PODate,intRef,formsPath,outputPath):
    # read and write the PO    
    poForm = openpyxl.load_workbook(
        os.path.join(formsPath, vendorName + '.PO.Form.xlsx'))
    sheet = poForm.active
    # to keep things simple all PO should have the same standard Info
    sheet.cell(row=6,column=2).value = PODate
    sheet.cell(row=6,column=4).value = PODate
    # IntRef should be below the Date
    sheet.cell(row=7,column=4).value = intRef
    
    # writing cell values
    for k in range(0,len(POFrame)):
        sheet.cell(row=(k+23),column=1).value = (k+1)
        sheet.cell(row=(k+23),column=2).value = POFrame.Name.iloc[k]
        sheet.cell(row=(k+23),column=3).value = POFrame.Quantity.iloc[k]
        sheet.cell(row=(k+23),column=4).value = POFrame.mfgCode.iloc[k]
        if 'Packaging' in POFrame.columns:
            sheet.cell(row=(k+23),column=5).value = POFrame.Packaging.iloc[k]
    
    # create the Path and Save the PO
    if not os.path.isdir(outputPath):
        os.makedirs(outputPath)
    poForm.save(os.path.join(outputPath,vendorName+'.PO.'+PODate+'.xlsx'))

# a standard protocol to sending email
def emailPO(POFileLocation,serverName,sender,pswd):
    # recipient
    recipient = 'Cuong Do<umdocc@gmail.com>'
    
    # recover the fileName
    POFileName = ntpath.basename(POFileLocation)
    PODate = re.sub('.xlsx','',POFileName)
    PODate = re.sub('^.*\.','',PODate)

    # message header
    msg = MIMEMultipart('alternative')
    msg['Subject'] = 'PO '+ PODate
    msg['From'] = sender
    msg['To'] = recipient

    server = smtplib.SMTP_SSL("mail9040.maychuemail.com",port=465)
    server.login(sender, pswd)
    
    text = "Hi Cuong!\nHere is your ASX updates:\n"
    part1 = MIMEText(text, 'plain')
    
    part2 = MIMEBase('application', "octet-stream")
    part2.set_payload(open(POFileLocation, "rb").read())
    encoders.encode_base64(part2)
    headerPart = 'attachment; filename="'+POFileName+'"'
    part2.add_header('Content-Disposition', headerPart)
    
    msg.attach(part1)
    msg.attach(part2)
    
    server.sendmail(sender, recipient, msg.as_string())
    server.quit()

# standard column rename procedures
def createRenameDict(dictType):
    if (dictType == 'colNamesDict'):
        renameDict = {'STT':'Stt','Tên':'Name','ĐVT':'Unit','Giá':'unitPrice',
                    'Số lượng':'Amount','Thành tiền':'totalPrice',
                    'Nơi sản xuất':'NSX'}
    return(renameDict)

def createTenderReport(salesData,tenderName,tenderDetails):
    return(tenderDetails)

# the function split ckmb control set into 2 separated boxes
def ckmbSplit(inputDataFrame,amtStr):
    # This Special cleaning needs to be a fuction
#    inputDataFrame = CRTenderInvitation.copy()
    # we need to raise error if the column is not numeric
#    if (inputDataFrame[amtStr].dtype !='float64'):
#        raise RuntimeError('CK MB split hack failed as the df column          \
#                           is not numeric')
#        
#    # split the ck mb qc entry
#    ckmbCtrl1 = inputDataFrame.copy()
#    ckmbCtrl1 = ckmbCtrl1[
#        ckmbCtrl1.prodCode=='HDLLDL_CKMB_QC12'].reset_index(drop=True)
#    currentName = ckmbCtrl1.Name[0]
#    ckmbCtrl1.Name[0] = currentName + ' 1'
#    for i in range(0,len(amtStr)):
#        ckmbCtrl1[amtStr[i]][0] = ckmbCtrl1[amtStr][0]/2
#    ckmbCtrl2 = ckmbCtrl1.copy()
#    ckmbCtrl2.Name[0] = currentName + ' 2'
#    inputDataFrame = inputDataFrame[
#        inputDataFrame.prodCode!='HDLLDL_CKMB_QC12'].reset_index(drop=True)
    
    return(inputDataFrame)

#when we want to add tender control, use a function
#def addTenderControl(inputFrame,salesData,tenderInfo,tenderDetails)
#salesDataSum = summariseSalesData(salesData,bangQuyCach,fromDate='06-12-2017',
#                                  customer = 'CR-')
#tenderData = convertToBox(tenderData,bangQuyCach,'Amount')
#tenderData = pd.merge(tenderData,salesDataSum, 
#                      how = 'left',on = ['prodCode','packUnit'])
#
## we will remove unrecognised items and set null sales to 0 for now
#tenderData = tenderData[~tenderData.AmountPack.isnull()]
#tenderData.loc[tenderData.unitAmtPack.isnull(),'unitAmtPack'] = 0
##calculate tenderRemain and add to Order
#tenderData['tenderRemain'] = tenderData.AmountPack-tenderData.unitAmtPack
#Order = pd.merge(Order,tenderData[['prodCode','tenderRemain']],how='left',
#                 on='prodCode')
#
## update the Note section
#Order.loc[Order.packAmount>Order.tenderRemain,'Note'] = 'Vuot Thau'
#Order.loc[Order.tenderRemain.isnull(),'Note'] = 'Khong Thau'
#
## check backOrder information for Abbott
#hangSapVe = hangSapVe[hangSapVe.NSX=='Abbott-EU']
