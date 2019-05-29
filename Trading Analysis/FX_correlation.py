import blpapi
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import matplotlib.pyplot as plt
from itertools import permutations, product
import matplotlib.dates as mdates
from scipy.stats import ttest_1samp
from matplotlib.backends.backend_pdf import PdfPages
import sys
from mpl_finance import candlestick_ohlc
import six
from PyPDF2 import PdfFileMerger, PdfFileReader
import os

#Bloomberg Wrapper Script adapted from Github
#Used for extracting and making API requests to Bloomberg
class RequestError(Exception):
    """A RequestError is raised when there is a problem with a Bloomberg API response."""
    def __init__ (self, value, description):
        self.value = value
        self.description = description
        
    def __str__ (self):
        return self.description + '\n\n' + str(self.value)


class BLPInterface:
    def __init__ (self, host='localhost', port=8194, open=True):
        self.active = False
        self.host = host
        self.port = port
        if open:
            self.open()
        
    def open (self):
        if not self.active:
            sessionOptions = blpapi.SessionOptions()
            sessionOptions.setServerHost(self.host)
            sessionOptions.setServerPort(self.port)
            self.session = blpapi.Session(sessionOptions)
            self.session.start()
            self.session.openService('//BLP/refdata')
            self.refDataService = self.session.getService('//BLP/refdata')
            self.active = True
    
    def close (self):
        if self.active:
            self.session.stop()
            self.active = False

    def historicalRequest (self, securities, fields, startDate, endDate, **kwargs):
        defaults = {'startDate'       : startDate,
            'endDate'                 : endDate,
            'periodicityAdjustment'   : 'ACTUAL',
            'periodicitySelection'    : 'DAILY',
            'nonTradingDayFillOption' : 'ACTIVE_DAYS_ONLY',
            'adjustmentNormal'        : False,
            'adjustmentAbnormal'      : False,
            'adjustmentSplit'         : True,
            'adjustmentFollowDPDF'    : False}   
        defaults.update(kwargs)

        response = self.sendRequest('HistoricalData', securities, fields, defaults)
        
        data = []
        keys = []
        
        for msg in response:
            securityData = msg.getElement('securityData')
            fieldData = securityData.getElement('fieldData')
            fieldDataList = [fieldData.getValueAsElement(i) for i in range(fieldData.numValues())]
            
            df = pd.DataFrame()
            
            for fld in fieldDataList:
                for v in [fld.getElement(i) for i in range(fld.numElements()) if fld.getElement(i).name() != 'date']:
                    df.ix[fld.getElementAsDatetime('date'), str(v.name())] = v.getValue()

            df.index = pd.to_datetime(df.index)
            df.replace('#N/A History', np.nan, inplace=True)
            
            keys.append(securityData.getElementAsString('security'))
            data.append(df)
        
        if len(data) == 0:
            return pd.DataFrame()
        if type(securities) == str:
            data = pd.concat(data, axis=1)
            data.columns.name = 'Field'
        else:
            data = pd.concat(data, keys=keys, axis=1, names=['Security','Field'])
            
        data.index.name = 'Date'
        return data
        
    def referenceRequest (self, securities, fields, **kwargs):
        response = self.sendRequest('ReferenceData', securities, fields, kwargs)
        
        data = pd.DataFrame()
        
        for msg in response:
            securityData = msg.getElement('securityData')
            securityDataList = [securityData.getValueAsElement(i) for i in range(securityData.numValues())]
            
            for sec in securityDataList:
                fieldData = sec.getElement('fieldData')
                fieldDataList = [fieldData.getElement(i) for i in range(fieldData.numElements())]
                
                for fld in fieldDataList:
                    data.ix[sec.getElementAsString('security'), str(fld.name())] = fld.getValue()
        
        if data.empty:
            return data
        else: 
            data.index.name = 'Security'
            data.columns.name = 'Field'
            return data.iloc[0,0] if ((type(securities) == str) and (type(fields) == str)) else data
        
    def bulkRequest (self, securities, fields, **kwargs):
        response = self.sendRequest('ReferenceData', securities, fields, kwargs)

        data = []
        keys = []
        
        for msg in response:
            securityData = msg.getElement('securityData')
            securityDataList = [securityData.getValueAsElement(i) for i in range(securityData.numValues())]
            
            for sec in securityDataList:
                fieldData = sec.getElement('fieldData')
                fieldDataList = [fieldData.getElement(i) for i in range(fieldData.numElements())]
                
                df = pd.DataFrame()
                
                for fld in fieldDataList:
                    for v in [fld.getValueAsElement(i) for i in range(fld.numValues())]:
                        s = pd.Series()
                        for d in [v.getElement(i) for i in range(v.numElements())]:
                            s[str(d.name())] = d.getValue()
                        df = df.append(s, ignore_index=True)

                if not df.empty:
                    keys.append(sec.getElementAsString('security'))
                    data.append(df.set_index(df.columns[0]))
                    
        if len(data) == 0:
            return pd.DataFrame()
        if type(securities) == str:
            data = pd.concat(data, axis=1)
            data.columns.name = 'Field'
        else:
            data = pd.concat(data, keys=keys, axis=0, names=['Security',data[0].index.name])
            
        return data
        
    def sendRequest (self, requestType, securities, fields, elements):
        request = self.refDataService.createRequest(requestType + 'Request')
        
        if type(securities) == str:
            securities = [securities]
        if type(fields) == str:
            fields = [fields]
        
        for s in securities:
            request.getElement("securities").appendValue(s)
        for f in fields:
            request.getElement("fields").appendValue(f)
        for k, v in elements.items():
            if type(v) == datetime:
                v = v.strftime('%Y%m%d')
            request.set(k, v)
            
        self.session.sendRequest(request)

        response = []
        while True:
            event = self.session.nextEvent(100)
            for msg in event:
                if msg.hasElement('responseError'):
                    raise RequestError(msg.getElement('responseError'), 'Response Error')
                if msg.hasElement('securityData'):
                    if msg.getElement('securityData').hasElement('fieldExceptions') and (msg.getElement('securityData').getElement('fieldExceptions').numValues() > 0):
                        raise RequestError(msg.getElement('securityData').getElement('fieldExceptions'), 'Field Error')
                    if msg.getElement('securityData').hasElement('securityError'):
                        raise RequestError(msg.getElement('securityData').getElement('securityError'), 'Security Error')
                
                if msg.messageType() == requestType + 'Response':
                    response.append(msg)
                
            if event.eventType() == blpapi.Event.RESPONSE:
                break
                
        return response

    def __enter__ (self):
        self.open()
        return self
        
    def __exit__ (self, exc_type, exc_val, exc_tb):
        self.close()

    def __del__ (self):
        self.close()
        
class correlation_func:
    
    def __init__(self, data_1, data_2):    
        self.pairs_1 = data_1
        self.pairs_2 = data_2
        self.date_range = list(range(500, len(self.pairs_1)-250,21))
        self.periods = list(range(100, 500, 10))
        self.result = []
    
    def random_search(self):
        global correlation_threshold
        for x,y in product(self.periods, self.date_range):
            corr = self.correlation_pair([x,y])
            if corr >= correlation_threshold:
                self.result.append([int(y-x), int(y), corr]) 
        return(self.result)

    def correlation_pair(self, x):
        x = [int(i) for i in x]
        period, date = x
        
        org_pairs = np.array(self.pairs_1).T[:,-(period+1):-1]
        lagged_pairs = np.array(self.pairs_2).T[:,(date-period):date]
        combined = list(zip(org_pairs, lagged_pairs))
        correl = list(map(lambda z: np.corrcoef(z[0],z[1])[0,1], combined))
        return(np.mean(correl))

def render_mpl_table(data, col_width=2.0, row_height=0.4, font_size=12,
                     header_color='#40466e', row_colors=['#f1f1f2', 'w'], edge_color='w',
                     bbox=[0, 0, 1, 1], header_columns=0,
                     ax=None, **kwargs):
    global pdf
    
    if ax is None:
        #size = (np.array(data.shape[::-1]) + np.array([0, 1])) * np.array([col_width, row_height])
        fig, ax = plt.subplots(figsize=(30,22))
        ax.axis('off')
    data['Chart'] = data['Chart'].str.wrap(22,replace_whitespace=True)
    mpl_table = ax.table(cellText=data.values, bbox=bbox, 
                         colLabels=data.columns.str.wrap(16, replace_whitespace=True), 
                         **kwargs)
    plt.title('Summary Table', fontsize = 36)
    mpl_table.auto_set_font_size(False)
    mpl_table.set_fontsize(font_size)
    plt.subplots_adjust(left=0.1, right=0.9, top=0.9, bottom=0.1)

    for k, cell in  six.iteritems(mpl_table._cells):
        cell.set_edgecolor(edge_color)
        if k[0] == 0 or k[1] < header_columns:
            cell.set_text_props(weight='bold', color='w')
            cell.set_facecolor(header_color)
        else:
            cell.set_facecolor(row_colors[k[0]%len(row_colors)])
    
    mng = plt.get_current_fig_manager()
    mng.full_screen_toggle()
    pdf.savefig(fig,bbox_inches='tight',dpi=100)
    plt.close()

#Finding all possible combinations of pairs
def correlation(tick_data):  
    global dates
    global pdf
    global summary_table
    global graph_table
    global methods
    
    order = ['PX_OPEN', 'PX_HIGH', 'PX_LOW', 'PX_LAST']
    dataset_1 = address[tick_data[0]].fillna(method='ffill').dropna()
    dataset_2 = address[tick_data[1]].fillna(method='ffill').dropna()
    dataset_1 = dataset_1[order]
    dataset_2 = dataset_2[order]
    dates = dataset_1.index
    if method == 'Comparison':
        name = tick_data[1] + ' (since '+ dataset_2.index[0].strftime("%Y-%m-%d") +') leads ' + tick_data[0] + ' (since ' + dataset_1.index[0].strftime("%Y-%m-%d") + ')'
        chart_name = tick_data[1] + ' leads ' + tick_data[0]
    elif method == 'Self':
        name = tick_data[0] + ' (since '+ dataset_1.index[0].strftime("%Y-%m-%d") +')'
        chart_name = tick_data[1] + ' leads ' + tick_data[0]
        
    #Running correlation on time series of pair
    cf = correlation_func(dataset_1, dataset_2)
    try:
        result = pd.DataFrame(np.array(cf.random_search()), columns = ['Start Date', 'End Date','Correlation']).drop_duplicates(subset=['End Date'], keep='first', inplace=False)         
        result = result.drop_duplicates(subset=['Start Date'], keep='first', inplace=False)  
    except ValueError:
        print('No Correlated Periods Found')
        sys.exit(1)
    
    #Extracting extrapolated forecast days from End Date
    stats = np.zeros((result.shape[0],forecast_period))
    
    for i in range(0,result.shape[0]):
        stats[i] = np.array(dataset_1.iloc[int(result.iloc[i,1]):int(result.iloc[i,1]+forecast_period),-1]).T
    
    #Calculating Returns and plots for graphs
    returns = np.zeros((4,forecast_period))
    stats = (stats.T / stats[:,0])

    returns[0] = np.mean(stats,axis=1)
    returns[1] = np.max(stats,axis=1)
    returns[2] = np.min(stats,axis=1)
    returns[3] = np.median(stats,axis=1)
    
    returns = returns*dataset_1['PX_LAST'][-1]
    
    #Appending past 180 days of ticker to extrapolated arrays
    original = np.zeros((4,180))
    original[0:4] = np.array(dataset_1.iloc[-180:].values).T
    
    prices = np.concatenate((original, returns), axis=1)
    last_date = list(dataset_1.index[-180:])
    for i in range(1,forecast_period+1):
        last_date.append(pd.Timestamp((last_date[179].to_pydatetime() + timedelta(days=i))))#.strftime("%Y-%m-%d"))
            
    #Calculating values for statistics table
    prices = pd.DataFrame(prices.T, index = last_date)
    result['Start Date'] = result['Start Date'].astype('int')
    result['End Date'] = (result['End Date'] + forecast_period).astype('int')
    result = result.sort_values(['Correlation'], ascending=False)
    
    tstat_d5 = str(round(ttest_1samp(stats[0:5,:].flatten(), popmean = 1).statistic,3))
    tstat_d10 = str(round(ttest_1samp(stats[0:10,:].flatten(), popmean = 1).statistic,3))
    tstat_d20 = str(round(ttest_1samp(stats[0:20,:].flatten(), popmean = 1).statistic,3))
    tstat_d50 = str(round(ttest_1samp(stats[0:forecast_period,:].flatten(), popmean = 1).statistic,3))
    
    t_stat = ['T-Stat', '-', '-'] + [tstat_d5,tstat_d10,tstat_d20,tstat_d50]
     
    result['5d Simple Returns'] = stats[5] -1
    result['10d Simple Returns'] = stats[9] -1
    result['20d Simple Returns'] = stats[19] -1
    result['50d Simple Returns'] = stats[forecast_period-1] -1
    
    #Number of Days
    d5 = (len(result['5d Simple Returns'][result['5d Simple Returns'] > 0]), len(result['5d Simple Returns'][result['5d Simple Returns'] < 0]), len(result['5d Simple Returns']))
    d10 = (len(result['10d Simple Returns'][result['10d Simple Returns'] > 0]), len(result['10d Simple Returns'][result['10d Simple Returns'] < 0]), len(result['10d Simple Returns']))
    d20 = (len(result['20d Simple Returns'][result['20d Simple Returns'] > 0]), len(result['20d Simple Returns'][result['20d Simple Returns'] < 0]), len(result['20d Simple Returns']))
    d50 = (len(result['50d Simple Returns'][result['50d Simple Returns'] > 0]), len(result['50d Simple Returns'][result['50d Simple Returns'] < 0]), len(result['50d Simple Returns']))
    
    up_down = ['Up/Down/Total', '-', '-']
    summary_table_updown = ['Up/Down/Total', '-', '-']
    for arr in [d5,d10,d20,d50]:
        up_down.append('+'+str(arr[0])+' / -'+str(arr[1]) + ' / ' + str(arr[2]))
        summary_table_updown.append('"+'+str(arr[0])+' /-'+str(arr[1]) + '/' + str(arr[2]) + '"')
    
    Mean = ['Mean', '-', '-'] + list(np.round([np.mean(result['5d Simple Returns'])*100, np.mean(result['10d Simple Returns'])*100, 
           np.mean(result['20d Simple Returns'])*100, np.mean(result['50d Simple Returns'])*100],3))
    Max = ['Max', '-', '-'] + list(np.round([np.max(result['5d Simple Returns'])*100, np.max(result['10d Simple Returns'])*100, 
           np.max(result['20d Simple Returns'])*100, np.max(result['50d Simple Returns'])*100],3))
    Min = ['Min', '-', '-'] + list(np.round([np.min(result['5d Simple Returns'])*100, np.min(result['10d Simple Returns'])*100, 
           np.min(result['20d Simple Returns'])*100, np.min(result['50d Simple Returns'])*100],3))
    Median = ['Median', '-', '-'] + list(np.round([np.median(result['5d Simple Returns'])*100, np.median(result['10d Simple Returns'])*100, 
           np.median(result['20d Simple Returns'])*100, np.median(result['50d Simple Returns'])*100],3))
    
    values_matrix = np.array([Mean, Max, Min, Median, up_down, t_stat])
    sum_matrix = np.array([Mean, Max, Min, Median, summary_table_updown, t_stat])
    
    result['Start Date'] = dates[result['Start Date']]
    result['End Date'] = dates[result['End Date']]
    result['Start Date'] = result['Start Date'].dt.strftime("%Y-%m-%d")
    result['End Date'] = result['End Date'].dt.strftime("%Y-%m-%d")
    result.iloc[:,2:8] = (round(result.iloc[:,2:8] * 100,4)).astype(str) + '%'
    result = result.iloc[0:5,:]
    result = result.append(pd.DataFrame(values_matrix, columns = result.columns))
    result.iloc[-6:-2,-4:] = result.iloc[-6:-2,-4:].astype(str) + '%'
    
    summary_table.append(np.array([chart_name, values_matrix.T[3][0], values_matrix.T[3][-2],
                                   values_matrix.T[3][-1], values_matrix.T[-1][0], 
                                   values_matrix.T[-1][-2], values_matrix.T[-1][-1]]))
    
    graph_table.append(np.array([chart_name, sum_matrix.T[3][0], sum_matrix.T[3][-2],
                                   sum_matrix.T[3][-1], sum_matrix.T[-1][0], 
                                   sum_matrix.T[-1][-2], sum_matrix.T[-1][-1]]))
    
    #Plotting Graph with statistics table
    fig, ax = plt.subplots(figsize=(30,22))
    plt.ioff()
    candlestick_ohlc(ax, quotes=zip(mdates.date2num(prices[0:180].index), prices.iloc[0:180,0],
                                    prices.iloc[0:180,1], prices.iloc[0:180,2],
                                    prices.iloc[0:180,3]),width=0.6, colorup = 'g',
                                    colordown='r')

    ax.xaxis_date()
    plt.axvline(x=prices.index[180], color = 'black', label = 'Today: ' + str(dataset_1.index[-1].strftime("%B-%d, %Y")))
    plt.axvline(x=prices.index[185], color = 'black', linestyle = 'dotted',
                dashes=(10, 10))
    plt.axvline(x=prices.index[190], color = 'black', linestyle = 'dotted',
                dashes=(10, 10))
    plt.axvline(x=prices.index[200], color = 'black', linestyle = 'dotted',
                dashes=(10, 10))
    ax.plot(prices.index[180:], returns[0], #np.array([prices.iloc[181:,0].values]).T,
            linestyle = 'dashed', linewidth=2.0, color = 'crimson', label = 'Mean')
    ax.plot(prices.index[180:], returns[3],#np.array([prices.iloc[181:,3].values]).T,
            linestyle = 'dashed', linewidth=2.0, color = 'darkblue', label = 'Median')
    plt.legend(loc='upper left', fontsize=24)
    plt.fill_between(prices.index[180:], prices.iloc[180:,1].values, 
                     prices.iloc[180:,2].values, color='silver')
    plt.xlabel('Dates', fontsize = 20)
    plt.ylabel('Price', fontsize = 24)
    plt.xticks(last_date[::30], fontsize = 24)
    plt.yticks(fontsize = 24)
    ax.text(0.9, 0.97,'Correlation-Implied Projection', horizontalalignment='center',
                 verticalalignment='center',
                 transform=ax.transAxes, fontsize=18)
    ax.text(0.1, 0.05,'Total Number of Samples: ' + str(d5[2]), horizontalalignment='center',
                 verticalalignment='center',
                 transform=ax.transAxes, fontsize=18)
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%b-%Y'))
    plt.title(name, fontsize = 28)
    plt.grid(linestyle='dotted', dashes=(10, 10))

    table = plt.table(cellText=result.values,colWidths = [0.4]*len(result.columns),
              colLabels=result.columns,
              cellLoc = 'center', rowLoc = 'center',
              loc='bottom',
              bbox = [0, -1.0, 1.0, 0.8])
    table.set_fontsize(22)
    plt.subplots_adjust(bottom=0.5)
    
    mng = plt.get_current_fig_manager()
    mng.full_screen_toggle()
    pdf.savefig(fig)
    plt.close(fig)

if __name__ == "__main__":
    
#####################PARAMETERS##################################
    tickers = ['USDJPY CURNCY', 'EURUSD CURNCY', 'GBPUSD CURNCY', 'AUDUSD CURNCY',
               'NZDUSD CURNCY', 'USDCAD CURNCY', 'USDCHF CURNCY', 'XAUUSD CURNCY',
               'DXY CURNCY', 'ADXY CURNCY', 'USDCNY CURNCY', 'USDCNH CURNCY',
               'KWN+1M CURNCY', 'NTN+1M CURNCY', 'IRN+1M CURNCY', 'IHN+1M CURNCY',
               'PPN+1M CURNCY', 'USDSGD CURNCY', 'JPYKRW CURNCY', 'EURJPY CURNCY',
               'GBPJPY CURNCY', 'AUDJPY CURNCY', 'NZDJPY CURNCY', 'CADJPY CURNCY',
               'CHFJPY CURNCY', 'EURGBP CURNCY', 'EURAUD CURNCY', 'EURNZD CURNCY',
               'EURCAD CURNCY', 'GBPAUD CURNCY', 'GBPNZD CURNCY', 'GBPCAD CURNCY',
               'AUDNZD CURNCY', 'AUDSGD CURNCY', 'AUDCAD CURNCY', 'NZDCAD CURNCY',
               'SHCOMP Index', 'SPX Index', 'ES1 Index', 'NKY Index','KOSPI Index',
               'TWSE Index','HSCEI Index','HSI Index','SET Index']
    
    forecast_period = 50
    days_to_extract = 8649 #Past number of days from today
    method = 'Self' #or 'Comparison'
    correlation_threshold = 0.8 #Value should be between 0 & 1
    end_date = datetime.now() 
    #end_date = '2018-01-04'      #To specify end date for backtesting
##################################################################
    
    
    if len(tickers) != len(set(tickers)):
        print('Duplicate Tickers in the list')
        tickers = list(set(tickers))
    #end_date = datetime.strptime(end_date, '%Y-%m-%d')
    
    #Request from Bloomberg API
    try:
        test_data = BLPInterface().historicalRequest(tickers,
                    ['PX_OPEN', 'PX_HIGH', 'PX_LOW', 'PX_LAST'], end_date - timedelta(days=days_to_extract), end_date)
    except RequestError:
        test_data = BLPInterface().historicalRequest(tickers,
                    ['PX_LAST'], end_date - timedelta(days=days_to_extract), end_date)
    except IndexError:
        print('Data Unavailable for Dates given')
    
    test_data = test_data.dropna(axis=1, how='all')
    if test_data.dropna().empty == True:
        print('Error: No Data Available')
        
    test_data = test_data[tickers]
    bbg_frames = [test_data[i] for i in tickers]
    names = tickers
    address = dict(zip(names, bbg_frames))

    if method == 'Self':
        combs = np.array([names, names]).T
    elif method == 'Comparison':
        combs = list(permutations(names,2))
        
    summary_table= []
    graph_table = []
    
    with PdfPages('correlation_output.pdf') as pdf:
        list(map(correlation, combs))
        d = pdf.infodict()
        d['Title'] = 'Correlation'
        d['Author'] = 'Kenneth'
    
    summary_table = pd.DataFrame(summary_table, columns = 
                                 ['Chart', '5d Simple Returns Mean',
                                  '5d Simple Returns Up/Down/Total',
                                  '5d Simple Returns T-Stat',
                                  '20d Simple Returns Mean',
                                  '20d Simple Returns Up/Down/Total',
                                  '20d Simple Returns T-Stat'])
        
    graph_table = pd.DataFrame(graph_table, columns = 
                                 ['Chart', '5d Simple Returns Mean',
                                  '5d Simple Returns Up/Down/Total',
                                  '5d Simple Returns T-Stat',
                                  '20d Simple Returns Mean',
                                  '20d Simple Returns Up/Down/Total',
                                  '20d Simple Returns T-Stat'])
    
    with PdfPages('result.pdf') as pdf:
        render_mpl_table(summary_table, header_columns=0, col_width=4.0)

    merger = PdfFileMerger()
    with open("result.pdf", "rb") as f:
        merger.append(PdfFileReader(f))
        f.close()
        
    with open("correlation_output.pdf", "rb") as f:
        merger.append(PdfFileReader(f))
        f.close()
        
    with open("correlation_result.pdf", "wb") as fout:
        merger.write(fout)
        fout.close()
  
    os.remove("correlation_output.pdf")
    os.remove("result.pdf")
    graph_table.to_csv('output.csv')
