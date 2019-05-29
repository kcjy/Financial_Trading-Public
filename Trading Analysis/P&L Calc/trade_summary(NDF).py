import numpy as np
import pandas as pd
from datetime import datetime, timedelta
import pdblp
from os import listdir
from os.path import isfile, join
import re

class trade_calc:
    
    def calculate_pnl(data_array, currencies, price_dates, data, prev_data, positions, today, qty):
        overall = []
        for index in data_array:
            print(index)
            #For positions with local currencies as base: USD/JPY
            if qty == 'Quantity':
                #Extracting today's price from Bloomberg
                ref_price = [con.bdh(currencies[index][1] + ' BGN Curncy', 'PX_LAST', date, date).values[0][0] for date in price_dates.values()]
                try:
                    cur_dates = data.loc[positions[index]]['TradeDate'].values
                    result = []
                    for trade in range(0,len(data.loc[positions[index]].index)):
                        #Identifying whether Trade Date is today
                        if cur_dates[trade] != today:
                            result.append((ref_price[1] - ref_price[0]) * data.loc[positions[index]][qty].values[trade] / ref_price[1])
                            initialprice = ref_price[0]
                        elif cur_dates[trade] == today:
                            result.append((ref_price[1] - data.loc[positions[index]]['InitialPrice'].values[trade]) * data.loc[positions[index]][qty].values[trade] / ref_price[1])
                            initialprice = data.loc[positions[index]]['InitialPrice'].values[trade]
                    qty_dates = max([datetime.strptime(date, '%Y%m%d').strftime('%Y%m%d') for date in np.array(data.loc[positions[index]]['SettlementDate'])])
                    date_filter = qty_dates == np.array(data.loc[positions[index]]['SettlementDate'])
                    #This try-catch is needed to filter when there are two or more positions that have Settlement Date being today
                    try:
                        quantities = [today, positions[index],
                                      data.loc[positions[index]][date_filter]['Quantity'].values[0], 
                                      data.loc[positions[index]][date_filter]['LocalCcyQuantity'].values[0],
                                      data.loc[positions[index]][date_filter]['TradeDate'].values[0],
                                      initialprice, ref_price[1], sum(result)]
                    except IndexError:
                        quantities = [today, positions[index],
                                      data.loc[positions[index]][date_filter]['Quantity'], 
                                      data.loc[positions[index]][date_filter]['LocalCcyQuantity'],
                                      data.loc[positions[index]][date_filter]['TradeDate'],
                                      initialprice, ref_price[1], sum(result)]
                    
                except AttributeError: #This exception is needed to catch instances when there is only one trade for a specific instrument
                    cur_dates = data.loc[positions[index]]['TradeDate']
                    result = []
                    if cur_dates != today:
                        result.append((ref_price[1] - ref_price[0]) * data.loc[positions[index]][qty] / ref_price[1])
                        initialprice = ref_price[0]
                    elif cur_dates == today:
                        result.append((ref_price[1] - data.loc[positions[index]]['InitialPrice']) * data.loc[positions[index]][qty] / ref_price[1])
                        initialprice = data.loc[positions[index]]['InitialPrice']
                        
                    qty_dates = data.loc[positions[index]]['SettlementDate']
                    quantities = [today, positions[index], data.loc[positions[index]]['Quantity'], 
                                  data.loc[positions[index]]['LocalCcyQuantity'], 
                                  data.loc[positions[index]]['TradeDate'],
                                  initialprice, ref_price[1], sum(result)]
                    
            #For positions with USD as base: JPY/USD    
            elif qty == 'LocalCcyQuantity':
                ref_price = [con.bdh(currencies[index][0] + ' BGN Curncy', 'PX_LAST', date, date).values[0][0] for date in price_dates.values()] #ref_price returns two dates in list: T-1, T
                try:
                    cur_dates = data.loc[positions[index]]['TradeDate'].values
                    result = []
                    for trade in range(0,len(data.loc[positions[index]].index)):
                        #Identifying whether Trade Date is today
                        if cur_dates[trade] != today: 
                            result.append((1/ref_price[1] - 1/ref_price[0]) * data.loc[positions[index]][qty].values[trade] / ref_price[1]) #Note the difference in calculation
                            initialprice = ref_price[0]
                        elif cur_dates[trade] == today:
                            result.append((1/ref_price[1] - 1/data.loc[positions[index]]['InitialPrice'].values[trade]) * data.loc[positions[index]][qty].values[trade] / ref_price[1])
                            initialprice = data.loc[positions[index]]['InitialPrice'].values[trade]
                    qty_dates = max([datetime.strptime(date, '%Y%m%d').strftime('%Y%m%d') for date in np.array(data.loc[positions[index]]['SettlementDate'])])
                    date_filter = qty_dates == np.array(data.loc[positions[index]]['SettlementDate'])
                    
                    #This try-catch is needed to filter when there are two or more positions that have Settlement Date being today
                    try:
                        quantities = [today, positions[index],
                                  data.loc[positions[index]][date_filter]['LocalCcyQuantity'].values[0], 
                                  data.loc[positions[index]][date_filter]['Quantity'].values[0],
                                  data.loc[positions[index]][date_filter]['TradeDate'].values[0],
                                  initialprice, 1/ref_price[1], sum(result)]
                    except IndexError:
                        quantities = [today, positions[index],
                                      data.loc[positions[index]][date_filter]['LocalCcyQuantity'], 
                                      data.loc[positions[index]][date_filter]['Quantity'],
                                      data.loc[positions[index]][date_filter]['TradeDate'],
                                      initialprice, ref_price[1], sum(result)]
                    
                except AttributeError: #This exception is needed to catch instances when there is only one trade for a specific instrument
                    cur_dates = data.loc[positions[index]]['TradeDate']
                    result = []
                    if cur_dates != today:
                        result.append((1/ref_price[1] - 1/ref_price[0]) * data.loc[positions[index]][qty] * ref_price[1])
                        initialprice = ref_price[0]
                    elif cur_dates == today:
                        result.append((1/ref_price[1] - 1/data.loc[positions[index]]['InitialPrice']) * data.loc[positions[index]][qty] * ref_price[1])
                        initialprice = data.loc[positions[index]]['InitialPrice']
                    qty_dates = data.loc[positions[index]]['SettlementDate']
                    quantities = [today, positions[index], data.loc[positions[index]]['LocalCcyQuantity'], 
                                  data.loc[positions[index]]['Quantity'], 
                                  data.loc[positions[index]]['TradeDate'],
                                  initialprice, ref_price[1], sum(result)]
            #For NDF positions (INCOMPLETE) 
            elif qty == 'NDF': 
                result = [] #Values from T csv
                prev_result = [] #Values from T-1 csv
                #ref_price returns two dates in list: T-1, T
                ref_price = [con.bdh(currencies[index][1] + ' BGN Curncy', 'PX_LAST', date, date).values[0][0] for date in price_dates.values()]
                
                for trade in range(0,len(data.loc[positions[index]].index)):
                    result.append(data.loc[positions[index]]['UnderlierClosePrice'].values[trade])
                resultsum = sum(data.loc[positions[index]]['Quantity'].values * np.array(result)) / ref_price[1]
                
                
                for trade in range(0,len(prev_data.loc[positions[index]].index)):
                    prev_result.append(prev_data.loc[positions[index]]['UnderlierClosePrice'].values[trade])
                prev_resultsum = sum(prev_data.loc[positions[index]]['Quantity'].values * np.array(prev_result)) / ref_price[1]
        
                qty_dates = max([datetime.strptime(date, '%Y%m%d').strftime('%Y%m%d') for date in np.array(data.loc[positions[index]]['SettlementDate'])])
                date_filter = qty_dates == np.array(data.loc[positions[index]]['SettlementDate'])
                try:
                    quantities = [today, positions[index],
                              data.loc[positions[index]][date_filter]['Quantity'].values[0], 
                              data.loc[positions[index]][date_filter]['LocalCcyQuantity'].values[0],
                              data.loc[positions[index]][date_filter]['TradeDate'].values[0],
                              prev_result[-1], result[-1], resultsum - prev_resultsum]
                except IndexError:
                    quantities = [today, positions[index],
                                  data.loc[positions[index]][date_filter]['Quantity'], 
                                  data.loc[positions[index]][date_filter]['LocalCcyQuantity'],
                                  data.loc[positions[index]][date_filter]['TradeDate'],
                                  prev_result[-1], result[-1], resultsum - prev_resultsum]

            overall.append(quantities)
        return(overall)

    def pnl(date, trading_dates):

        file = trade_files[date]
        file_loc = file_dir + file #File location
        if date == 0: #Condition to prevent first csv to reference last csv through -1
            prev_file = None
        else:
            prev_file = file_dir + trade_files[date-1]
        
        try: #Exception needed as some files do not have Underlier Close Price
            prev_data = pd.read_csv(prev_file)[['Quantity', 'LocalCcyQuantity', 'TradeDate','SettlementDate', 'UnderlierClosePrice']]
            data = pd.read_csv(file_loc)[['Quantity', 'LocalCcyQuantity', 'TradeDate','SettlementDate', 'UnderlierClosePrice']]
        except IndexError:
            prev_data = pd.read_csv(prev_file)[['Quantity', 'LocalCcyQuantity', 'TradeDate','SettlementDate']]
            data = pd.read_csv(file_loc)[['Quantity', 'LocalCcyQuantity', 'TradeDate','SettlementDate']]
        
        data['InitialPrice'] = 0
        #Formula to convert Excel dates into readable datetime format
        data['TradeDate'] = [datetime.fromordinal(datetime(1900, 
                                                        1, 1).toordinal() + date - 2).strftime('%Y%m%d') for date in data['TradeDate']]
        data['SettlementDate'] = [datetime.fromordinal(datetime(1900, 
                                                        1, 1).toordinal() + date - 2).strftime('%Y%m%d') for date in data['SettlementDate']]
        data.index = pd.read_csv(file_loc)['InstrumentName'] #Getting Instrument Name
        
        prev_data['InitialPrice'] = 0
        prev_data['TradeDate'] = [datetime.fromordinal(datetime(1900, 
                                                        1, 1).toordinal() + date - 2).strftime('%Y%m%d') for date in prev_data['TradeDate']]
        prev_data['SettlementDate'] = [datetime.fromordinal(datetime(1900, 
                                                        1, 1).toordinal() + date - 2).strftime('%Y%m%d') for date in prev_data['SettlementDate']]
        prev_data.index = pd.read_csv(prev_file)['InstrumentName']
        
        positions = np.array(list(set(data.index))) #Unique Instrument Names
        omit = ['IDR', 'INR', 'KRW','PHP','TWD']
        
        currencies = np.array([cur.split('/') for cur in positions])
        cur_filter = [(cur[0] and cur[1]) not in omit for cur in currencies] #Non-NDF positions
        ndf_filter = [(cur[0] and cur[1]) in omit for cur in currencies] #NDF positions
        
        fx = currencies[cur_filter]
        fx_positions = positions[cur_filter]
        
        ndf = currencies[ndf_filter]
        ndf_positions = positions[ndf_filter]
        
        today = trading_dates[trading_dates.index(dates[date])-1]
        prev = trading_dates[trading_dates.index(dates[date])-2]
        price_dates = {'T-1':prev, 'T':today}
        
        #Identifying and sorting positions with different base/local currencies
        usd_index = [cur[0] for cur in list(enumerate(fx)) if cur[1][0] == 'USD']
        local_index = [cur[0] for cur in list(enumerate(fx)) if cur[1][0] != 'USD']
        
        ndf_index = [cur[0] for cur in list(enumerate(ndf)) if cur[1][0] == 'USD']
        ndflocal_index = [cur[0] for cur in list(enumerate(ndf)) if cur[1][0] != 'USD']
        
        data.loc[fx_positions[usd_index], 'InitialPrice'] = data.loc[fx_positions[usd_index]]['LocalCcyQuantity'].values / data.loc[fx_positions[usd_index]]['Quantity'].values * -1
        data.loc[fx_positions[local_index], 'InitialPrice'] = data.loc[fx_positions[local_index]]['Quantity'].values / data.loc[fx_positions[local_index]]['LocalCcyQuantity'].values * -1
        
        #Main Calculation
        usd_data = trade_calc.calculate_pnl(usd_index, fx, price_dates, data, prev_data, fx_positions, today, 'Quantity')
        local_data = trade_calc.calculate_pnl(local_index, fx, price_dates, data, prev_data, fx_positions, today, 'LocalCcyQuantity')
        
        final = usd_data + local_data
        final = pd.DataFrame(final, columns = ['Date','Instrument','USD_Qty','LocalCCy_Quantity',
                                              'Trade Date','Start Price',
                                              'End Price','P&L'])
                        
        ndf_usd_data = trade_calc.calculate_pnl(ndf_index, ndf, price_dates, data, prev_data, ndf_positions, today, 'NDF')
        ndf_local_data = trade_calc.calculate_pnl(ndflocal_index, ndf, price_dates, data, prev_data, ndf_positions, today, 'NDF_Local')
        NDF = ndf_usd_data + ndf_local_data
        NDF = pd.DataFrame(NDF, columns = ['Date','Instrument','USD_Qty','LocalCCy_Quantity',
                                              'Trade Date','Start Price',
                                              'End Price','P&L'])
        final = pd.concat([final, NDF], axis=0)
        return(final)
#Run Function
if __name__ == "__main__":
    #File Directory
    file_dir = 'N:/Macro/Research & Strategy/@Interns/Kenneth/'
    con = pdblp.BCon(debug=False, port=8194, timeout=5000)
    con.start()
    #Extract all trading days using USDJPY as a proxy
    trade_files = [f for f in listdir(file_dir) if isfile(join(file_dir, f))]
    dates = [re.findall(r'\d+', s)[0] for s in trade_files]
    trading_dates = list(con.bdh('USDJPY Curncy', 'PX_LAST', (datetime.now() - timedelta(days=250)).strftime('%Y%m%d'),
                                 datetime.now().strftime('%Y%m%d')).index.strftime('%Y%m%d'))
    appended_data = []
    for date in dates:
        if date not in trading_dates:
            next
        else:
            try:
                appended_data.append(trade_calc.pnl(dates.index(date), trading_dates))
            except FileNotFoundError:
                print('File Not Found')
                next
    
    appended_data = pd.concat(appended_data, axis=0)
    appended_data.to_csv(file_dir+'Trade Summary.csv')
    #compiled = [sum(appended_data['P&L'][appended_data['Instrument'] == x]) for x in list(set(appended_data['Instrument']))]
    #dollar_pos = round(sum(appended_data['USD_Qty'] > 0) / len(appended_data['USD_Qty']),3)

