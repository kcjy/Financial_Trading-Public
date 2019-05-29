import numpy as np
import pandas as pd
from datetime import datetime, timedelta
import pdblp
from os import listdir
from os.path import isfile, join
import re

class trade_calc:

    def calculate_pnl(data_array, currencies, price_dates, data, positions, today, qty):
        global ndf_dict
        overall = []
        for index in data_array:
            print(index)
            if qty == 'Quantity':
            
                ref_price = [con.bdh(currencies[index][1] + ' BGN Curncy', 'PX_LAST', date, date).values[0][0] for date in price_dates.values()]
                try:
                    cur_dates = data.loc[positions[index]]['TradeDate'].values
                    result = []
                    for trade in range(0,len(data.loc[positions[index]].index)):
                        if cur_dates[trade] != today:
                            result.append((ref_price[1] - ref_price[0]) * data.loc[positions[index]][qty].values[trade] / ref_price[1])
                            initialprice = ref_price[0]
                        elif cur_dates[trade] == today:
                            result.append((ref_price[1] - data.loc[positions[index]]['InitialPrice'].values[trade]) * data.loc[positions[index]][qty].values[trade] / ref_price[1])
                            initialprice = data.loc[positions[index]]['InitialPrice'].values[trade]
                    qty_dates = max([datetime.strptime(date, '%Y%m%d').strftime('%Y%m%d') for date in np.array(data.loc[positions[index]]['SettlementDate'])])
                    date_filter = qty_dates == np.array(data.loc[positions[index]]['SettlementDate'])
                    
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
                    quantities = [today, positions[index], data.loc[positions[index]]['LocalCcyQuantity'], 
                                  data.loc[positions[index]]['Quantity'], 
                                  data.loc[positions[index]]['TradeDate'],
                                  initialprice, ref_price[1], sum(result)]
                
            elif qty == 'LocalCcyQuantity':
                ref_price = [con.bdh(currencies[index][0] + ' BGN Curncy', 'PX_LAST', date, date).values[0][0] for date in price_dates.values()]
                try:
                    cur_dates = data.loc[positions[index]]['TradeDate'].values
                    result = []
                    for trade in range(0,len(data.loc[positions[index]].index)):
                        if cur_dates[trade] != today:
                            result.append((1/ref_price[1] - 1/ref_price[0]) * data.loc[positions[index]][qty].values[trade] * ref_price[1])
                            initialprice = ref_price[0]
                        elif cur_dates[trade] == today:
                            result.append((1/ref_price[1] - 1/data.loc[positions[index]]['InitialPrice'].values[trade]) * data.loc[positions[index]][qty].values[trade] * ref_price[1])
                            initialprice = data.loc[positions[index]]['InitialPrice'].values[trade]
                    qty_dates = max([datetime.strptime(date, '%Y%m%d').strftime('%Y%m%d') for date in np.array(data.loc[positions[index]]['SettlementDate'])])
                    date_filter = qty_dates == np.array(data.loc[positions[index]]['SettlementDate'])
                    try:
                        quantities = [today, positions[index],
                                  data.loc[positions[index]][date_filter]['LocalCcyQuantity'].values[0], 
                                  data.loc[positions[index]][date_filter]['Quantity'].values[0],
                                  data.loc[positions[index]][date_filter]['TradeDate'].values[0],
                                  initialprice, ref_price[1], sum(result)]
                    except IndexError:
                        quantities = [today, positions[index],
                                      data.loc[positions[index]][date_filter]['LocalCcyQuantity'], 
                                      data.loc[positions[index]][date_filter]['Quantity'],
                                      data.loc[positions[index]][date_filter]['TradeDate'],
                                      initialprice, ref_price[1], sum(result)]
                    
                except AttributeError:
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
                    
            elif qty == 'NDF_USD':
                ref_price = [con.bdh(currencies[index][1] + ' BGN Curncy', 'PX_LAST', date, date).values[0][0] for date in price_dates.values()]
                #try:
                spotT = con.bdh(currencies[index][1] + ' BSYN Curncy', 'PX_LAST', date, date).values[0][0]
                fwd_price = con.bdh(ndf_dict[currencies[index][1]] + '+1M BGN Curncy', 'PX_LAST', date, date).values[0][0]
                
                cur_dates = [datetime.strptime(date, '%Y%m%d') for date in data.loc[positions[index]]['TradeDate'].values]
                traded_price = [con.bdh(ndf_dict[currencies[index][1]] + '+1M BGN Curncy', 
                                        'PX_LAST', datetime.strftime(date,'%Y%m%d'), 
                                        datetime.strftime(date,'%Y%m%d')).values[0][0] for date in cur_dates]
                t_lapse = [(datetime.strptime(date, '%Y%m%d') - curdate).days for curdate in cur_dates]
                
                value = np.array([fwd_price - ((fwd_price - spotT) / 30 * x) for x in t_lapse]) - np.array(traded_price)
                result = sum(data.loc[positions[index]]['Quantity'].values * value)
                
                qty_dates = max([datetime.strptime(date, '%Y%m%d').strftime('%Y%m%d') for date in np.array(data.loc[positions[index]]['SettlementDate'])])
                date_filter = qty_dates == np.array(data.loc[positions[index]]['SettlementDate'])
                initialprice = ref_price[0]
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
                
            elif qty == 'NDF_Local':
                ref_price = [con.bdh(currencies[index][1] + ' BGN Curncy', 'PX_LAST', date, date).values[0][0] for date in price_dates.values()]
                #try:
                spotT = con.bdh(currencies[index][1] + ' BSYN Curncy', 'PX_LAST', date, date).values[0][0]
                fwd_price = con.bdh(ndf_dict[currencies[index][1]] + '+1M BGN Curncy', 'PX_LAST', date, date).values[0][0]
                
                cur_dates = [datetime.strptime(date, '%Y%m%d') for date in data.loc[positions[index]]['TradeDate'].values]
                traded_price = [con.bdh(ndf_dict[currencies[index][1]] + '+1M BGN Curncy', 
                                        'PX_LAST', datetime.strftime(date,'%Y%m%d'), 
                                        datetime.strftime(date,'%Y%m%d')).values[0][0] for date in cur_dates]
                t_lapse = [(datetime.strptime(date, '%Y%m%d') - curdate).days for curdate in cur_dates]
                
                value = np.array([fwd_price - ((fwd_price - spotT) / 30 * x) for x in t_lapse]) - np.array(traded_price)
                result = sum(data.loc[positions[index]]['LocalCcyQuantity'].values * value)
                initialprice = ref_price[0]
                
                try:
                    quantities = [today, positions[index],
                              data.loc[positions[index]][date_filter]['LocalCcyQuantity'].values[0], 
                              data.loc[positions[index]][date_filter]['Quantity'].values[0],
                              data.loc[positions[index]][date_filter]['TradeDate'].values[0],
                              initialprice, ref_price[1], sum(result)]
                except IndexError:
                    quantities = [today, positions[index],
                                  data.loc[positions[index]][date_filter]['LocalCcyQuantity'], 
                                  data.loc[positions[index]][date_filter]['Quantity'],
                                  data.loc[positions[index]][date_filter]['TradeDate'],
                                  initialprice, ref_price[1], sum(result)]
                
            overall.append(quantities)
        return(overall)

    def pnl(file, trading_dates):
        print(file)
        file_loc = file_dir + file #File location
        data = pd.read_csv(file_loc)[['Quantity', 'LocalCcyQuantity', 'TradeDate','SettlementDate']]

        data['InitialPrice'] = 0
        data['TradeDate'] = [datetime.fromordinal(datetime(1900, 
                                                        1, 1).toordinal() + date - 2).strftime('%Y%m%d') for date in data['TradeDate']]
        data['SettlementDate'] = [datetime.fromordinal(datetime(1900, 
                                                        1, 1).toordinal() + date - 2).strftime('%Y%m%d') for date in data['SettlementDate']]
        data.index = pd.read_csv(file_loc)['InstrumentName'] #Getting Instrument Name
        positions = np.array(list(set(data.index))) #Unique Instrument Names
        omit = ['IDR', 'INR', 'KRW','PHP','TWD']
        
        currencies = np.array([cur.split('/') for cur in positions])
        cur_filter = [(cur[0] and cur[1]) not in omit for cur in currencies]
        ndf_filter = [(cur[0] and cur[1]) in omit for cur in currencies]
        fx = currencies[cur_filter]
        fx_positions = positions[cur_filter]
        
        ndf = currencies[ndf_filter]
        ndf_positions = positions[ndf_filter]
        
        today = trading_dates[trading_dates.index(date)-1]
        prev = trading_dates[trading_dates.index(date)-2]
        price_dates = {'T-1':prev, 'T':today}
        
        usd_index = [cur[0] for cur in list(enumerate(fx)) if cur[1][0] == 'USD']
        local_index = [cur[0] for cur in list(enumerate(fx)) if cur[1][0] != 'USD']
        
        ndf_index = [cur[0] for cur in list(enumerate(ndf)) if cur[1][0] == 'USD']
        ndflocal_index = [cur[0] for cur in list(enumerate(ndf)) if cur[1][0] != 'USD']
        
        data.loc[fx_positions[usd_index], 'InitialPrice'] = data.loc[fx_positions[usd_index]]['LocalCcyQuantity'].values / data.loc[fx_positions[usd_index]]['Quantity'].values * -1
        data.loc[fx_positions[local_index], 'InitialPrice'] = data.loc[fx_positions[local_index]]['Quantity'].values / data.loc[fx_positions[local_index]]['LocalCcyQuantity'].values * -1
        
        usd_data = trade_calc.calculate_pnl(usd_index, fx, price_dates, data, fx_positions, today, 'Quantity')
        local_data = trade_calc.calculate_pnl(local_index, fx, price_dates, data, fx_positions, today, 'LocalCcyQuantity')
        final = usd_data + local_data
        final = pd.DataFrame(final, columns = ['Date','Instrument','USD_Qty','LocalCCy_Quantity',
                                              'Trade Date','Start Price',
                                              'End Price','P&L'])
                        
        ndf_usd_data = trade_calc.calculate_pnl(ndf_index, ndf, price_dates, data, ndf_positions, today, 'Quantity')
        ndf_local_data = trade_calc.calculate_pnl(ndflocal_index, ndf, price_dates, data, ndf_positions, today, 'LocalCcyQuantity')
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
                appended_data.append(trade_calc.pnl(trade_files[dates.index(date)], trading_dates))
            except FileNotFoundError:
                print('File Not Found')
                next
        
    appended_data = pd.concat(appended_data, axis=0)
    appended_data.to_csv(file_dir+'Trade Summary.csv')

#trade_calc.pnl(portfolio, ticker, '20181017').to_csv(file_dir+'Trade Summary.csv')
