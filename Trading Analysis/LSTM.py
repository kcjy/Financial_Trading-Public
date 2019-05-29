
import theano
import keras
import blpapi
import matplotlib.pyplot as plt
import pdblp
import seaborn as sns
import numpy as np
import pandas as pd
from datetime import date, datetime, time
from pytz import timezone
import math
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import MinMaxScaler
from keras.models import Sequential
from keras.layers import Dense, LSTM, Dropout
import time

# Load Bloomberg Connection
con = pdblp.BCon()
con.start()

# Constants
sgt = timezone("Asia/Singapore")
start_date = datetime(2016,1,1,5,0)
end_date = datetime.now()
ticker = "USDJPY BGN Curncy"
num_lags = 1
num_forecasts = 5

def series_to_supervised(data, n_in=1, n_out=1, dropnan=True):

	n_vars = 1 if type(data) is list else data.shape[1]
	df = pd.DataFrame(data)
	cols, names = list(), list()
	# input sequence (t-n, ... t-1)
	for i in range(n_in, 0, -1):
		cols.append(df.shift(i))
		names += [('var%d(t-%d)' % (j+1, i)) for j in range(n_vars)]
	# forecast sequence (t, t+1, ... t+n)
	for i in range(0, n_out):
		cols.append(df.shift(-i))
		if i == 0:
			names += [('var%d(t)' % (j+1)) for j in range(n_vars)]
		else:
			names += [('var%d(t+%d)' % (j+1, i)) for j in range(n_vars)]
	agg = pd.concat(cols, axis=1)
	agg.columns = names
	if dropnan:
		agg.dropna(inplace=True)
	return agg

# transform series into train and test sets for supervised learning
def prepare_data(series, n_test, n_lag, n_seq):
	# extract raw values
	raw_values = series.values
	raw_values = raw_values.reshape(len(raw_values), 1)
	# transform into supervised learning problem X, y
	supervised = series_to_supervised(raw_values, n_lag, n_seq)
	supervised_values = supervised.values
	# split into train and test sets
	train, test = supervised_values[0:-n_test], supervised_values[-n_test:]
	return train, test
# Get prices and returns
usdjpy_hour_price = con.bdib(ticker=ticker,start_datetime=start_date, end_datetime=end_date, event_type = "TRADE", interval = 60)
usdjpy_hour_ret = usdjpy_hour_price.pct_change()

# Clean Data
usdjpy_hour_ret.drop(['volume', 'numEvents'], axis=1, inplace=True)
usdjpy_hour_ret.dropna(inplace=True)
usdjpy_train, usdjpy_test = prepare_data(usdjpy_hour_ret['close'], -700, num_lags, num_forecasts)
print('Train: %s, Test: %s' %(usdjpy_train.shape, usdjpy_test.shape))

# Make Persistent Forecast: Naive Model as baseline - bring forward the last observation
def persistence(last_ob, n_seq):
	return [last_ob for i in range(n_seq)]

# evaluate the persistence model
def make_forecasts(train, test, n_lag, n_seq):
	forecasts = list()
	for i in range(len(test)):
		X, y = test[i, 0:n_lag], test[i, n_lag:]
		# make forecast
		forecast = persistence(X[-1], n_seq)
		# store the forecast
		forecasts.append(forecast)
	return forecasts

forecasts = make_forecasts(usdjpy_train, usdjpy_test, num_lags, num_forecasts)

# evaluate the RMSE for each forecast time step
def evaluate_forecasts(test, forecasts, n_lag, n_seq):
	for i in range(n_seq):
		actual = test[:,(n_lag+i)]
		predicted = [forecast[i] for forecast in forecasts]
		rmse = math.sqrt(mean_squared_error(actual, predicted))
		print('t+%d RMSE: %f' % ((i+1), rmse))
        
evaluate_forecasts(usdjpy_test, forecasts, num_lags, num_forecasts)

# ## Multi-step LSTM

sns.tsplot(usdjpy_hour_ret['close'])

# # transform series into train and test sets for supervised learning
def prepare_lstm_data(series, n_test, n_lag, n_seq):
    # extract raw values
    raw_values = series.values
    raw_values = raw_values.reshape(len(raw_values), 1)
    
    # rescale values to -1,1
    scaler = MinMaxScaler(feature_range=(-1,1))
    scaled_values = scaler.fit_transform(raw_values)
    scaled_values = scaled_values.reshape(len(raw_values), 1)
    
    # transform into supervised learning problem X, y
    supervised = series_to_supervised(scaled_values, n_lag, n_seq)
    supervised_values = supervised.values
    # split into train and test sets
    train, test = supervised_values[0:-n_test], supervised_values[-n_test:]
    return scaler, train, test

scaler, usdjpy_train, usdjpy_test = prepare_lstm_data(usdjpy_hour_ret['close'], 700, num_lags, num_forecasts)

start = time.time()
# reshape data to fit LSTM
X, y = usdjpy_train[:, 0:num_lags], usdjpy_train[:, num_lags:]
X = X.reshape(X.shape[0], 1, X.shape[1])

# Design Network
model = Sequential()
model.add(LSTM(50, batch_input_shape = (1, X.shape[1], X.shape[2])))
model.add(Dropout(0.2))
model.add(Dense(y.shape[1]))
model.compile(loss='mse', optimizer='adam')

#fit network
for i in range(1):
    model.fit(X,y, epochs=100, batch_size=1, shuffle=False)
    model.reset_states()
end = time.time()

model_time = start - end
model_time
