#Function: Import libraries
import urllib.request
import pandas as pd
import requests 
import quandl
import numpy as np
from yahoo_finance import Share
import pandas_datareader as pdr
from datetime import datetime, timedelta
import re
import statistics
from PyQt5.QtWidgets import QDialog
from PyQt5.QtWidgets import QMainWindow, QApplication,QMessageBox
from PyQt5 import uic
import sys

#Function: Use of GUI
Ui_MainWindow, QtBaseClass = uic.loadUiType("DCFCalculator.ui")

class Main(QMainWindow, Ui_MainWindow,QDialog):
	
	def __init__(self):
		super().__init__()
		self.setupUi(self)
		self.pushButton_6.clicked.connect(self.calculateAll)
		
	#CALCULATE DEBT----------------------------------------------------------------------------------
	def costOfDebt(self):
		print(self.lineEdit_CurrentDebtLastest.text(), type(self.lineEdit_CurrentDebtLastest.text()))
		current_debt_latest_fy = float(str(self.lineEdit_CurrentDebtLastest.text()))
        #self.textEdit.setText(str(P))
        
		print(self.lineEdit_CurrentDebtPreceding.text(), type(self.lineEdit_CurrentDebtPreceding.text()))
		current_debt_preceding_year_fy = float(self.lineEdit_CurrentDebtPreceding.text())
        
		print(self.lineEdit_LongTermDebtLatest.text(), type(self.lineEdit_LongTermDebtLatest.text()))
		long_term_debt_latest_fy= float(self.lineEdit_LongTermDebtLatest.text())
        
		print(self.lineEdit_LongTermDebtPreceding.text(), type(self.lineEdit_LongTermDebtPreceding.text()))
		long_term_debt_preceding_fy = float(self.lineEdit_LongTermDebtPreceding.text())
        
		print(self.lineEdit_InterestExpense.text(), type(self.lineEdit_InterestExpense.text()))
		interest_expense = float(self.lineEdit_InterestExpense.text())
        
		current_debt = (current_debt_latest_fy + current_debt_preceding_year_fy)/2
		long_term_debt = (long_term_debt_latest_fy + long_term_debt_preceding_fy)/2
		cost_of_debt = interest_expense/(current_debt + long_term_debt)
		
		self.lineEdit_CostOfDebt.setText(str(round(cost_of_debt*100,4)))
		
		#Use of return statements to return values
		return cost_of_debt, current_debt_latest_fy, long_term_debt_latest_fy
	
	#CALCULATE COST OF EQUITY------------------------------------------------------------------------
	def costOfEquity(self):
		#Stock
		print(self.lineEdit_StockTicker.text(), type(self.lineEdit_StockTicker.text()))
		stock_ticker = self.lineEdit_StockTicker.text()
		
		#Function: Using try and catch exception
		try:
			params = {"formatted": "true",
					  "crumb": "AKV/cl0TOgz",
					  "lang": "en-US",
					  "region": "US",
					  "modules": "defaultKeyStatistics,financialData",
					  "corsDomain": "finance.yahoo.com"}

			scrape_add = "https://query1.finance.yahoo.com/v10/finance/quoteSummary/" + stock_ticker
			r = requests.get(scrape_add, params=params)
			financial_data = r.json()[u'quoteSummary']["result"][0][u'financialData']
			key_stats = r.json()[u'quoteSummary']["result"][0][u'defaultKeyStatistics']

			beta = float(key_stats['beta']['raw'])
			current_price = float(re.sub('[,!@#$]', '', (financial_data['currentPrice']['fmt'])))

			sharesO = key_stats['sharesOutstanding']['fmt']
			sharesO = re.sub('[,!@#$]', '', sharesO)
			
			if sharesO[-1] == "B":
				shares_outstanding = float(sharesO[:-1]) * (10**9)
			elif sharesO[-1] == "M":
				shares_outstanding = float(sharesO[:-1]) * (10**6)
			else:
				shares_outstanding = float(sharesO)

			mkt_cap = current_price * shares_outstanding               
			
			#Change in URL    		
			url = 'https://eservices.mas.gov.sg/api/action/datastore/search.json?'
			parametersOne = 'resource_id=1c1713de-6b5e-475d-bc1e-b6a45b3e063e&limit=13&'
			parametersTwo = 'fields=end_of_month,sti&sort=end_of_month%20desc'
			url = url + parametersOne + parametersTwo
			resp=requests.get(url)
			r=resp.json()
			data = r.get('result').get('records')
			index = []

			#Function: For loop
			for item in data:
				df = [item["end_of_month"], item["sti"]]
				#Function: Appending list
				index.append(df)
			# Monthly Return Calculation        
				prices = []
				for item in index:
					prices.append(float(item[1]))
				#Function: Zip    
				market_risk_return = [(x - y)/y for x, y in zip(prices, prices[1:])]
			
			self.lineEdit_Beta.setText(str(beta))
			self.lineEdit_CurrentStockPrice.setText(str(mkt_cap))
			self.lineEdit_NumberOfOutstandingShares.setText(str(shares_outstanding))
			
		except:	
			# Importing Price Data
			#Function: Date time 
				start = datetime.today() - timedelta(days=365)
				end = datetime.today()
				stock_type = pdr.get_data_yahoo(symbols=stock_ticker, start=start, end=end)
				current_price = (stock_type['Adj Close']) #Daily Stock Prices			

				#Market Return
				url = 'https://eservices.mas.gov.sg/api/action/datastore/search.json?'
				parametersOne = 'resource_id=1c1713de-6b5e-475d-bc1e-b6a45b3e063e&limit=13&'
				parametersTwo = 'fields=end_of_month,sti&sort=end_of_month%20desc'
				url = url + parametersOne + parametersTwo
				resp=requests.get(url)
				r=resp.json()
				data = r.get('result').get('records')
				index = []

				#Function: For loop
				for item in data:
					df = [item["end_of_month"], item["sti"]]
					#Function: Appending list
					index.append(df)
				print(index)
				# Monthly Return Calculation        
				prices = []
				for item in index:
					prices.append(float(item[1]))
				#Function: Zip    
				market_risk_return = [(x - y)/y for x, y in zip(prices, prices[1:])]

				stock = Share(stock_ticker)

				#Market Cap
				if (stock.get_market_cap())[-1] == "B":
					mkt_cap = float(stock.get_market_cap()[:-1]) * (10**9)
				elif (stock.get_market_cap())[-1] == "M":
					mkt_cap = float(stock.get_market_cap()[:-1]) * (10**6)
				else:
					mkt_cap = float(stock.get_market_cap())
				
				self.lineEdit_CurrentStockPrice.setText(str(mkt_cap))
				
				self.lineEdit_NumberOfOutstandingShares.setText(str(shares_outstanding))				
				
				#Beta
				data_array = pd.DataFrame([market_risk_return, stock_return]).T
				covariance = np.cov(data_array[0],data_array[1])
				beta = covariance[0,1]/covariance[1,1]
				self.lineEdit_Beta.setText(str(round(beta,4)))
		
		#Calculation of Risk Free Rate (Rf)
		data = quandl.get("MAS/SGSB")
		risk_free_rate = data['5Y Bond Close Yield'][0]/100
		print(risk_free_rate)
		self.lineEdit_RiskFreeRate.setText(str(risk_free_rate))
		
		market_risk_premium = statistics.mean(market_risk_return)*12-risk_free_rate #*12 because the market risk is monthly returns
		#Market Risk Premium
		self.lineEdit_MarketRiskPremium.setText(str(round(market_risk_premium,4)))
		#Cost of equity
		cost_of_equity = risk_free_rate + (beta*(market_risk_premium))
		
		self.lineEdit_CostOfEquity.setText(str(round(cost_of_equity*100,4)))
		
		return cost_of_equity, current_price, shares_outstanding, mkt_cap
	
	#CALCULATE WACC----------------------------------------------------------------------------------	
	def value_wacc(self, current_debt_latest_fy, long_term_debt_latest_fy, mkt_cap, cost_of_debt, cost_of_equity):
		#Debt
		debt = current_debt_latest_fy + long_term_debt_latest_fy
		print(debt,"Debt here")

		value = debt + mkt_cap
		print(value,"value here")
		print(cost_of_debt,"cod here")
		#Corporate Tax Rate
		url = 'https://data.gov.sg/api/action/datastore_search?resource_id=28daa03f-c755-4688-9e94-6cc1dd3819a0&limit=5'
		#Function: Using request for API
		resp=requests.get(url)
		#Function: Getting JSON results from API
		r=resp.json()
		#Function: Reading JSON and selecting appropriate result required
		data = r.get('result').get('records')[-1]['tax_rate']
		tax_rate = float(data)/100		
		self.lineEdit_CorporateTaxRate.setText(str(tax_rate))
		
		WACC = cost_of_debt*(1-tax_rate)*(debt/value) + cost_of_equity*(mkt_cap/value)
		
		#For testing
		self.lineEdit_Debt.setText(str(debt))

		self.lineEdit_WeightedAverageCostOfCapital.setText(str(round(WACC*100,4)))
		
		return WACC, debt
	
	#CALCULATE CASH FLOW----------------------------------------------------------------------------------
	def calculate_cash_flow(self, WACC):
		#Growth Rate
		print(self.lineEdit_GrowthRateOfCompany.text(), type(self.lineEdit_GrowthRateOfCompany.text()))
		growth_rate_of_company = 0		
		if str(self.lineEdit_GrowthRateOfCompany.text()) != '':		
			growth_rate_of_company = float(str(self.lineEdit_GrowthRateOfCompany.text()))
		
		if str(self.lineEdit_GrowthRateOfCompany.text()) == '':
			cash_flow_1 = float(str(self.lineEdit_CashFlow1.text()))
			cash_flow_2 = float(str(self.lineEdit_CashFlow2.text()))
			cash_flow_3 = float(str(self.lineEdit_CashFlow3.text()))			
			cash_flow_4 = float(str(self.lineEdit_CashFlow4.text()))
			cash_flow_5 = float(str(self.lineEdit_CashFlow5.text()))
		else:
			#Five Cash flows
			print(self.lineEdit_CashFlow1.text(), type(self.lineEdit_CashFlow1.text()))
			cash_flow_1 = float(str(self.lineEdit_CashFlow1.text()))
			print(self.lineEdit_CashFlow2.text(), type(self.lineEdit_CashFlow2.text()))
			
			if float(str(self.lineEdit_CashFlow2.text())) == 0.0:
				cash_flow_2 = cash_flow_1 *(1+ growth_rate_of_company/100)
			else:
				cash_flow_2 = float(str(self.lineEdit_CashFlow2.text()))
			self.lineEdit_CashFlow2.setText(str(round(cash_flow_2,4)))
			
			print(self.lineEdit_CashFlow3.text(), type(self.lineEdit_CashFlow3.text()))
			if float(str(self.lineEdit_CashFlow3.text())) == 0.0:
				cash_flow_3 = cash_flow_2 *(1+ growth_rate_of_company/100)
			else:
				cash_flow_3 = float(str(self.lineEdit_CashFlow3.text()))
			self.lineEdit_CashFlow3.setText(str(round(cash_flow_3,4)))
			
			print(self.lineEdit_CashFlow4.text(), type(self.lineEdit_CashFlow4.text()))
			if float(str(self.lineEdit_CashFlow4.text())) == 0.0:
				cash_flow_4 = cash_flow_3 *(1+ growth_rate_of_company/100)
			else:
				cash_flow_4 = float(str(self.lineEdit_CashFlow4.text()))
			self.lineEdit_CashFlow4.setText(str(round(cash_flow_4,4)))
			
			print(self.lineEdit_CashFlow5.text(), type(self.lineEdit_CashFlow5.text()))
			if float(str(self.lineEdit_CashFlow5.text())) == 0.0:
				cash_flow_5 = cash_flow_4 *(1+ growth_rate_of_company/100)
			else:
				cash_flow_5 = float(str(self.lineEdit_CashFlow5.text()))
			self.lineEdit_CashFlow5.setText(str(round(cash_flow_5,4)))
		
		#PV calculation
		cashFlows = [cash_flow_1, cash_flow_2, cash_flow_3, cash_flow_4, cash_flow_5]
		index = 0
	  
		for cf in cashFlows:
			if cf==0:
				index = cashFlows.index(cf)
				break
			else:
				index = len(cashFlows)
		
		present_values = 0
		#Sum of PVs
		for i in range(index):
			present_values = present_values + (cashFlows[i]/((1+WACC)**(i+1)))	
		present_values = round(present_values,4)

		self.lineEdit_CashFlowCalculator.setText(str(present_values))
		
		return present_values, cashFlows, index
	
	#CALCULATE TERMINAL VALUE-----------------------------------------------------------------------------	
	def calculate_terminal_value(self, cashFlows, index, WACC, present_values):
		#CALCULATE CASH FLOW
		print(self.lineEdit_LongTermGrowth.text(), type(self.lineEdit_LongTermGrowth.text()))
		long_term_growth_rate = float(self.lineEdit_LongTermGrowth.text())
        
		#CALCULATE TERMINAL VALUE
		terminal_value = (cashFlows[index-1] * (1 + long_term_growth_rate/100)) / (WACC - long_term_growth_rate/100)	

		self.lineEdit_WACC_fromAbove.setText(str(round(WACC*100,4)))
		self.lineEdit_TerminalValue.setText(str(round(terminal_value,4)))
		
		return terminal_value
	
	#CALCULATE ENTERPRISE VALUE---------------------------------------------------------------------------
	def calculate_enterprise_value(self, terminal_value, WACC, cashFlows, present_values):
		
		pv_terminal_value = round(terminal_value/((1 + WACC)**len(cashFlows)),4)
		enterprise_value = round((pv_terminal_value + present_values),4)
		#For testing
		self.lineEdit_PVOfTerminalValue.setText(str(pv_terminal_value))
		self.lineEdit_SumOfPVOfFCFS.setText(str(round(present_values,4)))
		self.lineEdit_EnterpriseValue.setText(str(enterprise_value))
		return enterprise_value
	
	#Estimated Share Price Calculator---------------------------------------------------------------------
	def calculate_estimated_shareprice(self, enterprise_value, shares_outstanding, debt, current_price):
	
		self.lineEdit_EnterpriseValue_fromAbove.setText(str(enterprise_value))

		self.lineEdit_NoOfOrdinarySharesOutstanding.setText(str(shares_outstanding))
		self.lineEdit_CurrentYearDebt.setText(str(debt))
		estimated_share_price = round((enterprise_value-debt)/shares_outstanding,4)
        
		self.lineEdit_EstimatedSharePrice.setText(str(estimated_share_price))

		self.lineEdit_CurrentSharePrice.setText(str(current_price))
		self.lineEdit_EstimatedSharePrice_fromAbove.setText(str(round(estimated_share_price,4)))
		if estimated_share_price > current_price:
			self.lineEdit_Result.setText(str("UNDERVALUED"))
		elif estimated_share_price < current_price:
			self.lineEdit_Result.setText(str("OVERVALUED"))
		else:
			self.lineEdit_Result.setText(str("ON HOLD"))

	
	def calculateAll(self):
		#Function: calling of functions with parameters
		cost_of_debt, current_debt_latest_fy, long_term_debt_latest_fy = self.costOfDebt()
		cost_of_equity, current_price, shares_outstanding, mkt_cap = self.costOfEquity()
		WACC, debt = self.value_wacc(current_debt_latest_fy, long_term_debt_latest_fy, mkt_cap, cost_of_debt, cost_of_equity)
		present_values, cashFlows, index = self.calculate_cash_flow(WACC)
		terminal_value = self.calculate_terminal_value(cashFlows, index, WACC, present_values)
		enterprise_value = self.calculate_enterprise_value(terminal_value, WACC, cashFlows, present_values)
		self.calculate_estimated_shareprice(enterprise_value, shares_outstanding, debt, current_price)
		
            
if __name__ == '__main__':

    app = QApplication(sys.argv)
    main = Main()
    main.show()
    sys.exit(app.exec_())		