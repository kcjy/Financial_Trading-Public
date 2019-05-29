PerformTest <- function () {
	library(wavelets)
	library(AMORE)
	library(wmtsa)
	library(plyr)
	library(tseries)

	#Initialize the parameters
	DirectionList= c("Original") #,"Predicted") #"Predicted", "Original"
	c6<- wt.filter("c6")
	d4=c(0.4829629131445341, 0.8365163037378077, 0.2241438680420134, -0.1294095225512603)
	D6=c(0.3326705529500827, 0.8068915093110928, 0.4598775021184915,-0.1350110200102546, -0.0854412738820267, 0.0352262918857096)
	D8=c(0.2303778133074431, 0.7148465705484058, 0.6308807679358788, -0.0279837694166834, -0.1870348117179132, 0.0308413818353661, 0.0328830116666778, -0.0105974017850021)
	D10=c(0.1601023979741930, 0.6038292697971898, 0.7243085284377729, 0.1384281459013204, -0.2422948870663824, -0.0322448695846381, 0.0775714938400459, -0.0062414902127983, -0.0125807519990820, 0.0033357252854738)
	ScalingTypeList= c("Lin") #, NonLin, 
	WaveletFunc= "d4" #c("d2","d4","d6","d8","d10","d12","d14","d16", "d18", "d20") #
	NLevels=9
	Seed= 1234
	NNeurons=c((NLevels+1),50,1)
	MiddleNeurons=50
	HiddenLayer= "tansig" #c("tansig","sigmoid") #
	OutputLayer="purelin"
	Method="ADAPTgdwm"
	GlobalLearningRate=4e-2
	GlobalMomentum=0.5
	ErrorCriteria= "LMS" #LMS", "LMLS"
	Stao=NA
	Steps=1000 
	NShows=1
	Report=TRUE
	RollingWindowList = c(550) #c(100,200,300, 500,800, 1000, 1250, 150, 1750, 2000, 2250,2500)
	InitialAmount=1000
	RollingIterations= 10 #floor((length(FileData[,])-(1-SplitRatio)*RollingWindow -1)/(SplitRatio*RollingWindow))

	#Read Index data
	FileData <- read.csv("Nifty Index.csv", header=TRUE)
	PriceData <- FileData[,2]
	
	# Main Program
	test.frame <- list()
	NMSE <- array()

	for (z in 1:(length(ScalingTypeList))) {
		ScalingType <- ScalingTypeList[z]

		for (t in 1:(length(RollingWindowList))) { 
			RollingWindow <- RollingWindowList[t]
			SplitRatio <- 1/RollingWindow
			
			for (d in 1:(length(DirectionList))) { 
				Direction <- DirectionList[d]
				
				for (Iteration in 1:RollingIterations) {
					StartIndex <- 1+(Iteration-1)*SplitRatio*RollingWindow
					flag <- FALSE

					PredictedValue=0
					ReturnList=0  	       
					PredictedValue <- array(1:((SplitRatio*RollingWindow+1)*2),c((SplitRatio*RollingWindow+1),2))
					PredictedValue[]=0 

					TrainData <- PriceData[StartIndex:(StartIndex+RollingWindow*(1-SplitRatio))]
					In.TrainData <- TrainData[1:(RollingWindow*(1-SplitRatio))]
					Out.TrainData <- TrainData[2:(RollingWindow*(1-SplitRatio)+1)]
					Scaled.In.TrainData <- GetScaledData(In.TrainData,max(TrainData),min(TrainData),ScalingType)
					Scaled.Out.TrainData <- GetScaledData(Out.TrainData,max(TrainData),min(TrainData),ScalingType)
					TrainDWT <- GetDWT(Scaled.In.TrainData, WaveletFunc, NLevels)
					Reconst.TrainDWT <- GetReconstructed(TrainDWT)
					#plot(Reconst.TrainDWT[,1])
					set.seed(Seed)

					net.start15 <- GetNN (NNeurons, GlobalLearningRate, GlobalMomentum, ErrorCriteria, Stao, HiddenLayer, OutputLayer, Method)  
					train15 <- train(net.start15, Reconst.TrainDWT, Scaled.Out.TrainData, error.criterium=ErrorCriteria, report=Report, show.step=Steps, n.shows=NShows)
	  
					for(i in 1:(RollingWindow*SplitRatio)) {
						TestData <- PriceData[(StartIndex):(StartIndex+RollingWindow*(1-SplitRatio)+i-1)]
						Scaled.TestData <- GetScaledData(TestData, max(TrainData), min(TrainData), ScalingType)
						TestDWT <- GetDWT(Scaled.TestData, WaveletFunc, NLevels)
						Reconst.TestDWT <- GetReconstructed(TestDWT) 

						Scaled.Prediction <- GetPrediction(train15$net, Reconst.TestDWT[(RollingWindow*(1-SplitRatio)+i-1):(RollingWindow*(1-SplitRatio)+i),1:(NLevels+1)])
						Prediction <- GetUnscaledData (Scaled.Prediction, TrainData, ScalingType)

						PredictedValue[(1+i-1):(1+i),1:2] <- cbind(PriceData[(length(TestData)+StartIndex-1):(length(TestData)+StartIndex)], Prediction[(length(Prediction)-1):length(Prediction)])##y_1[100]
					}
				}
			}
		}	
	return test.frame
	
}
# Get the DWT
GetDWT <- function(norm.x, WaveletFunc, NLevels)
{
  input=as.matrix(norm.x)
  input=as.numeric(input[])
  # library(wmtsa)
  # result <- wavDWT(input[1:RollingWindow], wavelet=WaveletFunc, keep.series=TRUE, n.levels=NLevels)
  result <- wavDWT(input, wavelet=WaveletFunc, keep.series=TRUE, n.levels=NLevels) 
 GetDWT <- result
 }

 
# Get reconstructed vectors
 GetReconstructed <- function(result)
{
#A10= wavMRD(result)[["S10"]]
#A10= wavMRD(result)[["S10"]]
#D10=wavMRD(result)[["D10"]]
A9= wavMRD(result)[["S9"]]
D9=wavMRD(result)[["D9"]]
D8=wavMRD(result)[["D8"]]
D7=wavMRD(result)[["D7"]]
D6=wavMRD(result)[["D6"]]
D5=wavMRD(result)[["D5"]]
D4=wavMRD(result)[["D4"]]
D3=wavMRD(result)[["D3"]]
D2=wavMRD(result)[["D2"]]
D1=wavMRD(result)[["D1"]]
#A5=A6+D6
#A4=A5+D5
#A3=A4+D4
#A2=A3+D3
#A1=A2+D2

A<- list()
#   A[[1]]=A1
#   A[[2]]=A2
#   A[[3]]=A3
#   A[[4]]=A4
#   A[[5]]=A5
#   A[[6]]=A6
   A[[1]]=D1
   A[[2]]=D2
   A[[3]]=D3
   A[[4]]=D4
  # A[[5]]=A4
   A[[5]]=D5
   A[[6]]=D6
 #  A[[7]]=A6
   A[[7]]=D7
   A[[8]]=D8
   A[[9]]=D9
   A[[10]]=A9
  # A[[11]]=A10
norm.MA=t(as.matrix(ldply(A)))

GetReconstructed <- norm.MA
}

# Get NN
GetNN <- function(NNeurons, GlobalLearningRate, GlobalMomentum, ErrorCriteria, Stao, HiddenLayer, OutputLayer, Method)
{
net.start15 <- newff(n.neurons=NNeurons, learning.rate.global=GlobalLearningRate, momentum.global= GlobalMomentum, error.criterium=ErrorCriteria, Stao=Stao, hidden.layer=HiddenLayer, output.layer=OutputLayer, method=Method)
GetNN <- net.start15
}

## Predict the future pattern
GetPrediction <- function(Net, norm.MA)
{
norm.y <- sim(Net, norm.MA)
GetPrediction <- norm.y
}

# Gets the scaled data as per specified scaling
GetScaledData <- function(x,maxx, minx, ScalingType)
{
  if (ScalingType == "Lin")
   {
    #norm.x=(x-(max(x)+min(x))/2)/(max(x)-min(x))
	#norm.x=(x-min(x))/(max(x)-min(x))
   #norm.x=(x-min(x)*0.9)/(1.5*max(x)-0.9*min(x))
   norm.x=(x- minx)/( maxx- minx)
   }
  
  if (ScalingType == "Norm")
   {
    norm.x= scale(x)
   }
GetScaledData=norm.x   
}

# Gets unscaled data as per specified scaling
GetUnscaledData <- function(norm.x, x, ScalingType)
{
  if (ScalingType == "Lin")
   {
   # y=(norm.x)*(max(x)-min(x))+ min(x)
   #y=norm.x*(max(x)-min(x))+ (max(x)+min(x))/2
	y=norm.x*(max(x)- min(x))+ min(x)
   }
  
  if (ScalingType == "Norm")
   {
    y= norm.x*sd(x)+mean(x)
   }
GetUnscaledData= y
}

# Calculates the returns of a given list
CalculateReturn<- function(PriceList)
{
Len = length(PriceList)
ReturnList= array()
  for(k in 2:Len)
  { ReturnList[k-1]= (PriceList[k]-PriceList[k-1])/PriceList[k-1]*100
   }
   CalculateReturn= ReturnList
}

# Calculate the real returns of a given list
CalculateRealReturn<- function(OriginalPriceList, PredictedPriceList)
{
Len= length(OriginalPriceList)
ReturnList= array()
  for(k in 2:Len)
  { ReturnList[k-1]= (PredictedPriceList[k]-OriginalPriceList[k-1])/OriginalPriceList[k-1]*100
   }
   CalculateRealReturn= ReturnList
}

# Calculates the correct sign predictions
CalculateSignPrediction <- function(OriginalReturnList, PredictedReturnList)
{
Len= length(OriginalReturnList)
count=0
  for (p in 1: Len)
   { 
    if (sign(OriginalReturnList[p]) == sign(PredictedReturnList[p])) 
	{count= count+1  }
    }
	CalculateSignPrediction = count
}

#Calculates value of the portfolio for some Initial Investment Value
CalculateInvestmentValue <- function(OriginalReturnList,PredictedReturnList, InitialAmount)
{
Len= length(OriginalReturnList)
Value=array()
Value[1]= InitialAmount

  for(k in 1:Len)
  {
    if(sign(OriginalReturnList[k])==sign(PredictedReturnList[k]))
     {Value[k+1]=Value[k]*(1+(abs(OriginalReturnList[k])-0.047*2)/100)}
	#  {Value[k+1]=Value[k]*(1+(abs(OriginalReturnList[k]*100/15)-0.047*2)/100)}
	 else    
	 {Value[k+1]=Value[k]*(1-(abs(OriginalReturnList[k])+0.047*2)/100)}
	#  {Value[k+1]=Value[k]*(1-(abs(OriginalReturnList[k]*100/15)+0.047*2)/100)}
   }
   CalculateInvestmentValue = Value
}

#Calculate max drawdown of the portfolio
CalculateMaxDrawDown <- function (InvestmentValueList)
{
	#	library(tseries)
		mdd <- maxdrawdown(InvestmentValueList)
		mdrawdown <- 0
		if (mdd$maxdrawdown > 0) 
		{
		mdrawdown <- (InvestmentValueList[mdd$to]-InvestmentValueList[mdd$from])/InvestmentValueList[mdd$from]*100
		}
	CalculateMaxDrawDown = mdrawdown
}

#Gives result summary for a scenario
GetResultSummary <- function(OriginalPriceList, PredictedPriceList, SignCount, RealReturnList, InvestmentValueList)
{
  PriceCorr = cor(OriginalPriceList, PredictedPriceList)
  OriginalReturnList = CalculateReturn(OriginalPriceList)
  PredictedReturnList = CalculateReturn(PredictedPriceList)
  RetCorr = cor(OriginalReturnList, PredictedReturnList)  
  CorrectSignPercent = SignCount/length( OriginalReturnList)*100
  AvgDailyRet = mean(RealReturnList)
  AvgDailyVol = sd (RealReturnList)
  SharpeRatio =(AvgDailyRet*252)/(AvgDailyVol*sqrt(252))
  Len = length(InvestmentValueList)
  ActualRet = (InvestmentValueList[Len]-InitialAmount)/InitialAmount*100
  MinRet = min(RealReturnList)
  MaxRet = max(RealReturnList)
  mdrawdown= CalculateMaxDrawDown(InvestmentValueList)

  test.row <- data.frame(PriceCorr= PriceCorr, RetCorr=RetCorr, CorrectSignPercent= CorrectSignPercent,  AvgDailyRet=AvgDailyRet, AvgDailyVol=AvgDailyVol, SharpeRatio= SharpeRatio, ActualRet= ActualRet, MinRet=MinRet, MaxRet=MaxRet, MaxDrawDown=mdrawdown)
  GetResultSummary = test.row
}

# Set the optimal parameters
Set<- function(RollingWindow, SplitRatio, WaveletFunc, ScalingType, MiddleNeurons, HiddenLayer)
{
NNeurons[2] = MiddleNeurons
Optimal <-list()
Optimal[1] = RollingWindow
Optimal[2] = SplitRatio
Optimal[3] = WaveletFunc
Optimal[4] = ScalingType
Optimal[5]=NNeurons
Optimal[6] = HiddenLayer
Set <- Optimal
}

# Calculating NMSE
CalculateNMSE <-function(OriginalList, PredictedList)
{

Error = (OriginalList-PredictedList)*(OriginalList-PredictedList)
Noise = (PredictedList-mean(OriginalList))*(PredictedList-mean(OriginalList))
NMSE=sum(Error)/sum(Noise);

CalculateNMSE <- NMSE
}
 

 
