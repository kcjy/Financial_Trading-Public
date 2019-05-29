"SinewaveIndicator" <-
function (price.list)
{
	library(aspace)
	
	size <- length(price.list)
	Period <- array(0, size)
	Detrender <- array(0, size)
	I1 <- array(0, size)
	Q1 <- array(0, size)
	jI <- array(0, size)
	jQ <- array(0, size)
	I2 <- array(0, size)
	Q2 <- array(0, size)
	Re <- array(0, size)
	Im <- array(0, size)
	Phase <- array(0, size)
	Smooth <- array(0, size)
	SmoothPeriod <- array(0, size)
	SmoothPrice <- array(0, size)
	DCPeriod <- array(0, size)
	RealPart <- array(0, size)
	ImagPart <- array(0, size)
	DCPhase <- array(0, size)
	ITrend <- array(0, size)
	TrendLine <- array(0, size)
	Trend <- array(0, size)
	DaysInTrend <- 0
	Summary <- NULL
	
	for (i in 1:size) {
		if (i >= 4) {
			Smooth[i] <- (4*price.list[i] + 3*price.list[i-1] + 2*price.list[i-2] + price.list[i-3])/10
		}
		if (i > 6) {
			Detrender[i] <- (0.0962*Smooth[i] + 0.5769*Smooth[i-2] - 0.5769*Smooth[i-4] - 0.0962*Smooth[i-6])*(0.075*Period[i-1] + 0.54)
			
			#Computer InPhase and Quadrature components
			Q1[i] <- (0.0962*Detrender[i] + 0.5769*Detrender[i-2] - 0.5769*Detrender[i-4] - 0.0962*Detrender[i-6])*(0.075*Period[i-1] + 0.54)
			I1[i] <- Detrender[i-3]
			
			#Advance the phase of I1 and Q1 by 90 degrees
			jI[i] <- (0.0962*I1[i] + 0.5769*I1[i-2] - 0.5769*I1[i-4] - 0.962*I1[i-6])*(0.075*Period[i-1] + 0.54)
			jQ[i] <- (0.0962*Q1[i] + 0.5769*Q1[i-2] - 0.5769*Q1[i-4] - 0.962*Q1[i-6])*(0.075*Period[i-1] + 0.54)
			
			#Phasor addition for 3 bar averaging
			I2[i] <- I1[i] - jQ[i]
			Q2[i] <- Q1[i] + jI[i]
			
			#Smooth the I and Q components before applying the discriminator
			I2[i] <- 0.2*I2[i] + 0.8*I2[i-1]
			Q2[i] <- 0.2*Q2[i] + 0.8*Q2[i-1]
			
			#Homodyne Discriminator
			Re[i] <- I2[i]*I2[i-1] + Q2[i]*Q2[i-1]
			Im[i] <- I2[i]*Q2[i-1] - Q2[i]*I2[i-1]
			Re[i] <- 0.2*Re[i] + 0.8*Re[i-1]
			Im[i] <- 0.2*Im[i] + 0.8*Im[i-1]
			
			if (Im[i] != 0 && Re[i] != 0) {Period[i] <- 360/(atan_d(Im[i]/Re[i]))}
			if (Period[i] > 1.5*Period[i-1]) {Period[i] <- 1.5*Period[i-1]}
			if (Period[i] < 0.67*Period[i-1]) {Period[i] <- 0.67*Period[i-1]}
			if (Period[i] < 6) {Period[i] <- 6}
			if (Period[i] > 50) {Period[i] <- 50}
			
			Period[i] <- 0.2*Period[i] + 0.8*Period[i-1]
			SmoothPeriod[i] <- 0.33*Period[i] + 0.67*SmoothPeriod[i-1]
			
			#Compute Dominant Cycle Phase
			SmoothPrice[i] <- (4*price.list[i] + 3*price.list[i-1] + 2*price.list[i-2] + price.list[i-3])/10
			DCPeriod[i] <- trunc(SmoothPeriod[i] + 0.5)
			if (DCPeriod[i] > 0) {
				count <- 0
				for (count in 0:(DCPeriod[i] - 1)) {
					#RealPart[i] <- RealPart[i] + cos_d(360*count/DCPeriod[i])*(SmoothPrice[i-count])
					RealPart[i] <- RealPart[i] + sin_d(360*count/DCPeriod[i])*(SmoothPrice[i-count])
					#ImagPart[i] <- ImagPart[i] + sin_d(360*count/DCPeriod[i])*(SmoothPrice[i-count])
					ImagPart[i] <- ImagPart[i] + cos_d(360*count/DCPeriod[i])*(SmoothPrice[i-count])
					
					ITrend[i] <- ITrend[i] + price.list[i-count]
				}
				if (abs(ImagPart[i]) > 0) {DCPhase[i] <- atan_d(RealPart[i]/ImagPart[i])}
				#if (abs(ImagPart[i]) > 0) {DCPhase[i] <- atan_d(ImagPart[i]/RealPart[i])}
				if (abs(ImagPart[i]) <= 0.001) {DCPhase[i] <- DCPhase[i] + 90*sign(RealPart[i])}
				DCPhase[i] <- DCPhase[i] + 90
				
				#Compensate for one bar lag of the Weighted Moving Average
				DCPhase[i] <- DCPhase[i] + 360/SmoothPeriod[i]
				
				if (ImagPart[i] < 0) {DCPhase[i] <- DCPhase[i] + 180 }
				if (DCPhase[i] > 315) {DCPhase[i] <- DCPhase[i] - 360 }
				
				#Compute Trendline as a simple average over the measured dominant cycle period
				ITrend[i] <- ITrend[i]/DCPeriod[i]
				TrendLine[i] <- (4*ITrend[i] + 3*ITrend[i-1] + 2*ITrend[i-2] + ITrend[i-3])/10
				if (i < 12) {TrendLine[i] <- price.list[i]}
			}
			
			#Assume Trend Mode
			Trend[i] <- 1
			
			#Measure days in trend from last crossing of the Sinewave Indicator lines
			if ((sin_d(DCPhase[i-1]) < sin_d(DCPhase[i-1] + 45) && sin_d(DCPhase[i]) > sin_d(DCPhase[i] + 45)) || #Cross over
				(sin_d(DCPhase[i-1]) > sin_d(DCPhase[i-1] + 45) && sin_d(DCPhase[i]) < sin_d(DCPhase[i] + 45))) { #Cross under
				DaysInTrend <- 0
				Trend[i] <- 0
			}
			DaysInTrend <- DaysInTrend + 1
			if (DaysInTrend < 0.5*SmoothPeriod[i]) {Trend[i] <- 0}
			
			#Cycle Mode if delta phase is +/- 50% of dominant cycle change of phase
			if (SmoothPeriod[i] != 0 && (DCPhase[i] - DCPhase[i-1]) >  0.67*360/SmoothPeriod[i] && (DCPhase[i] - DCPhase[i-1]) < 1.5*360/SmoothPeriod[i]) {
				Trend[i] <- 0
			}
			
			#Trend Mode if prices are widely separated from the TrendLine
			if (abs((SmoothPrice[i] - TrendLine[i])/TrendLine[i]) >= 0.015) {Trend[i] <- 1}
			
			
		} 
		summary.row <- data.frame(number=i, price=price.list[i], smooth.price=SmoothPrice[i], trend.line=TrendLine[i], trend=Trend[i], dc.period=DCPeriod[i], real.part=RealPart[i], imag.part=ImagPart[i], dc.phase=DCPhase[i])
		if (is.null(Summary$number)) {
			Summary <- summary.row
		} else {
			Summary <- rbind(Summary, summary.row)
		}
			
	}
	par(mfrow=c(3,1))
	plot(price.list, type="l")
	lines(TrendLine, type="l", col="blue")
	lines(SmoothPrice, type="l", col="red")
	plot(Trend, type="l")
	plot(sin_d(DCPhase), type="l", col="blue")
	lines(sin_d(DCPhase + 45), type="l", col="red")
	
	return (Summary)
}