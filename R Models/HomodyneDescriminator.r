"HomodyneDes" <-
function (price.list)
{
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
			
			if (Im[i] != 0 && Re[i] != 0) {
				Period[i] <- 360/(atan(Im[i]/Re[i])*57.2957795)
			}
			
			if (Period[i] > 1.5*Period[i-1]) {
				Period[i] <- 1.5*Period[i-1]
			}
			
			if (Period[i] < 0.67*Period[i-1]) {
				Period[i] <- 0.67*Period[i-1]
			}
			
			if (Period[i] < 6) {Period[i] <- 6}
			if (Period[i] > 50) {Period[i] <- 50}
			
			Period[i] <- 0.2*Period[i] + 0.8*Period[i-1]
			SmoothPeriod[i] <- 0.33*Period[i] + 0.67*SmoothPeriod[i-1]
		
		} 
	}
	#plot(InstPeriod)
	return (SmoothPeriod)
}