"PhaseAccumulation" <-
function (price.list)
{
	size <- length(price.list)
	Period <- array(0, size)
	Detrender <- array(0, size)
	I1 <- array(0, size)
	Q1 <- array(0, size)
	InstPeriod <- array(0, size)
	Phase <- array(0, size)
	Smooth <- array(0, size)
	DeltaPhase <- array(0, size)
	
	for (i in 1:size) {
		if (i >= 4) {
			Smooth[i] <- (4*price.list[i] + 3*price.list[i-1] + 2*price.list[i-2] + price.list[i-3])/10
		}
		if (i > 6) {
			Detrender[i] <- (0.0962*Smooth[i] + 0.5769*Smooth[i-2] - 0.5769*Smooth[i-4] - 0.0962*Smooth[i-6])*(0.075*Period[i-1] + 0.54)
			
			#Computer InPhase and Quadrature components
			Q1[i] <- (0.0962*Detrender[i] + 0.5769*Detrender[i-2] - 0.5769*Detrender[i-4] - 0.0962*Detrender[i-6])*(0.075*Period[i-1] + 0.54)
			I1[i] <- Detrender[i-3]
			
			#Smooth the I and Q components before applying the discriminator
			I1[i] <- 0.15*I1[i] + 0.85*I1[i-1]
			Q1[i] <- 0.15*Q1[i] + 0.85*Q1[i-1]
			
			#Use ArcTangent to compute the current phase
			if (abs(I1[i]) > 0) {
				Phase[i] <- atan(abs(Q1[i]/I1[i]))*57.2957795
				#Phase[i] <- atan(abs(Q1[i]/I1[i]))
			}	
			
			#Resolve the ArcTangent ambiguity for auadrants 2, 3, and 4
			if (I1[i]<0 && Q1[i]>0) {Phase[i] <- 180 - Phase[i]}
			if (I1[i]<0 && Q1[i]<0) {Phase[i] <- 180 + Phase[i]}
			if (I1[i]>0 && Q1[i]<0) {Phase[i] <- 360 - Phase[i]}
			
			#Compute a differential phase, resolve phase warparound from quadrant 1 to quadrant 4, and limit delta phase errors
			DeltaPhase[i] <- Phase[i-1] - Phase[i]
			if (Phase[i-1] < 90 && Phase[i] > 270) {
				DeltaPhase[i] <- 360 + Phase[i-1] - Phase[i]
			}
			
			#Limit DeltaPhase to be within the bounds of 6 bar and 50 bar cycles
			if (DeltaPhase[i] < 7) {DeltaPhase[i] <- 7}
			if (DeltaPhase[i] > 60) {DeltaPhase[i] <- 60}
			
			#Sum DeltaPhase to reach 360 degrees. The sum is the instananeous period
			PhaseSum <- 0
			k <- 0
			InstPeriod[i] <- 0
			while (PhaseSum <= 360) {
				PhaseSum <- PhaseSum + DeltaPhase[i-k]
				#if (k==39 || k==(i-1)) {break}
				if (k==(i-1)) {break}
				k <- k+1
			}
			#cat("PhaseSum is ",PhaseSum, "\n")
			#cat("K is ",k,"\n")
			#if (PhaseSum >= 360 && InstPeriod[i]==0) {
				InstPeriod[i] <- k
			#}
			
			#Resolve Instantaneous Period errors and smooth
			if (InstPeriod[i] == 0) {
				InstPeriod[i] <- InstPeriod[i-1]
			}
			Period[i] <- 0.25*InstPeriod[i] + 0.75*Period[i-1]
		
		} 
	}
	#plot(InstPeriod)
	return (list(period=Period,instperiod=InstPeriod))
}