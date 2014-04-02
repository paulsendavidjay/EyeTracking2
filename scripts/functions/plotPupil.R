plotPupil <- function(eye_data, beh_data, subjectID, trial_num, opt_title="") {

	opt_title <- paste(c("subj ", subjectID, " trial: ", i), sep="", collapse="")
	beh_data.current <- beh_data[beh_data$trial == trial_num,]

	eye_data.current <- eye_data[eye_data$trial == trial_num,]
	eye_data.current <- eye_data.current[eye_data.current$subjectID == subjectID,]

	eye_data.current$trialTime2 <- eye_data.current$tet_timeStamp - eye_data.current$tet_timeStamp[1]


	eye_data.current$left_pup_diam[eye_data.current$left_pup_diam==-1] <- NA
	eye_data.current$right_pup_diam[eye_data.current$right_pup_diam==-1] <- NA
	eye_data.current$left_pup_diam[eye_data.current$left_pup_diam==-Inf] <- NA
	eye_data.current$left_pup_diam[eye_data.current$left_pup_diam==Inf] <- NA
	eye_data.current$right_pup_diam[eye_data.current$right_pup_diam==-Inf] <- NA
	eye_data.current$right_pup_diam[eye_data.current$right_pup_diam==Inf] <- NA



	ymax <- max(eye_data.current$right_pup_diam, eye_data.current$left_pup_diam, na.rm=TRUE)
	yrange <- c(0, ymax + round(ymax/5)) # set

	
	presTime <- beh_data.current$presTime - beh_data.current$fixTime
	decisionTime <- beh_data.current$decisionTime - beh_data.current$fixTime
	respTime <- beh_data.current$respTime - beh_data.current$fixTime
	outcomeTime <- beh_data.current$outcomeTime - beh_data.current$fixTime




	if (eye_data.current$gamble[1] == 1) {
		bg_col ="gray90"
	} else {
		bg_col ="white"
	}
	
	par( mgp = c(0, 0, 0)  )
	

	plot(eye_data.current$trialTime2, eye_data.current$left_pup_diam, ylab="", xlab="", ylim=yrange, cex.main=1 , cex.axis=.5, col.axis="gray75", xaxt="n", axes=F, main= opt_title)

	# for background
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg_col)

	nTimePoints = length(eye_data.current$trialTime2)
	ytop <- rep(par("usr")[3], times= nTimePoints)
	ybottom <- rep(par("usr")[4], times= nTimePoints)	
	
	xfill_left_mag1 <- (eye_data.current$mag1_fix[2:nTimePoints] - eye_data.current$mag1_fix[1:nTimePoints-1]) > 0
	xfill_left_mag1 <- c(0, xfill_left_mag1)
	xfill_left_mag1 <- xfill_left_mag1 * eye_data.current$trialTime2
	xfill_left_mag1 <- xfill_left_mag1[xfill_left_mag1 > 0]
	if (sum(xfill_left_mag1) == 0) {xfill_left_mag1 <-0}
	
	xfill_right_mag1 <- (eye_data.current$mag1_fix[2:nTimePoints] - eye_data.current$mag1_fix[1:nTimePoints-1]) < 0
	xfill_right_mag1 <- c(0, xfill_right_mag1)
	xfill_right_mag1 <- xfill_right_mag1 * eye_data.current$trialTime2
	xfill_right_mag1 <- xfill_right_mag1[xfill_right_mag1 > 0]
	if (sum(xfill_right_mag1) == 0) {xfill_right_mag1 <-0}

	if (length(xfill_left_mag1) > length(xfill_right_mag1)) {
		xfill_right_mag1 <- (eye_data.current$mag1_fix[2:nTimePoints] - eye_data.current$mag1_fix[1:nTimePoints-1]) < 0
		xfill_right_mag1 <- c(0, xfill_right_mag1)
		xfill_right_mag1[length(xfill_right_mag1)] <- 1
		xfill_right_mag1 <- xfill_right_mag1 * eye_data.current$trialTime2
		xfill_right_mag1 <- xfill_right_mag1[xfill_right_mag1 > 0]		
	}
	
	ytop_mag1 <- 	ytop <- rep(par("usr")[3], times=length(xfill_right_mag1))
	ybottom_mag1 <- rep(par("usr")[4], times= length(xfill_right_mag1))
	
	
	
	
	
	xfill_left_mag2 <- (eye_data.current$mag2_fix[2:nTimePoints] - eye_data.current$mag2_fix[1:nTimePoints-1]) > 0
	xfill_left_mag2 <- c(0, xfill_left_mag2)
	xfill_left_mag2 <- xfill_left_mag2 * eye_data.current$trialTime2
	xfill_left_mag2 <- xfill_left_mag2[xfill_left_mag2 > 0]
	if (sum(xfill_left_mag2) == 0) {xfill_left_mag2<-0}
	
	xfill_right_mag2 <- (eye_data.current$mag2_fix[2:nTimePoints] - eye_data.current$mag2_fix[1:nTimePoints-1]) < 0
	xfill_right_mag2 <- c(0, xfill_right_mag2)
	xfill_right_mag2 <- xfill_right_mag2 * eye_data.current$trialTime2
	xfill_right_mag2 <- xfill_right_mag2[xfill_right_mag2 > 0]
	if (sum(xfill_right_mag2) == 0) {xfill_right_mag2 <-0}

	if (length(xfill_left_mag2) > length(xfill_right_mag2)) {
		xfill_right_mag2 <- (eye_data.current$mag2_fix[2:nTimePoints] - eye_data.current$mag2_fix[1:nTimePoints-1]) < 0
		xfill_right_mag2 <- c(0, xfill_right_mag2)
		xfill_right_mag2[length(xfill_right_mag2)] <- 1
		xfill_right_mag2 <- xfill_right_mag2 * eye_data.current$trialTime2
		xfill_right_mag2 <- xfill_right_mag2[xfill_right_mag2 > 0]
	}
	
	ytop_mag2 <- 	ytop <- rep(par("usr")[3], times=length(xfill_right_mag2))
	ybottom_mag2 <- rep(par("usr")[4], times= length(xfill_right_mag2))

	
	
	# SURE OPTION
	xfill_left_sure2 <- (eye_data.current$sure2_fix[2:nTimePoints] - eye_data.current$sure2_fix[1:nTimePoints-1]) > 0
	xfill_left_sure2 <- c(0, xfill_left_sure2)
	xfill_left_sure2 <- xfill_left_sure2 * eye_data.current$trialTime2
	xfill_left_sure2 <- xfill_left_sure2[xfill_left_sure2 > 0]
	if (sum(xfill_left_sure2) == 0) {xfill_left_sure2 <-0}
	
	xfill_right_sure2 <- (eye_data.current$sure2_fix[2:nTimePoints] - eye_data.current$sure2_fix[1:nTimePoints-1]) < 0
	xfill_right_sure2 <- c(0, xfill_right_sure2)
	xfill_right_sure2 <- xfill_right_sure2 * eye_data.current$trialTime2
	xfill_right_sure2 <- xfill_right_sure2[xfill_right_sure2 > 0]
	if (sum(xfill_right_sure2) == 0) {xfill_right_sure2 <-0}


	if (length(xfill_left_sure2) > length(xfill_right_sure2)) {
		xfill_right_sure2 <- (eye_data.current$sure2_fix[2:nTimePoints] - eye_data.current$sure2_fix[1:nTimePoints-1]) < 0
		xfill_right_sure2 <- c(0, xfill_right_sure2)
		xfill_right_sure2[length(xfill_right_sure2)] <- 1
		xfill_right_sure2 <- xfill_right_sure2 * eye_data.current$trialTime2
		xfill_right_sure2 <- xfill_right_sure2[xfill_right_sure2 > 0]	
	}

	ytop_sure2 <- 	ytop <- rep(par("usr")[3], times=length(xfill_right_sure2))
	ybottom_sure2 <- rep(par("usr")[4], times= length(xfill_right_sure2))
	

	
	rect(xfill_left_mag1, ytop_mag1, xfill_right_mag1, ybottom_mag1, col="darkseagreen1")
	rect(xfill_left_mag2, ytop_mag2, xfill_right_mag2, ybottom_mag2, col="pink")
	rect(xfill_left_sure2, ytop_sure2, xfill_right_sure2, ybottom_sure2, col="lightcyan")
	
	lines(eye_data.current$trialTime2, eye_data.current$left_pup_diam, col='black', lty="solid") 
	lines(eye_data.current$trialTime2, eye_data.current$right_pup_diam, col='black', lty="dashed")
	
	# eye_data.current$mag1_fix[eye_data.current$mag1_fix==0] <- NA
	# eye_data.current$mag2_fix[eye_data.current$mag2_fix ==0] <- NA
	# eye_data.current$sure2_fix[eye_data.current$sure2_fix ==0] <- NA	
	# lines(eye_data.current$trialTime2, eye_data.current$mag1_fix, col='green', lty="solid", lwd=14)
	# lines(eye_data.current$trialTime2, 2*eye_data.current$mag2_fix, col='red', lty="solid", lwd=14)
	# lines(eye_data.current$trialTime2, 3*eye_data.current$sure2_fix, col='blue', lty="solid", lwd=14)

	
	abline(v=presTime, col="blue", lwd=4)
	abline(v= decisionTime, col="orange", lwd=4)
	abline(v= respTime, col="red", lwd=4)
	abline(v= outcomeTime, col="red", lwd=4)
	abline(v= outcomeTime, col="green", lty="dashed", lwd=4)
		
	axis(1, line= -.75, cex=0.3, padj=-.25, cex.axis=0.5)
	axis(2, line= -.75, padj=-.25, tck=0.05, cex.axis=0.5)

	
}
