plotGaze <- function(eye_data, subjectID, trial_num, opt_title="") {


	eye_data.current <- eye_data[eye_data$trial == trial_num,]
	eye_data.current <- eye_data.current[eye_data.current$subjectID == subjectID,]



	if (eye_data.current$gamble[1] == 1) {
		bg_col ="lavenderblush"
	} else {
		bg_col ="aliceblue"
	}
	
	par( mgp = c(0, 0, -5)  )
	
	ymax <- max(eye_data.current$mag2, eye_data.current$mag1, eye_data.current$sure1)
	yrange <- c(0, ymax + round(ymax/5)) # set
	plot(eye_data.current$trialTime, eye_data.current$mag1, ylab="", xlab="", ylim=yrange, cex.main=1 , cex.axis=.5, col.axis="gray75", xaxt="n", axes=F)
	#axis(1, line=-1, cex=0.3)
	# for background
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg_col)
	lines(eye_data.current$trialTime, eye_data.current$mag1, col='green4', lty="longdash") 
	lines(eye_data.current$trialTime, eye_data.current$mag2, col='red', lty="dashed")
	lines(eye_data.current$trialTime, eye_data.current$sure1, col='blue', lty="solid")
	axis(2, line=-.75, padj=-.25, tck=0.05, cex.axis=0.5)

	
}
