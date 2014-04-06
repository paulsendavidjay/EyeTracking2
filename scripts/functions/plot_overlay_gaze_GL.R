plot_overlay_gaze <- function(eye_data, subjectID, trial_num, epoch, opt_title="") {
	
	# identify correct study directory (different for lab vs laptop)
	study_dir <- c("/Users/Shared/EyeTrackingGaze", 
		"/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze")
	
		
	if (file.exists(study_dir[1])) { 
		study_dir <- study_dir[1] 
	} else if (file.exists(study_dir[2])) { 
		study_dir <- study_dir[2]
	} else {
		return('study directory not found')
	}

	
	library(vcd) # opens palates of colors to work with: sequential_hcl(n), heat_hcl(n), rainbow_hcl(n), where n is the number of steps
	library(plotrix) # for plotting gradient.rect
	library(png) # for plotting png overlay
	require(Hmisc) # for subplot
	
	source(paste(c(study_dir, '/scripts/functions/positions.R'   ), collapse=""))
	source(paste(c(study_dir, '/scripts/functions/cut_repeats.R'   ), collapse=""))
#	source(paste(c(study_dir, '/scripts/functions/plotGaze.R'   ), collapse=""))
	
	eye_data.current <- subset(eye_data, subjectID == subjectID)
	eye_data.current <- subset(eye_data.current, trial == trial_num)
	eye_data.current <- eye_data.current[eye_data.current$epoch == epoch,] # not sure why subset() doesn't work with this

	trialTypeLabelList <- c("Sure vs Sure", "Low Risk", "Medium Risk", "High Risk", "Very High Risk", "Risk-Return")
	trialTypeLabel <- trialTypeLabelList[eye_data.current$trialType[1]]


	################################################
	############ 	DATA INITIALIZATION
	current_title <- paste(c(subjectID, ", trial ", trial_num, ": ", opt_title), sep=" ", collapse="")
	# convert VERT zeros and ones into twos and ones for naming of backgroundFiles, until using
	current_side <- eye_data.current$side[1]
	current_vert <- eye_data.current$vert[1]
	current_gamble <- eye_data.current$gamble[1]
	trialType <- eye_data.current$trialType[1]
	current_coinsWon <- eye_data.current$coinsWon[1]
	
	s1 <- eye_data.current$sure1[1] # value of sure bet
	s2 <- eye_data.current$sure2[1] # value of sure bet
	m1 <- eye_data.current$mag1[1] # value of large magnitude outcome
	m2 <- eye_data.current$mag2[1] # value of small magnitude outcome
	
	
	################################################
	############ 	PLOTTING
	# SET PLOT PARAMETERS
	yrange <- c(1024, 0) # 
	xrange <- c(0, 1280) # 
	current_colors_vector <- heat_hcl(length(eye_data.current$GazepointX))
	if ( length(current_colors_vector) == 0) { current_colors_vector = 1 } # if there are no data points, plotting scale will throw error
	gradient.pos <- c(1150, 288, 1160, 88) # x_left, y_bottom, x_right, y_top
	choiceMargin <- 20 # margin between fixation location boundaries and dashed box indicating chosen option
	cntrFixBndry <- c(max(xrange)/2 - 20, max(yrange)/2 - 20, max(xrange)/2 + 20, max(yrange)/2 + 20)

	# DRAW/SET BASIC POT DIMENSIONS
	par(mar=c(1,1,1,1))
	plot(eye_data.current$GazepointX, eye_data.current$GazepointY, main=current_title, ylab="", xlab="", ylim=yrange, xlim=xrange, axes=F, cex.main=1 , cex.axis=.8, col=current_colors_vector)

	# LOAD AND DRAW STIMULUS IMAGE
	backgroundFileName <- paste(c(study_dir, "/data/PNG/", eye_data.current$subjectID[1], "_pngs/trial", trial_num, ".png"), collapse="")
	backgroundFileName2 <- paste(c(study_dir, "/data/PNG/_pngs/trial", trial_num, ".png"), collapse="")
	
	if ( file.exists(backgroundFileName) ) {
		img <- readPNG(backgroundFileName)
	}  else if ( file.exists(backgroundFileName2) ) {
		 img <- readPNG(backgroundFileName2)
	}
	 
	if ( exists("img") ) {
		pix_reduc <- 8
		reduced_img <- 
		
		#rasterImage(img, xleft, ybottom, xright, ytop)
		rasterImage(img, xrange[1], yrange[1], xrange[2], yrange[2])
	}
		



################################################################################################
	# DRAW BOX INDICATING CHOICE
		# DRAW BOUNDS FOR FIX LOCATION 
	current_positions <- current_frame_positions(eye_data.current$trialType[1], eye_data.current$vert[1], eye_data.current$sideMag[1], eye_data.current$sideSure[1])
	if (trialType == 1)	{
	
		if (current_gamble == 99) { 
			choice_frame_pos <- 1
		} else {
			choice_frame_pos <- 2
		}
		
		# DRAW CHOICE BOX BOUNDARY
		rect(current_positions[[choice_frame_pos]][[1]][1] - choiceMargin, current_positions[[choice_frame_pos]][[2]][1] - choiceMargin, 
			current_positions[[choice_frame_pos]][[1]][2] + choiceMargin, current_positions[[choice_frame_pos]][[2]][2] + choiceMargin, 
			col="NA", lty="dashed", border="gray50", lwd=1)
		
		# DRAW OUTCOME BOX BOUNDARY
		if (epoch == 'outcome') { 
			rect(current_positions[[choice_frame_pos]][[1]][1], current_positions[[choice_frame_pos]][[2]][1], 
				current_positions[[choice_frame_pos]][[1]][2], current_positions[[choice_frame_pos]][[2]][2], 
				col="NA", lty="dashed", border="yellow", lwd=1)	
		}

	} else if (trialType > 1 & trialType < 6) {

		xmin <- min(current_positions[[1]][[1]][1], current_positions[[2]][[1]][1])
		xmax <- max(current_positions[[1]][[1]][2], current_positions[[2]][[1]][2])

		# PLOT CHOICE BOX AND SET OUTCOME BOX
		if (current_gamble == 1) { 

			ymin <- min(current_positions[[1]][[2]][1], current_positions[[2]][[2]][1])
			ymax <- max(current_positions[[1]][[2]][2], current_positions[[2]][[2]][2])

			rect(xmin - choiceMargin, ymin - choiceMargin, 
				xmax + choiceMargin, ymax + choiceMargin, 
				col="NA", lty="dashed", border="gray50", lwd=1)
			if (current_coinsWon == m1) {
				outcome_frame_pos <- 1
			} else {
				outcome_frame_pos <- 2				
			}
		
		} else {
			outcome_frame_pos <- 3
		}
		
		# DRAW OUTCOME BOX BOUNDARY
		if (epoch == 'outcome') {
			rect(current_positions[[outcome_frame_pos]][[1]][1], current_positions[[outcome_frame_pos]][[2]][1], 
				current_positions[[outcome_frame_pos]][[1]][2], current_positions[[outcome_frame_pos]][[2]][2], 
				col="NA", lty="solid", border="yellow", lwd=1)	
		}

	} # end if trialType
	
################################################################################################

    # DRAW CENTER FIXATION BOUNDARY
    rect(cntrFixBndry[1], cntrFixBndry[2], cntrFixBndry[3], cntrFixBndry[4], col="NA", border="gray50", lwd=1)
	
	# ONLY SELECT DATA FOR WHICH WE HAVE FIXATIONS
	eye_data.current <- eye_data.current[eye_data.current$Found != "None",] # only select data for which we have fixations
	
	# IF THERE ARE POINTS IN THE EYE DATA
	if (nrow(eye_data.current) > 0) {  
		
		# DRAW POINTS AT EACH FIXATION
		points(eye_data.current$GazepointX, eye_data.current$GazepointY, col=current_colors_vector, pch=21, bg=current_colors_vector, cex=0.5, lwd=0.01)
		
		# DRAW EYE TRACES
		for (j in 1:(length(eye_data.current$GazepointX) - 1)) {
			lines(c(eye_data.current$GazepointX[j],eye_data.current$GazepointX[j+1]), c(eye_data.current$GazepointY[j], eye_data.current$GazepointY[j+1]), col= current_colors_vector[j], lwd=1)
		}

		# DRAW GRADIENT LEGEND
		gradient.rect(gradient.pos[1], gradient.pos[2], gradient.pos[3], gradient.pos[4], col=rev(current_colors_vector) , gradient="y")
		startTime = paste(c(eye_data.current$trialTime[1], " ms"), sep="", collapse="")
		endTime = paste(c(round(tail(eye_data.current$trialTime, 1), 3), " ms"), sep="", collapse="")
		text(gradient.pos[1], gradient.pos[4], startTime, adj=c(0.5,-0.25), col="white") # first trialTime
		text(gradient.pos[1], gradient.pos[2], endTime, adj=c(0.5,1.25), col="white") # last trialTime
	
	} else {
		text(gradient.pos[1], gradient.pos[4], 'No Trace', adj=c(0.5,-0.25), col="white") # first trialTime
	} # end if nrow(eye_data)
	
	# DRAW TRIAL LEGEND
	legend.text <- paste(c(
		"Sure1: ", s1,
		"\nSure2: ", s2,
		"\nMag1: ", m1,
		"\nMag2: ", m2,
		"\ncoinsWon: ", current_coinsWon,			
		"\ncv: ", round(sd(c(m1,m2))/mean(c(m1,m2)), digits=3)
#		"\n\nFound: ", (100*(1 - eye_data.current$proportion_no_eye[1])), "%"
		), sep="", collapse="")
	legend("topleft", legend= legend.text, text.col="gray75", bty="n", inset=c(0.02, 0.02), cex=0.75)

	# LABEL TRIAL TYPE NAME
	text(0, 68, trialTypeLabel, pos=4, col="white")

	# LABEL EPOCH
	text(520, 68, epoch, pos=4, col="white")

	# NOTIFY IF TRIAL IS EXCLUDED
	if (nrow(eye_data.current) > 0) {
		if (eye_data.current$exclude[1] == 1) {
			text(512, 50, "EXCLUDE", col="white")
		}
	} 

} # end function()

	

