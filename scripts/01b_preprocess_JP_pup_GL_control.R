preprocess_control <- function(subjectID) {



#subjectID <- 90

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

setwd(paste(c(study_dir, '/scripts/'   ), collapse=""))
source(paste(c(study_dir, '/scripts/functions/is_bounded.R'   ), collapse=""))
source(paste(c(study_dir, '/scripts/functions/positions.R'   ), collapse=""))
source(paste(c(study_dir, '/scripts/functions/seek_time_line.R'   ), collapse=""))
source(paste(c(study_dir, '/scripts/functions/tally_counts.R'   ), collapse=""))
source(paste(c(study_dir, '/scripts/functions/reject_outliers.R'   ), collapse=""))
source(paste(c(study_dir, '/scripts/functions/eye_data_correction.R'   ), collapse=""))


library(reshape)


outlier_sd <- 3

xdim <- 1280
ydim <- 1024

data_path <- paste(c(study_dir, '/data/raw_data/'   ), collapse="")



# assemble data file name strings
beh_data_file_name <- paste(c(data_path, "risk_reinf_", subjectID, "_test_GL.txt"), sep="", collapse="")
eye_data_file_name <- paste(c(data_path, "Eye_tracking_", subjectID, "_GL.txt"), sep="", collapse="")
event_data_file_name <- paste(c(data_path, "Eye_events_", subjectID, "_GL.txt"), sep="", collapse="")

eye_data_header <- read.table(paste(c(study_dir, "/data/tobii_output_header.txt"), collapse=""), header=T)
# load data

beh_data <- read.table(beh_data_file_name, header=T)
beh_data$subjectID <- subjectID

eye_data <- read.table(eye_data_file_name, header=F)
names(eye_data) <- names(eye_data_header)

event_data <- read.table(event_data_file_name, skip=2, col.names=c("event","time","trial"))

# WHEN SUBJECTS DO NOT RECALIBRATE, RECALIBRATION TIME MARKERS ARE NOT INCLUDED, SO WE NEED TO ADD THEM SEPARATELY HERE AS NA
event_data$event <- as.character(event_data$event)
if (! is.element("Recalibration_on", event_data$event)) {
	event_data <- rbind(event_data, c("Recalibration_on", 0, 1))
	event_data <- rbind(event_data, c("Recalibration_off", 0, 1))
}
# this is needed for seek_time_line to work
event_data$event <- as.factor(event_data$event)
event_data$trial <- as.factor(as.numeric(event_data$trial))
event_data$time <- as.numeric(event_data$time)

event_data.table <- recast(event_data, trial ~ event, id.var=c("event","trial"))



beh_data <- merge(beh_data, event_data.table)

beh_data$outlier <- F
beh_data$outcome <- 0



# set Gazepoints to zero - will be calculated within loop.
eye_data$GazepointX <- 0
eye_data$GazepointY <- 0


# initialize sample count variables
beh_data$mag1_samp.decision <- 0
beh_data$mag2_samp.decision <- 0
beh_data$sure1_samp.decision <- 0
beh_data$sure2_samp.decision <- 0

beh_data$mag1_samp.choice <- 0
beh_data$mag2_samp.choice <- 0
beh_data$sure1_samp.choice <- 0
beh_data$sure2_samp.choice <- 0

beh_data$mag1_samp.outcome <- 0
beh_data$mag2_samp.outcome <- 0
beh_data$sure1_samp.outcome <- 0
beh_data$sure2_samp.outcome <- 0

beh_data$mag1_prop.decision <- 0
beh_data$mag2_prop.decision <- 0
beh_data$sure1_prop.decision <- 0
beh_data$sure2_prop.decision <- 0

beh_data$mag1_prop.choice <- 0
beh_data$mag2_prop.choice <- 0
beh_data$sure1_prop.choice <- 0
beh_data$sure2_prop.choice <- 0

beh_data$mag1_prop.outcome <- 0
beh_data$mag2_prop.outcome <- 0
beh_data$sure1_prop.outcome <- 0
beh_data$sure2_prop.outcome <- 0




beh_data$ffo.decision <- ""
beh_data$lfo.decision <- ""

beh_data$ffo.choice <- ""
beh_data$lfo.choice <- ""

beh_data$ffo.outcome <- ""
beh_data$lfo.outcome <- ""

beh_data$left_rep_decision <- 0
beh_data$left_rep_choice <- 0
beh_data$left_rep_outcome <- 0

beh_data$right_rep_decision <- 0
beh_data$right_rep_choice <- 0
beh_data$right_rep_outcome	<- 0


# JOHN PAYNES ANALYSIS
beh_data$mag1_samp.decision.1 <- 0
beh_data$mag2_samp.decision.1 <- 0
beh_data$sure1_samp.decision.1 <- 0
beh_data$sure2_samp.decision.1 <- 0

beh_data$mag1_samp.decision.2 <- 0
beh_data$mag2_samp.decision.2 <- 0
beh_data$sure1_samp.decision.2 <- 0
beh_data$sure2_samp.decision.2 <- 0

beh_data$mag1_samp.decision.3 <- 0
beh_data$mag2_samp.decision.3 <- 0
beh_data$sure1_samp.decision.3 <- 0
beh_data$sure2_samp.decision.3 <- 0

beh_data$mag1_samp.decision.4 <- 0
beh_data$mag2_samp.decision.4 <- 0
beh_data$sure1_samp.decision.4 <- 0
beh_data$sure2_samp.decision.4 <- 0

beh_data$mag1_samp.decision.5 <- 0
beh_data$mag2_samp.decision.5 <- 0
beh_data$sure1_samp.decision.5 <- 0
beh_data$sure2_samp.decision.5 <- 0
#


eye_data$trial <- 0
eye_data$subjectID <- subjectID
eye_data$trialType <- 0
eye_data$epoch <- ""
eye_data$vert <- -1
eye_data$sideMag <- -1
eye_data$sideSure <- -1
eye_data$gamble <- -1
eye_data$condition <- -1
eye_data$mag1 <- -1
eye_data$mag2 <- -1
eye_data$sure1 <- -1
eye_data$sure2 <- -1
eye_data$coinsWon <- -1
eye_data$ffo <- ''
eye_data$lfo <- ''


eye_data$mag1_fix <- 0
eye_data$mag2_fix <- 0
eye_data$sure1_fix <- 0
eye_data$sure2_fix <- 0


eye_data$Found <- 'None'
eye_data$Found[eye_data$left_valid < 4] = 'Left'
eye_data$Found[eye_data$right_valid < 4] = 'Right'
eye_data$Found[(eye_data$right_valid < 4) & (eye_data$left_valid < 4)] = 'Both'

eye_data$trialTime <- 0
eye_data$exclude <- 0

eye_data$replaced_L <- FALSE
eye_data$replaced_R <- FALSE



outcome_pupil <- data.frame()


for (i in 1:nrow(beh_data)) {
	
	
	#################################################################################
	#################################################################################
	# find onset and offset times
	
	delay_outcome_delta <- beh_data$outcomeTime[i] - beh_data$confirm_delayTime[i]
	outcome_onset <- beh_data$ChoiceOff[i] + delay_outcome_delta
	outcome_offset <- outcome_onset + 2
	current_trial_onset.outcome <- seek_time_line(outcome_onset, eye_data$tet_timeStamp)
	current_trial_offset.outcome <- seek_time_line(outcome_offset, eye_data$tet_timeStamp)



	#################################################################################
	#################################################################################	
	# ADD TRIAL INFORMATION TO EYE DATA
	
	eye_data$trial[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$trial[i]
	eye_data$trialType[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$trialType[i]
	eye_data$gamble[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$gamble[i]
	eye_data$condition[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$condition[i]
	eye_data$sure1[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$sure1[i]
	eye_data$sure2[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$sure2[i]
	eye_data$mag1[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$mag1[i]
	eye_data$mag2[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$mag2[i]
	eye_data$vert[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$vert[i]
	eye_data$sideSure[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$sideSure[i]
	eye_data$sideMag[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$sideMag[i]
	eye_data$coinsWon[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$coinsWon[i]

			# OUTCOME PHASE
	eye_data$epoch[current_trial_onset.outcome:current_trial_offset.outcome] <- "outcome"
	eye_data$trialTime[current_trial_onset.outcome:current_trial_offset.outcome] <- 
		round(eye_data$tet_timeStamp[current_trial_onset.outcome:current_trial_offset.outcome] - eye_data$tet_timeStamp[current_trial_onset.outcome], digits=4)

	
	#################################################################################
	#################################################################################		
	# INTERPOLATE MISSING DATA POINTS
	corrected_data <- eye_data_correction(eye_data, beh_data$trial[i])
	

	# CALCULATE NEW GAZEPOINTS BASED ON CORRECTED DATA: scale * ((T or F)*fixation + (T or F)*fixation)) / max(1 or 2) 
	corrected_data$GazepointX <- xdim * ( (corrected_data$left_x != -1)*corrected_data$left_x  + (corrected_data$right_x != -1)*corrected_data$right_x) /
		apply( data.frame( ( corrected_data$left_x != -1 ) + ( corrected_data$right_x != -1 ) ), 1, function(x) { max(x,1)})
	corrected_data$GazepointY <- ydim * ( (corrected_data$left_y != -1)*corrected_data$left_y  + (corrected_data$right_y != -1)*corrected_data$right_y) /
		apply( data.frame( ( corrected_data$left_y != -1 ) + ( corrected_data$right_y != -1 ) ), 1, function(x) { max(x,1)})
	
	
	# PLOTTING OF MISSING AND INTERPOLATED DATA SHOULD GO HERE, WHILE BOTH OLD AND NEW GAZEPOINTS ARE IN MEMORY
	# par(mfrow=c(2,1))
	# plot(1:length(corrected_data$GazepointX), eye_data$GazepointX[eye_data$trial==i],type='l', ylim=c(0,1200),lty='dotted', main="gazepoint X")
	# lines(1:length(corrected_data$GazepointX), corrected_data$GazepointX,col='green')
	# lines(1:length(corrected_data$GazepointX), 100*(corrected_data$epoch == "decision"), col="blue", lwd=2)
	# lines(1:length(corrected_data$GazepointX), 100*(corrected_data$epoch == "choice"), col="orange", lwd=2)
	# lines(1:length(corrected_data$GazepointX), 100*(corrected_data$epoch == "outcome"), col="red", lwd=2)

	# plot(1:length(corrected_data$GazepointY), eye_data$GazepointY[eye_data$trial==i],type='l', ylim=c(0,800),lty='dotted', main="gazepoint Y")
	# lines(1:length(corrected_data$GazepointY), corrected_data$GazepointY,col='green')
	# lines(1:length(corrected_data$GazepointY), 100*(corrected_data$epoch == "decision"), col="blue", lwd=2)
	# lines(1:length(corrected_data$GazepointY), 100*(corrected_data$epoch == "choice"), col="orange", lwd=2)
	# lines(1:length(corrected_data$GazepointY), 100*(corrected_data$epoch == "outcome"), col="red", lwd=2)	
	
	
	# reassign corrected data to eye_data
	eye_data[current_trial_onset.outcome:current_trial_offset.outcome,] <- corrected_data	
	
	# MISSING DATA POINTS SUMMARY
	beh_data$left_rep_outcome[i] <- round(sum(eye_data$replaced_L[current_trial_onset.outcome:current_trial_offset.outcome]) / 
		length(eye_data$replaced_L[current_trial_onset.outcome:current_trial_offset.outcome]), digits = 3)

	beh_data$right_rep_outcome[i]	<- round(sum(eye_data$replaced_R[current_trial_onset.outcome:current_trial_offset.outcome]) / 
		length(eye_data$replaced_R[current_trial_onset.outcome:current_trial_offset.outcome]), digits = 3)
	
	beh_data$good_outcome[i] <- sum(corrected_data$GazepointY[corrected_data$epoch == "outcome"] != 0) / length(corrected_data$GazepointY[corrected_data$epoch == "outcome"] != 0)
	
	
	#################################################################################
	#################################################################################
	# COUNT FIXATIONS FOR EACH PHASE BY TRIAL TYPE
	
	if (beh_data$trialType[i] == 1) {
		
		current_sure1_pos <- current_frame_positions(beh_data$trialType[i], beh_data$vert[i], beh_data$sideMag[i], beh_data$sideSure[i])[[1]]
		current_sure2_pos <- current_frame_positions(beh_data$trialType[i], beh_data$vert[i], beh_data$sideMag[i], beh_data$sideSure[i])[[2]]
		
		eye_data$sure1_fix[current_trial_onset.outcome:current_trial_offset.outcome] <- 
			is_bounded(eye_data$GazepointX[current_trial_onset.outcome:current_trial_offset.outcome],
				eye_data$GazepointY[current_trial_onset.outcome:current_trial_offset.outcome],
				current_sure1_pos[[1]], current_sure1_pos[[2]])
		
		eye_data$sure2_fix[current_trial_onset.outcome:current_trial_offset.outcome] <- 
			is_bounded(eye_data$GazepointX[current_trial_onset.outcome:current_trial_offset.outcome],
				eye_data$GazepointY[current_trial_onset.outcome:current_trial_offset.outcome],
				current_sure2_pos[[1]], current_sure2_pos[[2]])

		# DEFINE OUTCOMES
		if (beh_data$coinsWon[i] == beh_data$sure1[i]) {
			beh_data$outcome[i] <- 1
		} else { beh_data$outcome[i] <- 0 }


	} else if (beh_data$trialType[i] > 1 & beh_data$trialType[i] < 6) {

		current_mag1_pos <- current_frame_positions(beh_data$trialType[i], beh_data$vert[i], beh_data$sideMag[i], beh_data$sideSure[i])[[1]]
		current_mag2_pos <- current_frame_positions(beh_data$trialType[i], beh_data$vert[i], beh_data$sideMag[i], beh_data$sideSure[i])[[2]]
		current_sure2_pos <- current_frame_positions(beh_data$trialType[i], beh_data$vert[i], beh_data$sideMag[i], beh_data$sideSure[i])[[3]]



		eye_data$mag1_fix[current_trial_onset.outcome:current_trial_offset.outcome] <- 
			is_bounded(eye_data$GazepointX[current_trial_onset.outcome:current_trial_offset.outcome],
				eye_data$GazepointY[current_trial_onset.outcome:current_trial_offset.outcome],
				current_mag1_pos[[1]], current_mag1_pos[[2]])
		
		eye_data$mag2_fix[current_trial_onset.outcome:current_trial_offset.outcome] <- 
			is_bounded(eye_data$GazepointX[current_trial_onset.outcome:current_trial_offset.outcome],
				eye_data$GazepointY[current_trial_onset.outcome:current_trial_offset.outcome],
				current_mag2_pos[[1]], current_mag2_pos[[2]])		
				
				
				
		eye_data$sure2_fix[current_trial_onset.outcome:current_trial_offset.outcome] <- 
			is_bounded(eye_data$GazepointX[current_trial_onset.outcome:current_trial_offset.outcome],
				eye_data$GazepointY[current_trial_onset.outcome:current_trial_offset.outcome],
				current_sure2_pos[[1]], current_sure2_pos[[2]])


		# DEFINE OUTCOMES
		if (beh_data$coinsWon[i] == beh_data$mag1[i]) {
			beh_data$outcome[i] <- 2
		} else if (beh_data$coinsWon[i] == beh_data$sure2[i] ){ 
			beh_data$outcome[i] <- 1 
		} else { beh_data$outcome[i] <- 0 }

	
	} 
		
	
	fix_cols <- c("mag1_fix", "mag2_fix", "sure1_fix", "sure2_fix")
	fix_col_name <- c("mag1", "mag2", "sure1", "sure2")

	#eye_data[current_trial_onset.outcome:current_trial_offset.outcome, fix_cols]

	#################################################################################
	#################################################################################
	# 	SAMPLES	
	#	DECISION EPOCH	

	beh_data$mag1_samp.outcome[i] <- sum(eye_data$mag1_fix[current_trial_onset.outcome:current_trial_offset.outcome])
	beh_data$mag2_samp.outcome[i] <- sum(eye_data$mag2_fix[current_trial_onset.outcome:current_trial_offset.outcome])
	beh_data$sure1_samp.outcome[i] <- sum(eye_data$sure1_fix[current_trial_onset.outcome:current_trial_offset.outcome])
	beh_data$sure2_samp.outcome[i] <- sum(eye_data$sure2_fix[current_trial_onset.outcome:current_trial_offset.outcome])


	#	PROPORTIONS
	#	DECISION EPOCH
	
	if (beh_data$trialType[i] == 1) {

		beh_data$sure1_prop.outcome[i] <- beh_data$sure1_samp.outcome[i] / (beh_data$sure1_samp.outcome[i] + beh_data$sure2_samp.outcome[i])
		beh_data$sure2_prop.outcome[i] <- 1 - beh_data$sure1_prop.outcome[i]
		
		
	} else {
		

		outcome_samp <- (beh_data$mag1_samp.outcome[i] + beh_data$mag2_samp.outcome[i] + beh_data$sure1_samp.outcome[i] + beh_data$sure2_samp.outcome[i])
		beh_data$mag1_prop.outcome[i] <- beh_data$mag1_samp.outcome[i] / outcome_samp
		beh_data$mag2_prop.outcome[i] <- beh_data$mag2_samp.outcome[i] / outcome_samp
		beh_data$sure2_prop.outcome[i] <- beh_data$sure2_samp.outcome[i] / outcome_samp	
		
	}
		
		

	
	#################################################################################
	#################################################################################
	# identify first and last fixation objects
	

	for (j in current_trial_onset.outcome:current_trial_offset.outcome) {
			# loop through eye_data fixation colums until the first fixation appears, then identify and stop looping
			if (sum(eye_data[j, fix_cols]) > 0 ) {
				ind <- which(eye_data[j, fix_cols] == 1)
				beh_data$ffo.outcome[i] <- fix_col_name[ind]
				break
			}
	}
	for (j in current_trial_offset.outcome:current_trial_onset.outcome) {
			# loop through eye_data fixation colums until the first fixation appears, then identify and stop looping
			if (sum(eye_data[j, fix_cols]) > 0 ) {
				ind <- which(eye_data[j, fix_cols] == 1)
				beh_data$lfo.outcome[i] <- fix_col_name[ind]
				break
			}
	}



	#################################################################################
	#################################################################################
	# ADD LFO AND FFO TO EYE DATA

	eye_data$ffo[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$ffo.outcome[i]
	eye_data$lfo[current_trial_onset.outcome:current_trial_offset.outcome] <- beh_data$lfo.outcome[i]


	
	
	#################################################################################
	#################################################################################
	# ADD PUPILOMETRY DATA TO PUPILOMETRY ARRAYS
	
	tempL <- data.frame(subjectID, beh_data[i,1:8], beh_data$outcome[i], beh_data$condition[i], "L", t(eye_data$left_pup_diam[current_trial_offset.choice:(current_trial_offset.choice+120)]))
	names(tempL) <- c("subjectID", names(beh_data)[1:8], "outcome", "condition", "eye", 1:121)
	tempR <- data.frame(subjectID, beh_data[i,1:8], beh_data$outcome[i], beh_data$condition[i], "R", t(eye_data$right_pup_diam[current_trial_offset.choice:(current_trial_offset.choice+120)]))
	names(tempR) <- c("subjectID", names(beh_data)[1:8], "outcome", "condition", "eye", 1:121)
	delay_pupil <- rbind(delay_pupil, tempL, tempR)
	
	
	tempL <- data.frame(subjectID, beh_data[i,1:8], beh_data$outcome[i], beh_data$condition[i], "L", t(eye_data$left_pup_diam[current_trial_onset.outcome:(current_trial_onset.outcome +240)]))
	names(tempL) <- c("subjectID", names(beh_data)[1:8], "outcome", "condition", "eye", 1:241)
	tempR <- data.frame(subjectID, beh_data[i,1:8], beh_data$outcome[i], beh_data$condition[i], "R", t(eye_data$right_pup_diam[current_trial_onset.outcome:(current_trial_onset.outcome +240)]))
	names(tempR) <- c("subjectID", names(beh_data)[1:8], "outcome", "condition", "eye", 1:241)
	outcome_pupil <- rbind(outcome_pupil, tempL, tempR)
	
	
	

}


indices <- which(outcome_pupil == -1)
is.na(outcome_pupil)[indices] <- TRUE


eye_data <- eye_data[eye_data$epoch != "",]







# SAVING DATA

beh_data_output_file_name <- paste(c(study_dir, "/data/processed_data/beh_data_", subjectID, ".R"), collapse="")
eye_data_output_file_name <- paste(c(study_dir, "/data/processed_data/eye_data_", subjectID, ".R"), collapse="")
beh_data_table_file_name <- paste(c(study_dir, "/data/processed_data/beh_data_", subjectID, ".txt"), collapse="")


outcome_pupil_output_file_name <- paste(c(study_dir, "/data/processed_data/pupil_data_", subjectID, "_outcome.R"), collapse="")


unlink(beh_data_output_file_name) # remove old data
unlink(eye_data_output_file_name)
unlink(beh_data_table_file_name)

unlink(outcome_pupil_output_file_name)


write.table(beh_data, file = beh_data_table_file_name, quote=F,sep="\t", row.names=F)
save(beh_data, file= beh_data_output_file_name)
save(eye_data, file= eye_data_output_file_name)


save(outcome_pupil, file= outcome_pupil_output_file_name)



processed <- list(beh_data,eye_data)

}





