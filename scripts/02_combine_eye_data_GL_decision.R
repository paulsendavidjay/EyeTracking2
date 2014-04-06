# COMBINE DECISION DATA

library(arm)
library(mlogit)
library(MASS)
library(reshape)
library(ggplot2)
library(lme4)
source("./subject_lists.R")

# SPECIFY STUDY DIRECTORY
study_dir <- c("/Users/Shared/EyeTrackingGaze", 
	"/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze")
if (file.exists(study_dir[1])) { 
	study_dir <- study_dir[1] 
} else if (file.exists(study_dir[2])) { 
	study_dir <- study_dir[2]
} else {
	return('study directory not found')
}


# INITIALIZE DATA FRAMES
cmbd_riskData <- data.frame()
cmbd_outcome_pupil <- list()
cmbd_outcome_pupil_trial_count <- data.frame()
cmbd_outcome_pupil.norm <- data.frame()


# READ SUBJECT DATA ONE AT A TIME INTO DATA FRAMES
processed_data_dir <- "/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze/data/processed_data/"
for (subj in all_subjects) {
	
	#preprocess(subj)
 	fileName <- paste(c(processed_data_dir, "beh_data_", subj, ".R"), collapse="" )
 	load(fileName)

 	fileName <- paste(c(processed_data_dir, "eye_data_", subj, ".R"), collapse="" )
 	load(fileName)

 	fileName <- paste(c(processed_data_dir, "pupil_data_", subj, "outcome.R"), collapse="" )
 	load(fileName)

	eye_data$left_pup_diam[eye_data$left_pup_diam == -1] <- NaN # replace Inf with NaN
	eye_data$right_pup_diam[eye_data$right_pup_diam == -1] <- NaN # replace Inf with NaN
 	
	for (trialNum in 1:nrow(beh_data)) {
		print(trialNum)
	} 	
 	
 	# REPLACE BAD VALUES
	temp <- outcome_pupil[,13:253]
	temp[temp < -15 | temp > 15] <- NaN # replace Inf with NaN
	outcome_pupil[,13:253] <- temp
	
 	# FILL IN MISSING DATA POINTS
	for (i in 1:nrow(outcome_pupil)) {
		outcome_pupil[i,is.na(outcome_pupil[i,])] <- NA 
		# make sure there are NAs instead of 0's, and that they are not ALL NAs to avoid errors
		if ( sum(is.na(outcome_pupil[i,13:253])) > 0 & sum(is.na(outcome_pupil[i,13:253])) < 240) { 
			# the na.approx function operates on columns rather than rows, thus we need to transpose before and after use of na.approx		
			#outcome_pupil[i,13:253] <- t(na.spline(t(outcome_pupil[i,13:253]), na.rm=FALSE))
			outcome_pupil[i,13:253] <- t(na.approx(t(outcome_pupil[i,13:253]), maxgap=10, na.rm=FALSE))
		} 
	}
 	cmbd_outcome_pupil[[1]] <- rbind(cmbd_outcome_pupil[[1]], outcome_pupil) # add to list

	# GENERATE NORMALIZED PUPIL DIAMETERS cmbd_outcome_pupil[[2]]
	temp.norm <- outcome_pupil
	pup.mean <- mean(as.numeric(as.matrix(temp.norm[,13:253])), na.rm = T) # calculate mean pupil dilation
	pup.sd <- sd(as.numeric(as.matrix(temp.norm[,13:253])), na.rm = T) # calculate stand. dev. pupil dilation
	temp.norm[,13:253] <- (temp.norm[,13:253] - pup.mean) / pup.sd  # normalize all pupil dilations
 	cmbd_outcome_pupil[[2]] <- rbind(cmbd_outcome_pupil[[2]], temp.norm) # add to list

	#	GENERAL TABLE OF TRIAL COUNTS
	outcome_pupil.nacount <- aggregate(outcome_pupil[,13:253],
		by=list(outcome_pupil$eye, outcome_pupil$outcome, outcome_pupil$condition), function(x) { sum(is.na(x))})
	outcome_pupil.count <- aggregate(outcome_pupil[,13:253], 
		by=list(outcome_pupil$eye, outcome_pupil$outcome, outcome_pupil$condition), length)
	outcome_pupil.trialCount <- outcome_pupil.count[,4:ncol(outcome_pupil.count)] - outcome_pupil.nacount[,4:ncol(outcome_pupil.nacount)]
	outcome_pupil.trialCount <- cbind(outcome_pupil$subjectID[1], outcome_pupil.count[,1:3], outcome_pupil.trialCount)
	names(outcome_pupil.trialCount)[1:4] <- c("subjectID","eye", "outcome", "condition")
	cmbd_outcome_pupil[[3]] <- rbind(cmbd_outcome_pupil[[3]], outcome_pupil.trialCount) # add to list
 	
 }


# CHANGE NUMERIC TO FACTORS, LABEL GROUPS, CALCULATE CV
cmbd_riskData$gamble <- as.factor(cmbd_riskData$gamble)

# RISK LEVEL
cmbd_riskData$cv1 <- apply(cmbd_riskData[,c("mag1","mag2")], 1, sd) / apply(cmbd_riskData[,c("mag1","mag2")], 1, mean)
cmbd_riskData$cv2 <- apply(cmbd_riskData[,c("sure1","sure2")], 1, sd) / apply(cmbd_riskData[,c("sure1","sure2")], 1, mean)

# AGE GROUP
cmbd_riskData$AgeGroup <- "adult"
cmbd_riskData$AgeGroup[cmbd_riskData$subjectID > 100] <- "child"
cmbd_riskData$AgeGroup <- as.factor(cmbd_riskData$AgeGroup)
cmbd_riskData$condition <- as.factor(cmbd_riskData$condition)

# ADD AGE GROUP TO PUPIL DATA
for (i in 1:3) { 
	cmbd_outcome_pupil[[i]][,'AgeGroup'] <- "adult"
	cmbd_outcome_pupil[[i]][(cmbd_outcome_pupil[[i]][,"subjectID"] > 100),'AgeGroup'] <- "child"
	cmbd_outcome_pupil[[i]][,"AgeGroup"] <- as.factor(cmbd_outcome_pupil[[i]][,"AgeGroup"])
	cmbd_outcome_pupil[[i]][,"condition"] <- as.factor(cmbd_outcome_pupil[[i]][,"condition"])
}

# MAKE SURE FACTORS ARE SET
cmbd_riskData$ffo.decision <- as.factor(cmbd_riskData$ffo.decision)
cmbd_riskData$lfo.decision <- as.factor(cmbd_riskData$lfo.decision)
cmbd_riskData$ffo.choice <- as.factor(cmbd_riskData$ffo.choice)
cmbd_riskData$lfo.choice <- as.factor(cmbd_riskData$lfo.choice)
cmbd_riskData$ffo.outcome <- as.factor(cmbd_riskData$ffo.outcome)
cmbd_riskData$lfo.outcome <- as.factor(cmbd_riskData$lfo.outcome)

# REPLACE NA WITH 0
cmbd_riskData$good_decision[is.na(cmbd_riskData$good_decision)] <- 0

# DELETE OLD, SAVE NEW DATAFRAMES
cmbd_outcome_pupil_file_name <- paste(c(study_dir, "/data/processed_data/pupil_data_cmbd_outcome_pupil.R"), collapse="")
unlink(cmbd_outcome_pupil_file_name)
save(cmbd_outcome_pupil, file= cmbd_outcome_pupil_file_name)

cmbd_riskData_file_name <- paste(c(study_dir, "/data/processed_data/beh_data_cmbd_riskData.R"), collapse="")
unlink(cmbd_riskData_file_name)
save(cmbd_riskData, file= cmbd_riskData_file_name)


