

library(arm)
library(mlogit)
library(MASS)
library(reshape)
library(ggplot2)
library(lme4)


study_dir <- c("/Users/Shared/EyeTrackingGaze", 
	"/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze2")

	
if (file.exists(study_dir[1])) { 
	study_dir <- study_dir[1] 
} else if (file.exists(study_dir[2])) { 
	study_dir <- study_dir[2]
} else {
	return('study directory not found')
}




nsubs <- function(data_frame) {length(levels(as.factor(data_frame$subjectID)))} # function to find number of subjects


           

source("./subject_lists.R")
#source("./participant_ages.R") # participant_ages 



cmbd_riskData <- data.frame()
# cmbd_outcome_pupil <- list()
	# cmbd_outcome_pupil[[1]] <- data.frame() # raw
	# cmbd_outcome_pupil[[2]] <- data.frame() # normalized
	# cmbd_outcome_pupil[[3]] <- data.frame() # ncounts != NA
# cmbd_outcome_pupil_trial_count <- data.frame()
# cmbd_outcome_pupil.norm <- data.frame()

# cmbd_iti_pupil <- data.frame()
# cmbd_decision_pupil <- data.frame()
# cmbd_delay_pupil <- data.frame()


processed_data_dir <- "/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze/data/processed_data/"

for (subj in all_subjects) {
	#preprocess(subj)
 	fileName <- paste(c(processed_data_dir, "beh_data_", subj, ".R"), collapse="" )
 	load(fileName)
 	cmbd_riskData <- rbind(cmbd_riskData, beh_data)
 	
 }

# outcome def
cmbd_riskData$outcome <- 0
cmbd_riskData$outcome[cmbd_riskData$coinsWon == cmbd_riskData$mag1] <- 2
cmbd_riskData$outcome[cmbd_riskData$coinsWon == cmbd_riskData$sure2] <- 1
cmbd_riskData$outcome <- as.factor(cmbd_riskData$outcome)
cmbd_riskData$gamble <- as.factor(cmbd_riskData$gamble)

# Risk level
cmbd_riskData$cv1 <- apply(cmbd_riskData[,c("mag1","mag2")], 1, sd) / apply(cmbd_riskData[,c("mag1","mag2")], 1, mean)
cmbd_riskData$cv2 <- apply(cmbd_riskData[,c("sure1","sure2")], 1, sd) / apply(cmbd_riskData[,c("sure1","sure2")], 1, mean)

# Age group
cmbd_riskData$AgeGroup <- "adult"
cmbd_riskData$AgeGroup[cmbd_riskData$subjectID > 100] <- "child"
cmbd_riskData$AgeGroup <- as.factor(cmbd_riskData$AgeGroup)
cmbd_riskData$condition <- as.factor(cmbd_riskData$condition)

cmbd_riskData$ffo.decision <- as.factor(cmbd_riskData$ffo.decision)
cmbd_riskData$lfo.decision <- as.factor(cmbd_riskData$lfo.decision)
cmbd_riskData$ffo.choice <- as.factor(cmbd_riskData$ffo.choice)
cmbd_riskData$lfo.choice <- as.factor(cmbd_riskData$lfo.choice)
cmbd_riskData$ffo.outcome <- as.factor(cmbd_riskData$ffo.outcome)
cmbd_riskData$lfo.outcome <- as.factor(cmbd_riskData$lfo.outcome)

cmbd_riskData$good_decision[is.na(cmbd_riskData$good_decision)] <- 0

cmbd_riskData_file_name <- paste(c(study_dir, "/data/processed_data/beh_data_cmbd_riskData.R"), collapse="")
 unlink(cmbd_riskData_file_name)
 save(cmbd_riskData, file= cmbd_riskData_file_name)

load(cmbd_riskData_file_name)






