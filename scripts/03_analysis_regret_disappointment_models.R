
#mcmc_samp2 <- mcmcsamp(lm2, n = 10001)
#	GIVES THE PERCENTAGE OF MCMC ESTIMATES GREATER THAN 0
#pval2 <- round(rowMeans(mcmc_samp2@fixef > 0), 4)
# adjusts pval to be smallest
#pval2 <- abs(abs(pval2 - 0.5) - 0.5)
#cbind(data.frame(lm2@fixef), data.frame(pval2))

library(RColorBrewer)
library(zoo)
library(arm)
library(MASS)
library(reshape)
library(ggplot2)
library(lme4)
library(RColorBrewer)
suppressMessages(library(doMC)) # for parallel processing





# INCLUDING PRIOR OUTCOME CHANGES THE OUTCOME OF RESULTS NEGLIGIBLY FOR LOSS DOMAIN
rm(list=ls())
number_of_processors_to_use <- 7
setwd("/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze2/scripts")
study_dir <- "/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze/"

source("~/R-stuff/functions/summarySE.R")

load("subset_riskData.outcome")

missing_data_thresh <- 0.75



cmbd_riskData_file_name <- paste(c(study_dir, "/data/processed_data/beh_data_cmbd_riskData.R"), collapse="")
#save(cmbd_riskData, file=cmbd_riskData_file_name)
load(cmbd_riskData_file_name) 




cmbd_riskData <- cmbd_riskData[,c("subjectID","trial","trialType","AgeGroup","mag1","mag2", "sure1","sure2", "cv1","gamble","outcome", 
	"mag1_prop.outcome", "mag2_prop.outcome", "sure1_prop.outcome", "sure2_prop.outcome", "good_outcome", "ffo.decision", "lfo.decision", 
	"mag1_prop.decision", "mag2_prop.decision", "sure1_prop.decision", "sure2_prop.decision", "RT","condition")]



# SEPARATE GAIN FROM LOSS DATA, AND REMOVE SURE BET TRIALS
cmbd_riskData_regret_Gain <- subset(cmbd_riskData, condition == 1)
cmbd_riskData_regret_Gain <- subset(cmbd_riskData_regret_Gain, subjectID %in% unique(subset_riskData.outcomeGain$subjectID))
cmbd_riskData_regret_Gain <- subset(cmbd_riskData_regret_Gain, trialType != 1) # & good_outcome >= missing_data_thresh


cmbd_riskData_regret_Loss <- subset(cmbd_riskData, condition == 0)
cmbd_riskData_regret_Loss <- subset(cmbd_riskData_regret_Loss, subjectID %in% unique(subset_riskData.outcomeLoss$subjectID))
cmbd_riskData_regret_Loss <- subset(cmbd_riskData_regret_Loss, trialType != 1) #  & good_outcome >= missing_data_thresh


cmbd_riskData_regret_Gain[is.na(cmbd_riskData_regret_Gain)] <- 0
cmbd_riskData_regret_Loss[is.na(cmbd_riskData_regret_Loss)] <- 0

summarySE(cmbd_riskData_regret_Gain, measurevar="condition", groupvars="AgeGroup")
  # AgeGroup   N condition sd se ci
# 1    adult 622        NA  0  0  0
# 2    child 506        NA  0  0  0
# 15 adults: 622/15 # 471.4 trials per
# 13 children 506/13 # 38.9 trials per (min is 28 trials)

# SORT
# 
# SORTING BY TRIAL ALSO TAKES PLACE DURING LOOPING AT THE SUBJECT LEVEL
cmbd_riskData_regret_Gain <- cmbd_riskData_regret_Gain[order(cmbd_riskData_regret_Gain$subjectID, cmbd_riskData_regret_Gain$trial),]
cmbd_riskData_regret_Loss <- cmbd_riskData_regret_Loss[order(cmbd_riskData_regret_Loss$subjectID, cmbd_riskData_regret_Loss$trial),]




source("functions/emot/emot_decay_Gain_model1_rr.R")
source("functions/emot/emot_decay_Loss_model1_rr.R")
source("functions/emot/emot_decay_Gain_model2_rrrr.R")
source("functions/emot/emot_decay_Loss_model2_rrrr.R")
source("functions/emot/emot_decay_Gain_model3_rrrrde.R")
source("functions/emot/emot_decay_Loss_model3_rrrrde.R")


for (model in c(1, 2, 3)) {
for (domain in c("Gain","Loss")) {
for	(decay_type in c(0, 1, 2, 3)) {

	if (model == 1 & decay_type == 3) {
		next
	}
	
	if (domain == "Gain") {
		current_data_set <- cmbd_riskData_regret_Gain
		
		if (model == 1) {
			current_model <- emot_decay_Gain_model1_rr
		} else if (model == 2) {
			current_model <- emot_decay_Gain_model2_rrrr
		} else if (model == 3) {
			current_model <- emot_decay_Gain_model3_rrrrde
		}
	
	} else if (domain == "Loss"){
	
		current_data_set <- cmbd_riskData_regret_Loss
	
		if (model == 1) {
			current_model <- emot_decay_Loss_model1_rr
		} else if (model == 2) {
			current_model <- emot_decay_Loss_model2_rrrr
		} else if (model == 3) {
			current_model <- emot_decay_Loss_model3_rrrrde
		}
		
	}
	
	current_model_df <- data.frame()
	for (sub in unique(current_data_set$subjectID)) {
	
		current_data <- subset(current_data_set, subjectID == sub)
		#current_data <- current_data[,c("subjectID","trial","AgeGroup","mag1","mag2", "sure1","sure2", "cv1","gamble","outcome", "mag1_prop.outcome", "mag2_prop.outcome", "sure2_prop.outcome")]
		
		
		# GET MEANS FOR REPLACEMENT (IMPUTATION)
		lose_means <- colMeans(subset(current_data, good_outcome > missing_data_thresh & gamble != 99 & outcome == 0)[,c("mag1_prop.outcome", "mag2_prop.outcome", "sure1_prop.outcome", "sure2_prop.outcome")])
		safe_means <- colMeans(subset(current_data, good_outcome > missing_data_thresh & gamble != 99 & outcome == 1)[,c("mag1_prop.outcome", "mag2_prop.outcome", "sure1_prop.outcome", "sure2_prop.outcome")])
		win_means <- colMeans(subset(current_data, good_outcome > missing_data_thresh & gamble != 99 & outcome == 2)[,c("mag1_prop.outcome", "mag2_prop.outcome", "sure1_prop.outcome", "sure2_prop.outcome")])
	
		current_data[current_data$good_outcome < missing_data_thresh & current_data$outcome == 0, 
			c("mag1_prop.outcome", "mag2_prop.outcome", "sure1_prop.outcome", "sure2_prop.outcome")] <- lose_means
		current_data[current_data$good_outcome < missing_data_thresh & current_data$outcome == 1, 
			c("mag1_prop.outcome", "mag2_prop.outcome", "sure1_prop.outcome", "sure2_prop.outcome")] <- safe_means
		current_data[current_data$good_outcome < missing_data_thresh & current_data$outcome == 2, 
			c("mag1_prop.outcome", "mag2_prop.outcome", "sure1_prop.outcome", "sure2_prop.outcome")] <- win_means
	
		current_data <- current_data[order(current_data$trial),]
	
	
		
	
		starting_param <- c(0.5, 0.5, 0.5, 0.5)
	
		# SUBJECT SPECIFIC STARTING PARAMETERS
		if (decay_type == 0) {
	
		} else 	if (decay_type == 1) {
			
			if (model==1) { if (domain=="Gain") { if (sub==62) {starting_param=0.3}}}
		
			if (model==2) { if (domain=="Loss") { if (sub==518) {starting_param=0.7
												} else if (sub==521) {starting_param=1
												} else if (sub==534) {starting_param=1
												}}}

			if (model==2) { if (domain=="Gain") { if (sub==61) {starting_param=0.9}}} # lower starting values approx. 0, but do not converge, high starting value converges at 1, slightly higher LL

			if (model==3) { if (domain=="Gain") { if (sub==61) {starting_param=0.9
												} else if (sub==65) {starting_param=0.9
												}}}

			if (model==3) { if (domain=="Loss") { if (sub==509) {starting_param=0.98 # converges @ 1
												} else if (sub==518) {starting_param=0.8 # converges @ 1
												} else if (sub==526) {starting_param=0.2 
												}}}
		
		} else	if (decay_type == 2) {
		
			if (model==1) { if (domain=="Loss") { if (sub==518) {starting_param=c(0.7,0.7)}}}
			
			if (model==2) { if (domain=="Gain") { if (sub==61) {starting_param=c(0, 1) # does not converge
												} else if (sub==509) {starting_param=c(0.2, 0.8)
												} else if (sub==530) {starting_param=c(0.8, 0.3)
												}}}

			if (model==2) { if (domain=="Loss") { if (sub==512) {starting_param=c(0.8, 0.2) 
												} else if (sub==526) {starting_param=c(0.1, 0.14)
												} else if (sub==534) {starting_param=c(0.8, 0.3)
												}}}

			if (model==3) { if (domain=="Gain") { if (sub==54) {starting_param=c(0.8, 0.8) 
												} else if (sub==61) {starting_param=c(0.96, 0.9)
												} else if (sub==63) {starting_param=c(0.9, 0.9) # converges @ c(1,1)
												} else if (sub==525) {starting_param=c(0.05, 0.9) # does not converge
												} else if (sub==527) {starting_param=c(0.9,0.9)
												} else if (sub==530) {starting_param=c(0.9,0.9)
												}}}

			if (model==3) { if (domain=="Loss") { if (sub==509) {starting_param=c(0.9, 0.2)
												} else if (sub==512) {starting_param=c(0.8, 0.8)
												} else if (sub==518) {starting_param=c(0.8, 0.8)
												} else if (sub==527) {starting_param=c(0.05, 0.05)
												}}}

		
		} else	if (decay_type == 3) {

			if (model==2) { if (domain=="Gain") { if (sub==54) {starting_param=c(0.8, 0.5, 0.2, 0.9) #13.77
												} else if (sub==61) {starting_param=c(0.8, 0.6, 0.4, 0.3) # does not converge
												} else if (sub==66) {starting_param=c(0.2, 1, 0.8, 0.4) # second start value does not adjust, model does not converge
												} else if (sub==509) {starting_param=c(0.2, 0.7, 0.2, 0.8) # fourth start value does not adjust, model does not converge
												} else if (sub==516) {starting_param=c(0.2, 0.7, 0.2, 0.8)
												} else if (sub==525) {starting_param=c(0.8, 0.7, 0.1, 0.8) # does not converge
												} else if (sub==530) {starting_param=c(0.8, 0.7, 0.5, 0.8)
												}}}

			if (model==2) { if (domain=="Loss") { if (sub==509) {starting_param=c(0.8, 0.8, 0.8, 0.9)
												} else if (sub==512) {starting_param=c(0.3,0.7,0.3,0.1) # does not converge
												} else if (sub==521) {starting_param=c(0.3,0.7,0.6,0.6)	
												} else if (sub==527) {starting_param=c(0.3, 0.4, 0.4, 0.4)
												} else if (sub==534) {starting_param=c(0.8, 0.3, 0.7, 0.8)
												}}}

			if (model==3) { if (domain=="Gain") { if (sub==54) {starting_param=c(0.7, 0.8, 0.9, 0.8)
												} else if (sub==61) {starting_param=c(0.7, 0.8, 0.7, 0.7)
												} else if (sub==63) {starting_param=c(0.9, 0.7, 0.5, 0.9)
												} else if (sub==66) {starting_param=c(0.05, 1, 0.4, 0.3) # does not converge
												} else if (sub==509) {starting_param=c(0.8, 0.8, 0.2, 0.5) # does not converge
												} else if (sub==516) {starting_param=c(0.2, 0.9, 0.1, 0.6)
												} else if (sub==525) {starting_param=c(0.9, 0.75, 0.9, 0.7)
												} else if (sub==527) {starting_param=c(0.8, 0.7, 0.8, 0.8)
												} else if (sub==530) {starting_param=c(0.8, 0.8, 0.8, 0.8)
												}}}

			if (model==3) { if (domain=="Loss") { if (sub==67) {starting_param=c(0.7, 0.8, 0.9, 0.8)
												} else if (sub==509) {starting_param=c(0.3, 0.4, 0.2, 0.2) # does not converge
												} else if (sub==512) {starting_param=c(0.7, 0.7, 0.7, 0.7) # does not converge
												} else if (sub==518) {starting_param=c(0.7, 0.7, 0.7, 0.7)
												} else if (sub==526) {starting_param=c(0.8, 0.95, 0.6, 0.5)
												} else if (sub==527) {starting_param=c(0.9, 0.7, 0.8, 0.5)
												} else if (sub==534) {starting_param=c(0.7, 0.8, 0.7, 0.7)
												}}}

	
		}
	
		# OPTIMIZATION ROUTINE
		optim_output = optim(starting_param, current_model, 
			trial = current_data$trial, 
			choice = current_data$gamble, 
			outcome= current_data$outcome, 
			mag1.prop= current_data$mag1_prop.outcome, 
			mag2.prop= current_data$mag2_prop.outcome, 
			sure2.prop= current_data$sure2_prop.outcome,
			output_type="LL", 
			decay_type = decay_type,
			method="L-BFGS-B", lower=0, upper=1) 
		
		# COLLECT OR DECLARE DECAY PARAMETERES FROM OPTIM OUTPUT
		if (decay_type == 0) {
			neg_decay = 0
			pos_decay = 0
			neg_decay_inaction = NA
			pos_decay_inaction = NA
			decay_params <- c(0,0)		
		} else 	if (decay_type == 1) {
			neg_decay = optim_output$par[1]
			pos_decay = neg_decay
			neg_decay_inaction = NA
			pos_decay_inaction = NA
			decay_params <- c(neg_decay, pos_decay)		
		} else	if (decay_type == 2) {
			neg_decay = optim_output$par[1]
			pos_decay = optim_output$par[2]
			neg_decay_inaction = NA
			pos_decay_inaction = NA
			decay_params <- c(neg_decay, pos_decay)
		} else	if (decay_type == 3) {
			neg_decay = optim_output$par[1]
			pos_decay = optim_output$par[2]
			neg_decay_inaction = optim_output$par[3]
			pos_decay_inaction = optim_output$par[4]
			decay_params <- c(neg_decay, pos_decay, neg_decay_inaction, pos_decay_inaction)
		}
		
		# CREATE DATA BASED ON DECAY PARAMETERS
		emot_decay_out = data.frame(current_model(decay_params, 
			trial = current_data$trial, 
			choice = current_data$gamble, 
			outcome= current_data$outcome, 
			mag1.prop= current_data$mag1_prop.outcome, 
			mag2.prop= current_data$mag2_prop.outcome, 
			sure2.prop= current_data$sure2_prop.outcome, 
			output_type="data",
			decay_type = decay_type) )
	
		emot_decay_out$neg_decay <- rep(neg_decay, times=nrow(emot_decay_out))
		emot_decay_out$pos_decay <- rep(pos_decay, times=nrow(emot_decay_out))
		emot_decay_out$neg_decay_inaction = rep(neg_decay_inaction, times=nrow(emot_decay_out))
		emot_decay_out$pos_decay_inaction = rep(pos_decay_inaction, times=nrow(emot_decay_out))
		
		emot_decay_out$convergence <- rep(optim_output$convergence, times=nrow(emot_decay_out))
		emot_decay_out$iterations <- rep(optim_output$counts[1], times=nrow(emot_decay_out))
		emot_decay_out$LL			 <- rep(optim_output$value, times=nrow(emot_decay_out))
		
		emot_decay_out$domain <- rep(domain, times=nrow(emot_decay_out))
		emot_decay_out$decay_type <- rep(decay_type, times=nrow(emot_decay_out))
		emot_decay_out$model <- rep(model, times=nrow(emot_decay_out))
		
		current_data <- cbind(current_data, emot_decay_out)	
		current_model_df <- rbind(current_model_df, current_data) # for subloop
		
	}
	
	out_file_name <- paste(c("../analysis/_analysis_emot/model", 
		model, "_", domain,"_decayType", decay_type, ".txt"), sep="", collapse="")
	out_file_data_name <- paste(c("../analysis/_analysis_emot/model", 
		model, "_", domain,"_decayType", decay_type, "_data.R"), sep="", collapse="")
	save(current_model_df, file=out_file_data_name)
	
	

} # for decay_type
} # for domain
} # for model





