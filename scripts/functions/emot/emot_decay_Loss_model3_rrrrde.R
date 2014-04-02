# THIS MODEL USES ONLY REGRET AND RELIEF: LOOKING AT COUNTERFACTUAL CHOICE
#	AFTER LOSS - REGRET
#	AFTER WIN  - RELIEF
# 	AFTER SURE BET - REGRET_INACTION & RELIEF_INACTION
#	MAG1 > MAG2


# SURE TRIALS ARE SKIPPED, BUT DECAY RATES STILL APPLY FOR THAT TRIAL BY USING ^trial_interval


emot_decay_Loss_model3_rrrrde <- function(decay_temp, trial, choice, outcome, mag1.prop, mag2.prop, sure2.prop, output_type, decay_type) {
	

	if (decay_type == 0) {
	
		neg_decay = 0
		pos_decay = 0	
		neg_decay_inaction = 0
		pos_decay_inaction = 0
	
	} else if (decay_type == 1) {
	
		neg_decay <- decay_temp[1]
		pos_decay <- neg_decay		
		neg_decay_inaction <- neg_decay
		pos_decay_inaction <- neg_decay
	
	} else if (decay_type == 2) {
	
		if (length(decay_temp) < 2) {
			stop("decay params must have length 2 (neg, pos) if decay_type == 2")
		}		
		neg_decay <- decay_temp[1]
		pos_decay <- decay_temp[2]
		neg_decay_inaction <- neg_decay
		pos_decay_inaction <- pos_decay
		
	} else if (decay_type == 3) {
		
		if (length(decay_temp) < 4) {
			stop("decay params must have length 4 (neg, pos, neg_inaction, pos_inaction) if decay_type == 3")
		}		
		
		neg_decay <- decay_temp[1]
		pos_decay <- decay_temp[2]
		neg_decay_inaction <- decay_temp[3]
		pos_decay_inaction <- decay_temp[4]
	
	}
	
	n_obs <- length(choice)
	relief <- rep(0, n_obs)
	regret <- rep(0, n_obs)		
	regret_inaction <- rep(0, n_obs)		
	relief_inaction <- rep(0, n_obs)
	disappointment <- rep(0, n_obs)		
	elation <- rep(0, n_obs)

	prev_outcome <- rep(NA, n_obs)

	for (i in 1:n_obs) {


		current_trial_num <- trial[i]
		if ( i > 1) {
			trial_interval <- trial[i] - trial[i-1]
		}

		if (outcome[i] == 0) { #WINS

			# ADJUST POSITIVE EMOTION
			relief[i] <- sure2.prop[i] # [counterfactual choice]
			elation[i] <- mag1.prop[i]

			if ( i > 1) {
				# UPDATE NEGATIVE EMOTION
				regret[i] <- neg_decay^trial_interval*regret[i-1] # [counterfactual choice]
				disappointment[i] <- neg_decay^trial_interval*disappointment[i-1] # [counterfactual outcome]
				# UPDATE POSITIVE EMOTION
				relief[i] <- relief[i] + pos_decay^trial_interval*relief[i-1] # [counterfactual choice]	
				elation[i] <- elation[i] + neg_decay^trial_interval*elation[i-1] # [counterfactual outcome]

				# UPDATE NEGATIVE EMOTION INACTION
				regret_inaction[i] <- neg_decay_inaction^trial_interval*regret_inaction[i-1] # [counterfactual choice]
				# UPDATE POSITIVE EMOTION INACTION
				relief_inaction[i] <- pos_decay_inaction^trial_interval*relief_inaction[i-1] # [counterfactual choice]	
				
			}
			
				

		} else if (outcome[i] == 2) { # LOSSES

			# ADJUST NEGATIVE EMOTION
			regret[i] <- sure2.prop[i] # LOSSES: sure time [counterfactual choice]
			disappointment[i] <- mag2.prop[i] # [counterfactual outcome]

			if ( i > 1) {
				# UPDATE NEGATIVE EMOTION
				regret[i] <- regret[i] + neg_decay^trial_interval*regret[i-1] # LOSSES: sure time [counterfactual choice]
				disappointment[i] <- disappointment[i] + neg_decay^trial_interval*disappointment[i-1] # [counterfactual outcome]
				# UPDATE POSITIVE EMOTION			
				relief[i] <- pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual outcome]
				elation[i] <- neg_decay^trial_interval*elation[i-1] # [counterfactual outcome]

				# UPDATE NEGATIVE EMOTION INACTION
				regret_inaction[i] <- neg_decay_inaction^trial_interval* regret_inaction[i-1] # [counterfactual choice]
				# UPDATE POSITIVE EMOTION INACTION
				relief_inaction[i] <- pos_decay_inaction^trial_interval*relief_inaction[i-1] # [counterfactual choice]	

			}			


		} else if (outcome[i] == 1) { # SAFE BET
			
			# ADJUST POSITIVE EMOTION
			relief_inaction[i] <- mag2.prop[i] # WINS: sure time [counterfactual choice]			

			# ADJUST NEGATIVE EMOTION
			regret_inaction[i] <- mag1.prop[i] # LOSSES: sure time [counterfactual choice]
	
			if ( i > 1) {
	
				# UPDATE POSITIVE EMOTION
				relief[i] <- pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual choice & outcome]
				disappointment[i] <- neg_decay^trial_interval*disappointment[i-1] # [counterfactual outcome]

				# UPDATE NEGATIVE EMOTION
				regret[i] <- neg_decay^trial_interval*regret[i-1] # LOSSES: sure time [counterfactual choice & outcome]
				elation[i] <- neg_decay^trial_interval*elation[i-1] # [counterfactual outcome]

				# UPDATE NEGATIVE EMOTION INACTION
				regret_inaction[i] <- regret_inaction[i] + neg_decay_inaction^trial_interval*regret_inaction[i-1] # [counterfactual choice]
				# UPDATE POSITIVE EMOTION INACTION
				relief_inaction[i] <- relief_inaction[i] + pos_decay_inaction^trial_interval*relief_inaction[i-1] # [counterfactual choice]	

			}
	
			
		}	# if outcome == c(win, loss, sure bet)

		if (i > 1) {
			prev_outcome[i] <- as.numeric(as.character(outcome[i-1]))
		}

	} # for i in 1:n_obs


	regret0 <- 0
	relief0 <- 0
	regret_inaction0 <- 0
	relief_inaction0 <- 0
	disappointment0 <- 0
	elation0 <- 0


	# shift all values down by 1
	regret0[2:n_obs] 		<- 		regret[1:n_obs-1]
	relief0[2:n_obs] 		<- 		relief[1:n_obs-1]
	regret_inaction0[2:n_obs] <- 	regret_inaction[1:n_obs-1]
	relief_inaction0[2:n_obs] <- 	relief_inaction[1:n_obs-1]
	disappointment0[2:n_obs] <- 	disappointment[1:n_obs-1]
	elation0[2:n_obs] 		<- 		elation[1:n_obs-1]

	regret <- regret0
	relief <- relief0
	regret_inaction <- regret_inaction0
	relief_inaction <- relief_inaction0
	disappointment <- disappointment0
	elation <- elation0
	
		
	glm1 <- glm(choice ~ regret + relief + regret_inaction + relief_inaction + disappointment + elation, family=binomial(link = "logit"))


	if (output_type == "LL") {
		-1*logLik(glm1)[1]
	} else if (output_type == "data") {
		cbind(regret, relief, regret_inaction, relief_inaction, disappointment, elation, prev_outcome)
	}


	
}


