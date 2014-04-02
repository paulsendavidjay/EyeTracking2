# THIS MODEL USES ONLY REGRET AND RELIEF: LOOKING AT COUNTERFACTUAL CHOICE
#	AFTER LOSS - REGRET
#	AFTER WIN  - RELIEF
# 	AFTER SURE BET - -1*REGRET & -1*RELIEF



# SURE TRIALS ARE SKIPPED, BUT DECAY RATES STILL APPLY FOR THAT TRIAL BY USING ^trial_interval


emot_decay_Loss_model1_rr <- function(decay_temp, trial, choice, outcome, mag1.prop, mag2.prop, sure2.prop, output_type, decay_type) {
	

	if (decay_type == 0) {
		neg_decay = 0
		pos_decay = 0	
	} else if (decay_type == 1) {
		neg_decay <- decay_temp[1]
		pos_decay <- neg_decay		
	} else if (decay_type == 2) {
		neg_decay <- decay_temp[1]
		pos_decay <- decay_temp[2]
	}
	
	n_obs <- length(choice)
	relief <- rep(0, n_obs)
	regret <- rep(0, n_obs)		

	prev_outcome <- rep(NA, n_obs)

	for (i in 1:n_obs) {


		current_trial_num <- trial[i]
		if ( i > 1) {
			trial_interval <- trial[i] - trial[i-1]
		}

		if (outcome[i] == 0) { #WINS

			# ADJUST POSITIVE EMOTION
			relief[i] <- sure2.prop[i] # WINS: sure time [counterfactual choice]

			if ( i > 1) {
				# UPDATE NEGATIVE EMOTION
				regret[i] <- neg_decay^trial_interval*regret[i-1] # LOSSES: sure time [counterfactual choice]
				# UPDATE POSITIVE EMOTION
				relief[i] <- relief[i] + pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual outcome]	
			}
			
				

		} else if (outcome[i] == 2) { # LOSSES

			# ADJUST NEGATIVE EMOTION
			regret[i] <- sure2.prop[i] # LOSSES: sure time [counterfactual choice]
		
			if ( i > 1) {
				# UPDATE NEGATIVE EMOTION
				regret[i] <- regret[i] + neg_decay^trial_interval*regret[i-1] # LOSSES: sure time [counterfactual choice]
				# UPDATE POSITIVE EMOTION			
				relief[i] <- pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual outcome]
			}			


		} else if (outcome[i] == 1) {
			
			# ADJUST POSITIVE EMOTION
			relief[i] <- -1*mag1.prop[i] # WINS: sure time [counterfactual choice]			

			# ADJUST NEGATIVE EMOTION
			regret[i] <- -1*mag2.prop[i] # LOSSES: sure time [counterfactual choice]
	
			if ( i > 1) {
	
				# UPDATE POSITIVE EMOTION
				relief[i] <- relief[i] + pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual choice & outcome]

				# UPDATE NEGATIVE EMOTION
				regret[i] <- regret[i] + neg_decay^trial_interval*regret[i-1] # LOSSES: sure time [counterfactual choice & outcome]
			}
	
			
		}	# if outcome == c(win, loss, sure bet)

		if (i > 1) {
			prev_outcome[i] <- as.numeric(as.character(outcome[i-1]))
		}

	} # for i in 1:n_obs


	regret0 <- 0
	relief0 <- 0

	# shift all values down by 1
	regret0[2:n_obs] <- 	regret[1:n_obs-1]

	relief0[2:n_obs] <- 	relief[1:n_obs-1]
	
	regret <- regret0
	relief <- relief0
	
		glm1 <- glm(choice ~ regret + relief, family=binomial(link = "logit"))


		if (output_type == "LL") {
			-1*logLik(glm1)[1]
		} else if (output_type == "data") {
			cbind(regret, relief, prev_outcome)
		}
	
}


