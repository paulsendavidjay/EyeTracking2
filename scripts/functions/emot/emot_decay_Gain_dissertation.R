
# example
# choice <- c(0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1)
# outcome <- c(1, 0, 1, 1, 2, 0, 1, 2, 1, 2, 0, 2, 0, 1, 1, 1, 1, 0, 2, 1, 1, 2, 1, 2, 0, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 1, 0, 1, 1, 1, 2, 0)
# mag1.prop <- c(0.183333333, 0.252525253, 0.000000000, 0.000000000, 0.850467290, 0.529411765, 0.571428571, 0.850000000, 0.302521008, 0.747899160, 0.181034483, 0.557522124, 0.358333333, 0.000000000, 0.008333333, 0.037037037, 0.295918367, 0.193181818, 0.884297521, 0.099099099, 0.442622951, 0.834710744, 0.000000000, 0.764705882, 0.282828283, 0.842975207, 0.000000000, 0.571428571, 0.000000000, 0.000000000, 0.342857143, 0.208333333, 0.000000000, 0.000000000, 0.217391304, 0.773109244, 0.246753247, 0.983471074, 0.000000000, 0.473214286, 0.113207547, 0.000000000, 0.000000000, 0.000000000, 0.245762712, 0.064220183, 0.525423729, 0.100840336)
# mag2.prop <- c(0.00000000, 0.60606061, 0.00000000, 0.00000000, 0.14953271, 0.47058824, 0.00000000, 0.15000000, 0.00000000, 0.15966387, 0.81896552, 0.14159292, 0.64166667, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.80681818, 0.11570248, 0.00000000, 0.00000000, 0.16528926, 0.00000000, 0.23529412, 0.65656566, 0.15702479, 0.00000000, 0.15966387, 0.04347826, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.12173913, 0.11764706, 0.00000000, 0.01652893, 0.32258065, 0.20535714, 0.12264151, 0.00000000, 1.00000000, 0.00000000, 0.00000000, 0.25688073, 0.47457627, 0.89915966)
# sure2.prop <- c(0.81666667, 0.14141414, 1.00000000, 1.00000000, 0.00000000, 0.00000000, 0.42857143, 0.00000000, 0.69747899, 0.09243697, 0.00000000, 0.30088496, 0.00000000, 1.00000000, 0.99166667, 0.96296296, 0.70408163, 0.00000000, 0.00000000, 0.90090090, 0.55737705, 0.00000000, 1.00000000, 0.00000000, 0.06060606, 0.00000000, 1.00000000, 0.26890756, 0.95652174, 1.00000000, 0.65714286, 0.79166667, 1.00000000, 1.00000000, 0.66086957, 0.10924370, 0.75324675, 0.00000000, 0.67741935, 0.32142857, 0.76415094, 1.00000000, 0.00000000, 1.00000000, 0.75423729, 0.67889908, 0.00000000, 0.00000000)
 
 
# optim(c(.7,.8), emot_decay, choice = choice, outcome=outcome, mag1.prop=mag1.prop, mag2.prop=mag2.prop, sure2.prop=sure2.prop, output_type=="LL") 

# SURE TRIALS ARE SKIPPED, BUT DECAY RATES STILL APPLY FOR THAT TRIAL BY USING ^trial_interval


emot_decay_Gain_dissertation <- function(decay_temp, trial, trialType, gamble, outcome, coinsWon, sure1, sure2, mag1, mag2, mag1.prop, mag2.prop, sure1.prop, sure2.prop, output_type) {
	
	# mag1.prop <- current_data$mag1_prop.outcome
	# mag2.prop <- current_data$mag2_prop.outcome
	# sure1.prop <- current_data$sure1_prop.outcome
	# sure2.prop <- current_data$sure2_prop.outcome
	# choice <- current_data$gamble
	# outcome <- current_data$outcome

	# neg_decay <- 0.6455282
	# pos_decay <- 0.8963159

	neg_decay <- decay_temp[1]
	pos_decay <- decay_temp[2]

	
	n_obs <- length(trial)
	
	elation <- rep(0, n_obs)
	relief <- rep(0, n_obs)
	gain <- rep(0, n_obs)
	relief_inaction <- rep(0, n_obs)


	disappointment <- rep(0, n_obs)		
	regret <- rep(0, n_obs)		
	loss <- rep(0, n_obs)
	regret_inaction <- rep(0, n_obs)

	prev_outcome <- rep(NA, n_obs)

	for (i in 1:n_obs) {

		#print(i)
		current_trial_num <- trial[i]
		if ( i > 1) {
			trial_interval <- trial[i] - trial[i-1]
		}

		
		
		if (trialType[i] == 6) { # mag1 > mag2; sure1 > sure2
			
			if (coinsWon[i] %in% c(mag1[i],mag2[i]) ) {
				
				if (coinsWon[i] == mag1[i] ) { # they won
					# ADJUST POSITIVE EMOTION
					elation[i] <- mag2.prop[i] # WINS: loss time [counterfactual outcome]
					relief[i] <- sure1.prop[i] + sure2.prop[i] # WINS: sure time [counterfactual choice]
					gain[i] <- mag1.prop[i] # WINS: win time [outcome]

					if ( i > 1) {
						# UPDATE NEGATIVE EMOTION
						disappointment[i] <- neg_decay^trial_interval*disappointment[i-1] # LOSSES: win time [counterfactual outcome]
						regret[i] <- neg_decay^trial_interval*regret[i-1] # LOSSES: sure time [counterfactual choice]
						loss[i] <- neg_decay^trial_interval*loss[i-1] # LOSSES: loss time [outcome]			
		
						# UPDATE POSITIVE EMOTION
						elation[i] <- elation[i] + pos_decay^trial_interval*elation[i-1]  # WINS: sure time [counterfactual choice]
						relief[i] <- relief[i] + pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual outcome]
						gain[i] <- gain[i] + pos_decay^trial_interval*gain[i-1] # WINS: win time [outcome]
		
						# UPDATE INACTION EMOTIONS
						relief_inaction[i] <- pos_decay^trial_interval*relief_inaction[i-1]
						regret_inaction[i] <- neg_decay^trial_interval*regret_inaction[i-1]	
					}						
				}					
					
				if (coinsWon[i] == mag2[i] ) { # they lost
					# ADJUST NEGATIVE EMOTION
					
					disappointment[i] <- mag1.prop[i] # LOSSES: win time [counterfactual outcome]
					regret[i] <- sure1.prop[i] + sure2.prop[i] # LOSSES: sure time [counterfactual choice]
					loss[i] <- mag2.prop[i] # LOSSES: loss time [outcome]			
				
					if ( i > 1) {
						# UPDATE POSITIVE EMOTION			
						elation[i] <- pos_decay^trial_interval*elation[i-1] # WINS: sure time [counterfactual choice]
						relief[i] <- pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual outcome]
						gain[i] <- pos_decay^trial_interval*gain[i-1] # WINS: win time [outcome]
		
						# UPDATE NEGATIVE EMOTION
						disappointment[i] <- disappointment[i] + neg_decay^trial_interval*disappointment[i-1] # LOSSES: win time [counterfactual outcome]
						regret[i] <- regret[i] + neg_decay^trial_interval*regret[i-1] # LOSSES: sure time [counterfactual choice]
						loss[i] <- loss[i] + neg_decay^trial_interval*loss[i-1] # LOSSES: loss time [outcome]			

						# UPDATE INACTION EMOTIONS
						relief_inaction[i] <- pos_decay^trial_interval*relief_inaction[i-1]
						regret_inaction[i] <- neg_decay^trial_interval*regret_inaction[i-1]	
		
					}			
				}					
	
			} else { # coinsWon %in% c(sure1, sure2)
			
				if (coinsWon[i] == sure1[i] ) { # they won
					# ADJUST POSITIVE EMOTION
					elation[i] <- sure2.prop[i] # WINS: loss time [counterfactual outcome]
					relief[i] <- mag2.prop[i] # WINS: loss time [counterfactual choice]
					gain[i] <- sure1.prop[i] # WINS: win time [outcome]
					# ADJUST NEGATIVE EMOTION FOR COUNTERFACTUAL GAIN
					regret_inaction[i] <- mag1.prop[i] # WINS: win time [counterfactual choice]

					if ( i > 1) {
						# UPDATE NEGATIVE EMOTION
						disappointment[i] <- neg_decay^trial_interval*disappointment[i-1] # LOSSES: win time [counterfactual outcome]
						loss[i] <- neg_decay^trial_interval*loss[i-1] # LOSSES: loss time [outcome]			

						regret[i] <- regret[i] + neg_decay^trial_interval*regret[i-1] # WIN: win time [counterfactual choice]

		
						# UPDATE POSITIVE EMOTION
						elation[i] <- elation[i] + pos_decay^trial_interval*elation[i-1]  # WINS: sure time [counterfactual choice]
						relief[i] <- relief[i] + pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual outcome]
						gain[i] <- gain[i] + pos_decay^trial_interval*gain[i-1] # WINS: win time [outcome]
			
						# UPDATE INACTION EMOTIONS
						relief_inaction[i] <- pos_decay^trial_interval*relief_inaction[i-1]
						regret_inaction[i] <- regret_inaction[i] + neg_decay^trial_interval*regret_inaction[i-1]	
					}						
				}					

				if (coinsWon[i] == sure2[i] ) { # they lost
					# ADJUST NEGATIVE EMOTION
					disappointment[i] <- sure1.prop[i] # LOSSES: win time [counterfactual outcome]
					loss[i] <- sure2.prop[i] # LOSSES: loss time [outcome]			
					
					# ADJUST POSITIVE EMOTION FOR COUNTERFACTUAL
					relief_inaction[i] <- mag2.prop[i] # LOSSES: loss time [counterfactual choice]
					regret_inaction[i] <- mag1.prop[i]  # LOSSES: win time [counterfactual choice]

					if ( i > 1) {
						# UPDATE NEGATIVE EMOTION
						disappointment[i] <- disappointment[i] + neg_decay^trial_interval*disappointment[i-1] # LOSSES: win time [counterfactual outcome]
						loss[i] <- loss[i] + neg_decay^trial_interval*loss[i-1] # LOSSES: loss time [outcome]			

						regret[i] <- neg_decay^trial_interval*regret[i-1] # LOSSES: sure time [counterfactual choice]
		
						# UPDATE POSITIVE EMOTION
						elation[i] <- pos_decay^trial_interval*elation[i-1]  # WINS: sure time [counterfactual choice]
						relief[i] <- pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual outcome]
						gain[i] <- pos_decay^trial_interval*gain[i-1] # WINS: win time [outcome]
			
						# UPDATE INACTION EMOTIONS
						relief_inaction[i] <- relief_inaction[i] + pos_decay^trial_interval*relief_inaction[i-1]
						regret_inaction[i] <- regret_inaction[i] + neg_decay^trial_interval*regret_inaction[i-1]	
					}						
				}					

				
			
			}
			
		} else { # trialType != 6
		
			if (coinsWon[i] == mag1[i]) { #WINS
	
				# ADJUST POSITIVE EMOTION
				elation[i] <- mag2.prop[i] # WINS: loss time [counterfactual outcome]
				relief[i] <- sure2.prop[i] # WINS: sure time [counterfactual choice]
				gain[i] <- mag1.prop[i] # WINS: win time [outcome]
				
				if ( i > 1) {
					# UPDATE NEGATIVE EMOTION
					disappointment[i] <- neg_decay^trial_interval*disappointment[i-1] # LOSSES: win time [counterfactual outcome]
					regret[i] <- neg_decay^trial_interval*regret[i-1] # LOSSES: sure time [counterfactual choice]
					loss[i] <- neg_decay^trial_interval*loss[i-1] # LOSSES: loss time [outcome]			
	
					# UPDATE POSITIVE EMOTION
					elation[i] <- elation[i] + pos_decay^trial_interval*elation[i-1]  # WINS: sure time [counterfactual choice]
					relief[i] <- relief[i] + pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual outcome]
					gain[i] <- gain[i] + pos_decay^trial_interval*gain[i-1] # WINS: win time [outcome]
	
					# UPDATE INACTION EMOTIONS
					relief_inaction[i] <- pos_decay^trial_interval*relief_inaction[i-1]
					regret_inaction[i] <- neg_decay^trial_interval*regret_inaction[i-1]
					
				}
	
			} else if (coinsWon[i] == mag2[i]) { # LOSSES
				
	
				# ADJUST NEGATIVE EMOTION
				
				disappointment[i] <- mag1.prop[i] # LOSSES: win time [counterfactual outcome]
				regret[i] <- sure2.prop[i] # LOSSES: sure time [counterfactual choice]
				loss[i] <- mag2.prop[i] # LOSSES: loss time [outcome]			
			
				if ( i > 1) {
					# UPDATE POSITIVE EMOTION			
					elation[i] <- pos_decay^trial_interval*elation[i-1] # WINS: sure time [counterfactual choice]
					relief[i] <- pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual outcome]
					gain[i] <- pos_decay^trial_interval*gain[i-1] # WINS: win time [outcome]
	
					# UPDATE NEGATIVE EMOTION
					disappointment[i] <- disappointment[i] + neg_decay^trial_interval*disappointment[i-1] # LOSSES: win time [counterfactual outcome]
					regret[i] <- regret[i] + neg_decay^trial_interval*regret[i-1] # LOSSES: sure time [counterfactual choice]
					loss[i] <- loss[i] + neg_decay^trial_interval*loss[i-1] # LOSSES: loss time [outcome]			

					# UPDATE INACTION EMOTIONS
					relief_inaction[i] <- pos_decay^trial_interval*relief_inaction[i-1]
					regret_inaction[i] <- neg_decay^trial_interval*regret_inaction[i-1]
	
				}			
	
			} else if (coinsWon[i] == sure2[i]) {


				relief_inaction[i] <- mag2.prop[i] # SURE: loss time [counterfactual choice]
				regret_inaction[i] <- mag1.prop[i] # SURE: win time [counterfactual choice]
			
					
				if ( i > 1) {
	
					# UPDATE POSITIVE EMOTION
					elation[i] <- pos_decay^trial_interval*elation[i-1] # WINS: sure time [counterfactual choice]
					relief[i] <- pos_decay^trial_interval*relief[i-1] # WINS: loss time [counterfactual outcome]
					gain[i] <- pos_decay^trial_interval*gain[i-1] # WINS: win time [outcome]
	
					# UPDATE NEGATIVE EMOTION
					disappointment[i] <- neg_decay^trial_interval*disappointment[i-1] # LOSSES: win time [counterfactual outcome]
					regret[i] <- neg_decay^trial_interval*regret[i-1] # LOSSES: sure time [counterfactual choice]
					loss[i] <- neg_decay^trial_interval*loss[i-1] # LOSSES: loss time [outcome]			

					# UPDATE INACTION EMOTIONS
					relief_inaction[i] <- relief_inaction[i] + pos_decay^trial_interval*relief_inaction[i-1]
					regret_inaction[i] <- regret_inaction[i] + neg_decay^trial_interval*regret_inaction[i-1]


				}
				
			}	# if outcome == c(win, loss, sure bet)

		}

		if (i > 1) {
			prev_outcome[i] <- as.numeric(as.character(outcome[i-1]))
		}

	} # for i in 1:n_obs


	disappointment0 <- 0
	regret0 <- 0
	loss0 <- 0
	regret_inaction0 <- 0

	elation0 <- 0
	relief0 <- 0
	gain0 <- 0
	relief_inaction0 <- 0


	# shift all values down by 1
	disappointment0[2:n_obs] <- 	disappointment[1:n_obs-1]
	regret0[2:n_obs] <- 			regret[1:n_obs-1]
	loss0[2:n_obs] <- 				loss[1:n_obs-1]
	regret_inaction0[2:n_obs] <- 	regret_inaction[1:n_obs-1]

	elation0[2:n_obs] <- 			elation[1:n_obs-1]
	relief0[2:n_obs] <- 			relief[1:n_obs-1]
	gain0[2:n_obs] <- 				gain[1:n_obs-1]
	relief_inaction0[2:n_obs] <- 			relief_inaction[1:n_obs-1]
	
	
	disappointment <- disappointment0
	regret <- regret0
	loss <- loss0
	elation <- elation0
	relief <- relief0
	gain <- gain0
	relief_inaction <- relief_inaction0
	regret_inaction <- regret_inaction0
		

	glm1 <- glm(gamble ~ disappointment + regret + elation + relief, family=binomial(link = "logit"))
	# + relief_inaction + regret_inaction
	if (output_type == "LL") {
		-1*logLik(glm1)[1]
	} else if (output_type == "data") {
		cbind(disappointment, regret, loss, elation, relief, regret_inaction, relief_inaction, gain, prev_outcome)
	}
}


