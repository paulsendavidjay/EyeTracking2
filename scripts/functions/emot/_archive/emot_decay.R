
# example
# choice <- c(0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1)
# outcome <- c(1, 0, 1, 1, 2, 0, 1, 2, 1, 2, 0, 2, 0, 1, 1, 1, 1, 0, 2, 1, 1, 2, 1, 2, 0, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 1, 0, 1, 1, 1, 2, 0)
# mag1.prop <- c(0.183333333, 0.252525253, 0.000000000, 0.000000000, 0.850467290, 0.529411765, 0.571428571, 0.850000000, 0.302521008, 0.747899160, 0.181034483, 0.557522124, 0.358333333, 0.000000000, 0.008333333, 0.037037037, 0.295918367, 0.193181818, 0.884297521, 0.099099099, 0.442622951, 0.834710744, 0.000000000, 0.764705882, 0.282828283, 0.842975207, 0.000000000, 0.571428571, 0.000000000, 0.000000000, 0.342857143, 0.208333333, 0.000000000, 0.000000000, 0.217391304, 0.773109244, 0.246753247, 0.983471074, 0.000000000, 0.473214286, 0.113207547, 0.000000000, 0.000000000, 0.000000000, 0.245762712, 0.064220183, 0.525423729, 0.100840336)
# mag2.prop <- c(0.00000000, 0.60606061, 0.00000000, 0.00000000, 0.14953271, 0.47058824, 0.00000000, 0.15000000, 0.00000000, 0.15966387, 0.81896552, 0.14159292, 0.64166667, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.80681818, 0.11570248, 0.00000000, 0.00000000, 0.16528926, 0.00000000, 0.23529412, 0.65656566, 0.15702479, 0.00000000, 0.15966387, 0.04347826, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.12173913, 0.11764706, 0.00000000, 0.01652893, 0.32258065, 0.20535714, 0.12264151, 0.00000000, 1.00000000, 0.00000000, 0.00000000, 0.25688073, 0.47457627, 0.89915966)
# sure2.prop <- c(0.81666667, 0.14141414, 1.00000000, 1.00000000, 0.00000000, 0.00000000, 0.42857143, 0.00000000, 0.69747899, 0.09243697, 0.00000000, 0.30088496, 0.00000000, 1.00000000, 0.99166667, 0.96296296, 0.70408163, 0.00000000, 0.00000000, 0.90090090, 0.55737705, 0.00000000, 1.00000000, 0.00000000, 0.06060606, 0.00000000, 1.00000000, 0.26890756, 0.95652174, 1.00000000, 0.65714286, 0.79166667, 1.00000000, 1.00000000, 0.66086957, 0.10924370, 0.75324675, 0.00000000, 0.67741935, 0.32142857, 0.76415094, 1.00000000, 0.00000000, 1.00000000, 0.75423729, 0.67889908, 0.00000000, 0.00000000)
 
 
# optim(c(.7,.8), emot_decay, choice = choice, outcome=outcome, mag1.prop=mag1.prop, mag2.prop=mag2.prop, sure2.prop=sure2.prop, output_type=="LL") 



emot_decay <- function(decay_temp, choice, outcome, mag1.prop, mag2.prop, sure2.prop, output_type) {
	
	

	# mag1.prop <- current_data$mag1_prop.outcome
	# mag2.prop <- current_data$mag2_prop.outcome
	# sure2.prop <- current_data$sure2_prop.outcome
	# choice <- current_data$gamble
	# outcome <- current_data$outcome

	# neg_decay <- 0.6455282
	# pos_decay <- 0.8963159

	neg_decay <- decay_temp[1]
	pos_decay <- decay_temp[2]

	
	n_obs <- length(choice)
	
	elation <- rep(0, n_obs)
	relief <- rep(0, n_obs)
	gain <- rep(0, n_obs)

	disappointment <- rep(0, n_obs)		
	regret <- rep(0, n_obs)		
	loss <- rep(0, n_obs)



	for (i in 1:n_obs) {

		if (outcome[i] == 0) { #WINS

			# ADJUST POSITIVE EMOTION
			elation[i] <- sure2.prop[i] # WINS: sure time [counterfactual choice]
			relief[i] <- mag1.prop[i] # WINS: loss time [counterfactual outcome]
			gain[i] <- mag2.prop[i] # WINS: win time [outcome]

			if ( i > 1) {
				# UPDATE NEGATIVE EMOTION
				disappointment[i] <- neg_decay*disappointment[i-1] # LOSSES: win time [counterfactual outcome]
				regret[i] <- neg_decay*regret[i-1] # LOSSES: sure time [counterfactual choice]
				loss[i] <- neg_decay*loss[i-1] # LOSSES: loss time [outcome]			

				# UPDATE POSITIVE EMOTION
				elation[i] <- elation[i] + pos_decay*elation[i-1]  # WINS: sure time [counterfactual choice]
				relief[i] <- relief[i] + pos_decay*relief[i-1] # WINS: loss time [counterfactual outcome]
				gain[i] <- gain[i] + pos_decay*gain[i-1] # WINS: win time [outcome]

			}


		} else if (outcome[i] == 2) { # LOSSES
			
			# ADJUST NEGATIVE EMOTION
			disappointment[i] <- mag2.prop[i] # LOSSES: win time [counterfactual outcome]
			regret[i] <- sure2.prop[i] # LOSSES: sure time [counterfactual choice]
			loss[i] <- mag1.prop[i] # LOSSES: loss time [outcome]			
			
			if ( i > 1) {
				# UPDATE POSITIVE EMOTION			
				elation[i] <- pos_decay*elation[i-1] # WINS: sure time [counterfactual choice]
				relief[i] <- pos_decay*relief[i-1] # WINS: loss time [counterfactual outcome]
				gain[i] <- pos_decay*gain[i-1] # WINS: win time [outcome]

				# UPDATE NEGATIVE EMOTION
				disappointment[i] <- disappointment[i] + neg_decay*disappointment[i-1] # LOSSES: win time [counterfactual outcome]
				regret[i] <- regret[i] + neg_decay*regret[i-1] # LOSSES: sure time [counterfactual choice]
				loss[i] <- loss[i] + neg_decay*loss[i-1] # LOSSES: loss time [outcome]			

			}			


		} else if (outcome[i] == 1) {
			
			if ( i > 1) {
				elation[i] <- pos_decay*elation[i-1] # WINS: sure time [counterfactual choice]
				relief[i] <- pos_decay*relief[i-1] # WINS: loss time [counterfactual outcome]
				gain[i] <- pos_decay*gain[i-1] # WINS: win time [outcome]

				# UPDATE NEGATIVE EMOTION
				disappointment[i] <- neg_decay*disappointment[i-1] # LOSSES: win time [counterfactual outcome]
				regret[i] <- neg_decay*regret[i-1] # LOSSES: sure time [counterfactual choice]
				loss[i] <- neg_decay*loss[i-1] # LOSSES: loss time [outcome]			
			}
		}	# if outcome == c(win, loss, sure bet)

	} # for i in 1:n_obs


	disappointment0 <- 0
	regret0 <- 0
	loss0 <- 0

	elation0 <- 0
	relief0 <- 0
	gain0 <- 0

	# shift all values down by 1
	disappointment0[2:n_obs] <- 	disappointment[1:n_obs-1]
	regret0[2:n_obs] <- 	regret[1:n_obs-1]
	loss0[2:n_obs] <- 		loss[1:n_obs-1]

	elation0[2:n_obs] <- 	elation[1:n_obs-1]
	relief0[2:n_obs] <- 	relief[1:n_obs-1]
	gain0[2:n_obs] <- 		gain[1:n_obs-1]
	
	disappointment <- disappointment0
	regret <- regret0
	loss <- loss0
	elation <- elation0
	relief <- relief0
	gain <- gain0
	
	
	glm1 <- glm(choice ~ disappointment + regret + elation + relief, family=binomial(link = "logit"))
	
	if (output_type == "LL") {
		-1*logLik(glm1)[1]
		#logLik(glm1)[1]
	} else if (output_type == "data") {
		cbind(disappointment, regret, elation, relief)
	}
	
	
}


