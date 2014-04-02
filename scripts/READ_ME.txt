

This collection of scripts has been set aside for use in processing data for the 'emotional' model of eye-tracking data, i.e. regret, relief, disappointment, elation.

%%%		PREPROCESSING STEP
preprocess_JP_pup_GL(subjectID)
	preprocess: begins with raw data
	JP: includes John Payne's split of decision epoch eye-tracking data into 5 sub-epochs.
	pup: includes pupillometry measurements
	GL: Gain and Loss conditions (in contrast to earlier pilot and Exp. 1 studies which only had Gain)

	Control subjects recieve their own processing script because the paradigm is different with substantial reduction in the number of variables that get created.

%%%		COMBINE INDIVIDUAL DATA
combine_$
	The combine scripts bring together data for the final list of subjects into a single dataframe object.

%%%		COLLECT DECAY PARAMETER ESTIMATES AND GENERATE EMOTIONAL VARIABLE VALUES FOR EACH SUBJECT
analysis_regret_disappointment_models.R
	runs all subjects through the different models to be compared for Gain and Loss domains, for multiple ways of including decay parameters: results are dataframe objects with individual parameters for each trial, as well as the generated emotional variable values


%%%   REGRESSIONS
analysis_regret_disappointment_models_regressions.R
	Runs regression models that include AgeGroup and its interactions as predictors.
	Examines convergence at step 1 to verify and/or edit initial parameter estimates.
	Need to rerun analysis_regret_disappointment_models.R if any changes have been made
	
analysis_regret_disappointment_models_regressions_adults_only
analysis_regret_disappointment_models_regressions_children_only.R
	Runs regression models for each AgeGroup separately.
	Examines convergence at step 1 to verify and/or edit initial parameter estimates.
	Need to rerun analysis_regret_disappointment_models.R if any changes have been made
	This also occurs in analysis_regret_disappointment_models_regressions.R, so efforts may not need to be duplicated.
	Results compare the fit of each of the different models

	adult plots:
		analysis/_analysis_emot/emotion_adult_params_Gain_2-4.pdf
		analysis/_analysis_emot/emotion_adult_params_Gain_all.pdf
		analysis/_analysis_emot/emotion_adult_params_Gain_model3_.pdf
		analysis/_analysis_emot/emotion_adult_params_Gain_vs_Loss_model2_.pdf
		analysis/_analysis_emot/emotion_adult_params_Loss_2-4.pdf
		analysis/_analysis_emot/emotion_adult_params_Loss_all.pdf
		analysis/_analysis_emot/emotion_adult_params_Loss_model3_.pdf
		analysis/_analysis_emot/emotion_AIC_model_comparisons_adult.pdf


# COMPARE EMOTION MODELS TO NON-EMOTION MODELS
analysis_regret_disappointment_FULL_model_regressions_adults_only.R

	adult plots: 
	analysis/AIC_comparison_base_vs_FULL_adults.pdf
	analysis/analysis_model_comparison_base_vs_FULL_PEs_Gain_adults.pdf
	analysis/analysis_model_comparison_base_vs_FULL_PEs_Loss_adults.pdf






