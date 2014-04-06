# EXAMINE CORRELATIONS BETWEEN DURATION OF FIXATION DURING OUTCOME EPOCHS
# AND DURATION OF FIXATION DURING SUBSEQUENT DECISION EPOCHS

library(lme4)
library(ggplot2)
library(RColorBrewer)
library(reshape)
source("~/R-stuff/functions/summarySE.R")

setwd("~/Documents/Academics/Projects/EyeTrackingGaze/scripts/")

# SPECIFY MODEL FOR TESTING - THIS IS THE ONE WE'LL ULTIMATELY USE
model = 2
domain = "Gain"
decay_type = 2

out_file_data_name <- paste(c("../analysis/_analysis_emot/model", 
	model, "_", domain,"_decayType", decay_type, "_data.R"), sep="", collapse="")
load(out_file_data_name) # current_model_df


# SET UP DATAFRAME OBJECT FOR STATS
current_model_df <- subset(current_model_df, AgeGroup == "adult")

current_model_df <- ddply(current_model_df, .(subjectID), transform, 
	mag1_shifted.outcome =(c(0,mag1_prop.outcome[1:length(mag1_prop.outcome)-1])))

current_model_df <- ddply(current_model_df, .(subjectID), transform, 
	mag2_shifted.outcome =(c(0,mag2_prop.outcome[1:length(mag2_prop.outcome)-1])))

current_model_df <- ddply(current_model_df, .(subjectID), transform, 
	sure2_shifted.outcome =(c(0,sure2_prop.outcome[1:length(sure2_prop.outcome)-1])))

do.call("rbind", as.list(
  by(current_model_df, current_model_df["subjectID"], cor(mag1_shifted.outcome, mag1_prop.decision))
))



# RUN STATISTICS

# OVERALL CORRELATION
cor.test(current_model_df$mag1_shifted.outcome, current_model_df$mag1_prop.decision)

# CORRELATION BY SUBJECTID
ddply(current_model_df, .(subjectID), cor.test(mag1_shifted.outcome, mag1_prop.decision))

# GET CORRELATION PVALUES AND ESTIMATES BETWEEN MAG1 LOOKING TIMES AT CHOICE AND AT PRIOR OUTCOME
# SPLIT TURNS dataframe into list according to 
sapply(split(current_model_df,current_model_df$subjectID), function(x) { 
	cor.test(x[,'mag1_shifted.outcome'], x[,'mag1_prop.decision'])[c("estimate", "p.value")]
})


# SET UP DATAFRAME OBJECT FOR PLOTTING
prop.decision <- melt(current_model_df[,c("mag1_prop.decision","mag2_prop.decision","sure2_prop.decision")])
prop.outcome <-	 melt(current_model_df[,c("mag1_shifted.outcome","mag2_shifted.outcome","sure2_shifted.outcome")])

prop.decision$object <- apply(prop.decision, 2, function(x) { 
	substr(x, 1,4)
})[,1]

prop <- cbind(prop.decision[,c(3,2)], prop.outcome[2])
names(prop) <- c("object", "decision", "outcome")

prop.outcome$epoch <- "outcome"


# PLOT DATA
pdf(file="~/Documents/Academics/Projects/EyeTrackingGaze/analysis/_analysis_emot/fixation_outcome_vs_decision.pdf", height=3.5, width=5)

	ggplot(prop, aes(x=decision, y=outcome)) +
		geom_point(aes(color=object), size=1) +
		theme(panel.background=element_blank()) + 
		theme(axis.text.x = element_text(size=6),
			axis.text.y = element_text(size=6),
			axis.title.x = element_text(size=8),
			axis.title.y = element_text(size=8),
			legend.text = element_text(size=8),
			legend.title = element_text(size=8),
			#legend.key.size = unit(3, "mm"), 
			strip.text.x = element_text(size=9),
			title = element_text(size=12)) +
		labs(title="Prop. Gaze Duration Between Outcome\n and Subsequent Choice Epochs", color="Fixation\nObject")

dev.off()









