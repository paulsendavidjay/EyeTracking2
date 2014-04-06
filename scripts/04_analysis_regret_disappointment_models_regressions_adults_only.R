library(lme4)
library(ggplot2)
library(RColorBrewer)
library(reshape)
source("~/R-stuff/functions/summarySE.R")

setwd("~/Documents/Academics/Projects/EyeTrackingGaze2/scripts/")


# model = 2
# domain = "Gain"
# decay_type = 2

cols = brewer.pal(6,"Set2")

# SET TEXT SIZING FOR ALL PLOTS
text_sizing <- 	theme(axis.text.x = element_text(size=6),
		axis.text.y = element_text(size=6),
		axis.title.x = element_text(size=8),
		axis.title.y = element_text(size=8),
		legend.text = element_text(size=8),
		legend.title = element_text(size=8),
		strip.text.x = element_text(size=9),
		title = element_text(size=12)) 


##############################################################################
##############################################################################
#								CHECKING CONVERGENCE
sink("~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/_analysis_emot/_convergence.txt", append=F)
for (model in c(1, 2, 3)) {
for	(decay_type in c(0, 1, 2, 3)) {
for (domain in c("Gain","Loss")) {

	if (model == 1 & decay_type == 3) {
		next
	}

	out_file_data_name <- paste(c("../analysis/_analysis_emot/model", 
		model, "_", domain,"_decayType", decay_type, "_data.R"), sep="", collapse="")
	load(out_file_data_name) # current_model_df
	

	if (sum(current_model_df$convergence > 0) > 0) {
		cat(paste(c("\nmodel=", model, "\ndecay_type=", decay_type, "\ndomain=\"", domain, "\"\n"),sep="",collapse=""))
		print(unique(current_model_df[,c("subjectID","convergence")]))
	} else {
		cat(paste(c("\nmodel=", model, "\ndecay_type=", decay_type, "\ndomain=\"", domain, "\"\nGOOD\n"),sep="",collapse=""))
	}

} # for domain
} # for decay_type
} # for model

sink()




##############################################################################
##############################################################################
#								COLLECT STATISTICS


model_list <- list()
model_list[[1]] <- as.formula("gamble ~ cv1 + prev_outcome + relief + regret + (1 | subjectID)")
model_list[[2]] <- as.formula("gamble ~ cv1 + prev_outcome + relief + regret + relief_inaction + regret_inaction + (1 | subjectID)")
model_list[[3]] <- as.formula("gamble ~ cv1 + prev_outcome + relief + regret + relief_inaction + regret_inaction + disappointment + elation + (1 | subjectID)")

max_nonconvergent <- list()
max_nonconvergent[["Gain"]] <- c(60, 61, 66, 509, 512, 516, 530)
max_nonconvergent[["Loss"]] <- c(55, 66, 67, 512, 521)

# INITIALIZE EMPTY DATA FRAME
aic_z_df <- data.frame()
aic_z_excld_nonconverg_df <- data.frame()
for (model in c(3, 2, 1)) {
for	(decay_type in c(0, 1, 2, 3)) {
for (domain in c("Gain","Loss")) {

	if (model == 1 & decay_type == 3) {
		next
	}

	out_file_data_name <- paste(c("../analysis/_analysis_emot/model", 
		model, "_", domain,"_decayType", decay_type, "_data.R"), sep="", collapse="")
	load(out_file_data_name) # current_model_df
	
	current_model_df <- subset(current_model_df, AgeGroup == "adult")
	
	current_convergence <- summarySE(current_model_df, measurevar="convergence", groupvars="subjectID")$convergence
		
	current_model_df$prev_outcome <- factor(current_model_df$prev_outcome)
	
	# RUN FOR ALL SUBJECTS
	current_glm <- glmer(model_list[[model]], data=current_model_df, family=binomial("logit"))
	current_z <- data.frame(t(summary(current_glm)@coefs[,3]))
	current_df <- data.frame(model=model, decay_type=decay_type, 
		domain=domain,AIC=summary(current_glm)@AICtab[1], nonconvergent=sum(current_convergence > 0),
		n_param=length(current_z))
	current_df <- cbind(current_df , current_z)
	aic_z_df <- rbind.fill(aic_z_df, current_df)

	# RUN FOR SUBJECTS WITH SUCCESSFUL CONVERGENCE
	current_glm <- glmer(model_list[[model]], data=current_model_df, subset=convergence==0, family=binomial("logit"))
	exclude_list <- max_nonconvergent[[domain]]
	# TO COMPARE AICs WE NEED AN EQUAL NUMBER OF DATA POINTS - THIS EXCLUDES MAX NUM SUBJECTS FOR MODEL 3, DECAY 3, BY DOMAIN
	AIC_max_non <- summary( glmer(model_list[[model]], data=current_model_df, 
		subset= !(subjectID %in% exclude_list), 
		family = binomial("logit")) )@AICtab[1]
	current_z <- data.frame(t(summary(current_glm)@coefs[,3]))
	current_df <- data.frame(model=model, decay_type=decay_type, 
		domain=domain,AIC=summary(current_glm)@AICtab[1], AIC_max_non=AIC_max_non, nonconvergent=sum(current_convergence > 0),
		n_param=length(current_z))
	current_df <- cbind(current_df , current_z)
	
	# COMBINE RESULTS
	aic_z_excld_nonconverg_df <- rbind.fill(aic_z_excld_nonconverg_df, current_df)


} # for domain
} # for decay_type
} # for model
out_file_data_name <- "../analysis/_analysis_emot/emotion_model_comparisons_AIC_z_adults_only.R"
save(aic_z_df, aic_z_excld_nonconverg_df,  file=out_file_data_name) # current_model_df




##############################################################################
##############################################################################
#								PLOTTING RESULTS


out_file_data_name <- "../analysis/_analysis_emot/emotion_model_comparisons_AIC_z_adults_only.R"
load(out_file_data_name)

aic_z_df$decay_type <- factor(aic_z_df$decay_type)
aic_z_df$model <- factor(aic_z_df$model)
aic_z_excld_nonconverg_df$decay_type <- factor(aic_z_excld_nonconverg_df$decay_type)
aic_z_excld_nonconverg_df$model <- factor(aic_z_excld_nonconverg_df$model, 
	order=c(1,2,3), 
	labels=c("Regret & Relief", "Regret & Relief X\nAction & Inaction", "Regret & Relief X\nAction & Inaction\nDissapointment, Elation"))

# CREATE ADJUSTED AIC BY DOMAIN
aic_z_df <- ddply(aic_z_df, .(domain), transform, AIC_adjusted = AIC + as.numeric(as.character(decay_type)) + n_param - min(AIC) - min(n_param))
aic_z_excld_nonconverg_df <- ddply(aic_z_excld_nonconverg_df, .(domain), transform, AIC_adjusted = AIC + as.numeric(as.character(decay_type)) - min(AIC))
aic_z_excld_nonconverg_df <- ddply(aic_z_excld_nonconverg_df, .(domain), transform, AIC_adjusted_excld = AIC.1 + as.numeric(as.character(decay_type))  - min(AIC.1) )

aic_z_df$AIC_adjusted[aic_z_df$decay_type == 3] <- 
	aic_z_df$AIC_adjusted[aic_z_df$decay_type == 3]  + 1
aic_z_excld_nonconverg_df$AIC_adjusted[aic_z_excld_nonconverg_df$decay_type == 3] <- 
	aic_z_excld_nonconverg_df$AIC_adjusted[aic_z_excld_nonconverg_df$decay_type == 3] + 1
aic_z_excld_nonconverg_df$AIC_adjusted_excld[aic_z_excld_nonconverg_df$decay_type == 3] <- 
	aic_z_excld_nonconverg_df$AIC_adjusted_excld[aic_z_excld_nonconverg_df$decay_type == 3] + 1


# COMPARE ADJUSTED AICS FOR EXCLUSIONARY MODELS - MAX MODEL/DECAY/DOMAIN SPECIFIC SUBJECTS EXCLUDED
#dev.new(height=4,width=4)
pdf("~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/_analysis_emot/emotion_AIC_model_comparisons_adult.pdf",height=4, width=7)
ggplot(aic_z_excld_nonconverg_df, aes(x=model, y= AIC_adjusted_excld)) +
	geom_bar(stat="identity", aes(fill= decay_type), position="dodge") +
	facet_wrap( ~ domain, ncol=2) +
	theme(panel.background=element_blank()) + 
	text_sizing +
	labs(title="Model Comparison", x="", y="Adjusted AIC", fill="N Decay\nParams") +
	scale_fill_manual(values=cols[1:4], breaks=c("0","1","2","3"), labels=c("0","1","2","4"))
dev.off()

# IDENTIFY VARIABLES OF DISINTEREST FOR EXCLUSION DURING PLOTTING
uninteresting_vars <- c("nonconvergent","AIC", "AIC_adjusted","AIC.1","AIC_adjusted_excld", "X.Intercept.", "n_param", "cv")

# MAKE LIST OF VARIABLES ORDERED TO GET PLOTS IN THE RIGHT ORDER
aic_ordered_vars <- rev(c("regret","relief","regret_inaction","relief_inaction","disappointment",
	"elation","cv1","prev_outcome1", "prev_outcome2","X.Intercept.","AIC","AIC_adjusted","nonconvergent", "n_param"))

aic_excld_ordered_vars <- rev(c("regret","relief","regret_inaction","relief_inaction","disappointment",
	"elation","cv1","prev_outcome1", "prev_outcome2","X.Intercept.","AIC","AIC.1","AIC_adjusted","AIC_adjusted_excld","nonconvergent", "n_param"))

aic_excld_ordered_vars_labels <- rev(c("regret","relief","regret_inaction","relief_inaction","disappointment",
	"elation","cv","prev. outcome (sure bet)", "prev. outcome\n(Gain: Win, Loss: Lose)","X.Intercept.","AIC","AIC.1","AIC_adjusted","AIC_adjusted_excld","nonconvergent", "n_param"))

# PLACE DATAFRAME OF ALL SUBJECTS INTO LONG FORMAT FOR GGPLOT
aic_z_df_long <- melt(aic_z_df, id=c("model", "domain", "decay_type"))
# REORDER VARIABLES FOR PLOTTING
aic_z_df_long$variable <- factor(aic_z_df_long$variable, 
	levels= aic_ordered_vars)

# PLACE DATAFRAME OF CONVERGED SUBJECTS INTO LONG FORMAT FOR GGPLOT
aic_z_excld_nonconverg_df_long <- melt(aic_z_excld_nonconverg_df, id=c("model", "domain", "decay_type"))
# REORDER VARIABLES FOR PLOTTING
aic_z_excld_nonconverg_df_long$variable <- factor(aic_z_excld_nonconverg_df_long$variable, 
	levels= aic_excld_ordered_vars,
	labels= aic_excld_ordered_vars_labels)


##############################################################################
#								GAIN


# REORDER VARIABLES FOR PLOTTING
aic_excld_ordered_vars_labels_gain <- rev(c("regret","relief","regret_inaction","relief_inaction","disappointment",
	"elation","cv","prev. outcome (sure bet)", "prev. outcome (win)","X.Intercept.","AIC","AIC.1","AIC_adjusted","AIC_adjusted_excld","nonconvergent", "n_param"))

# PLACE DATAFRAME OF CONVERGED SUBJECTS INTO LONG FORMAT FOR GGPLOT
aic_z_excld_nonconverg_df_long <- melt(aic_z_excld_nonconverg_df, id=c("model", "domain", "decay_type"))
# REORDER VARIABLES FOR PLOTTING
aic_z_excld_nonconverg_df_long$variable <- factor(aic_z_excld_nonconverg_df_long$variable, 
	levels= aic_excld_ordered_vars,
	labels= aic_excld_ordered_vars_labels_gain)


# EXCLUDE NONCONVERGENT SUBJECTS
# COMPARE MODELS AND DECAY TYPE FOR GAIN DOMAIN - DECAY TYPES 1-3
#dev.new(height=4,width=9)
pdf("~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/_analysis_emot/emotion_adult_params_Gain_all.pdf",height=4,width=9)
ggplot(subset(aic_z_excld_nonconverg_df_long, domain=="Gain" & ! variable %in% uninteresting_vars & decay_type != 0), aes(x= variable, y=value, fill= decay_type)) +
	geom_bar(stat="identity", position="dodge") +
	facet_wrap(~ model, ncol=3) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip() +
	theme(panel.background=element_blank()) + 
	text_sizing +
	labs(title="Model Parameters: Gain Domain", x="", y="z value", fill="N Decay\nParams") +
	scale_fill_manual(values = cols[2:4], breaks=c("1","2","3"), labels=c("1","2","4")) 	
dev.off()

	
# COMPARE MODELS 2 AND 3 FOR GAIN DOMAIN	
#dev.new(height=4,width=9)
pdf("~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/_analysis_emot/emotion_adult_params_Gain_2-4.pdf",height=4,width=6)
ggplot(subset(aic_z_excld_nonconverg_df_long, domain=="Gain" & ! variable %in% uninteresting_vars & decay_type != 0 & model != "Regret & Relief"), aes(x= variable, y=value, fill= decay_type)) +
	geom_bar(stat="identity", position="dodge") +
	facet_wrap(~ model, ncol=2) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip() +
	theme(panel.background=element_blank()) + 
	text_sizing +
	labs(title="Model Parameters: Gain Domain", x="", y="z value", fill="N Decay\nParams") +
	scale_fill_manual(values = cols[2:4], breaks=c("1","2","3"), labels=c("1","2","4")) 	
dev.off()


##############################################################################
#								LOSS


# REORDER VARIABLES FOR PLOTTING
aic_excld_ordered_vars_labels_loss <- rev(c("regret","relief","regret_inaction","relief_inaction","disappointment",
	"elation","cv","prev. outcome (sure bet)", "prev. outcome (lose)","X.Intercept.","AIC","AIC.1","AIC_adjusted",
	"AIC_adjusted_excld","nonconvergent", "n_param"))

aic_z_excld_nonconverg_df_long <- melt(aic_z_excld_nonconverg_df, id=c("model", "domain", "decay_type"))
# REORDER VARIABLES FOR PLOTTING
aic_z_excld_nonconverg_df_long$variable <- factor(aic_z_excld_nonconverg_df_long$variable, 
	levels= aic_excld_ordered_vars,
	labels= aic_excld_ordered_vars_labels_loss)

# COMPARE MODELS AND DECAY TYPE FOR LOSS DOMAIN - DECAY TYPES 1-3
#dev.new(height=4,width=9)
pdf("~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/_analysis_emot/emotion_adult_params_Loss_all.pdf",height=4,width=9)
ggplot(subset(aic_z_excld_nonconverg_df_long, domain=="Loss" & ! variable %in% uninteresting_vars & decay_type != 0), aes(x= variable, y=value, fill= decay_type)) +
	geom_bar(stat="identity", position="dodge") +
	facet_wrap(~ model, ncol=4) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip() +
	theme(panel.background=element_blank()) + 
	text_sizing +
	labs(title="Model Parameters: Loss Domain", x="", y="z value", fill="N Decay\nParams") +
	scale_fill_manual(values=cols[2:4], breaks=c("1","2","3"), labels=c("1","2","4")) 
dev.off()


# COMPARE MODELS 2 AND 3 FOR LOSS DOMAIN	
#dev.new(height=4,width=9)
pdf("~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/_analysis_emot/emotion_adult_params_Loss_2-4.pdf",height=4,width=9)
ggplot(subset(aic_z_excld_nonconverg_df_long, domain=="Loss" & ! variable %in% uninteresting_vars & decay_type != 0 & model != "Regret & Relief"), aes(x= variable, y=value, fill= decay_type)) +
	geom_bar(stat="identity", position="dodge") +
	facet_wrap(~ model, ncol=2) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip() +
	theme(panel.background=element_blank()) + 
	text_sizing +
	labs(title="Model Parameters: Loss Domain", x="", y="z value", fill="N Decay\nParams") +
	scale_fill_manual(values=cols[2:4], breaks=c("1","2","3"), labels=c("1","2","4")) 
dev.off()


##############################################################################
#								GAIN VS. LOSS


# REORDER VARIABLES FOR PLOTTING
aic_z_excld_nonconverg_df_long <- melt(aic_z_excld_nonconverg_df, id=c("model", "domain", "decay_type"))
aic_z_excld_nonconverg_df_long$variable <- factor(aic_z_excld_nonconverg_df_long$variable, 
	levels= aic_excld_ordered_vars,
	labels= aic_excld_ordered_vars_labels)

# COMPARE GAIN TO LOSS FOR MODEL 3, DECAY TYPE 3	
myColors <- brewer.pal(3,"Set1")[c(3,1)]
names(myColors) <- levels(aic_z_excld_nonconverg_df_long$domain)

#dev.new(height=4,width=9)
pdf("~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/_analysis_emot/emotion_adult_params_Gain_vs_Loss_model2_.pdf",height=4,width=6)

ggplot(subset(aic_z_excld_nonconverg_df_long, model == "Regret & Relief X\nAction & Inaction" & !variable %in% uninteresting_vars & decay_type == 3 & !variable %in% c("disappointment", "elation")), aes(x= variable, y=value, fill=domain)) +
	geom_bar(stat="identity", position="dodge") +
	scale_fill_manual(name = "Domain", values = myColors) +
	facet_wrap(~ domain, ncol=2) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip() +
	theme(panel.background=element_blank()) + 
	text_sizing +
	labs(title="Gain & Loss Domains", x="", y="z value")
dev.off()


pdf("~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/_analysis_emot/emotion_adult_params_Loss_model3_.pdf",height=4,width=3.5)
ggplot(subset(aic_z_excld_nonconverg_df_long, domain == "Loss" & model == "Regret & Relief X\nAction & Inaction\nDissapointment, Elation" & !variable %in% uninteresting_vars & decay_type == 3), aes(x= variable, y=value, fill=domain)) +
	geom_bar(stat="identity", position="dodge", fill=myColors[2]) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	facet_wrap(~ domain, ncol=1) +
	coord_flip() +
	theme(panel.background=element_blank()) + 
	text_sizing +
	labs(title="", x="", y="z value")
dev.off()

	
pdf("~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/_analysis_emot/emotion_adult_params_Gain_model3_.pdf",height=4,width=3.5)
ggplot(subset(aic_z_excld_nonconverg_df_long, 
	domain == "Gain" & 
	model == "Regret & Relief X\nAction & Inaction\nDissapointment, Elation" & 
	!variable %in% uninteresting_vars & decay_type == 3), 
	aes(x= variable, y=value, fill=domain)) +
	geom_bar(stat="identity", position="dodge", fill=myColors[1]) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	facet_wrap(~ domain, ncol=1) +
	coord_flip() +
	theme(panel.background=element_blank()) + 
	text_sizing +
	labs(title="", x="", y="z value")
dev.off()







