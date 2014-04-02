library(lme4)
library(ggplot2)
library(RColorBrewer)
library(reshape)
source("~/R-stuff/functions/summarySE.R")

setwd("~/Documents/Academics/Projects/EyeTrackingGaze/scripts/")


model = 1
domain = "Gain"
decay_type = 1



##############################################################################
##############################################################################
#								CHECKING CONVERGENCE
sink("~/Documents/Academics/Projects/EyeTrackingGaze/analysis/_analysis_emot/_convergence.txt", append=F)
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
		print(unique(current_model_df[,c("subjectID", "convergence")]))
	} else {
		cat(paste(c("\nmodel=", model, "\ndecay_type=", decay_type, "\ndomain=\"", domain, "\"\nGOOD\n"),sep="",collapse=""))
	}

} # for domain
} # for decay_type
} # for model

sink()


##############################################################################
##############################################################################
#								COLLECTING STATISTICS


model_list <- list()

model_list[[1]] <- as.formula("gamble ~ cv1 + AgeGroup:cv1 + prev_outcome + prev_outcome:AgeGroup + relief + regret + relief:AgeGroup + regret:AgeGroup + (1 | subjectID)")

model_list[[2]] <- as.formula("gamble ~ cv1 + AgeGroup:cv1 + prev_outcome + prev_outcome:AgeGroup + relief + regret + relief:AgeGroup + regret:AgeGroup + relief_inaction + regret_inaction + relief_inaction:AgeGroup + regret_inaction:AgeGroup + (1 | subjectID)")

model_list[[3]] <- as.formula("gamble ~ cv1 + AgeGroup:cv1 + prev_outcome + prev_outcome:AgeGroup + relief + regret + relief:AgeGroup + regret:AgeGroup + relief_inaction + regret_inaction + relief_inaction:AgeGroup + regret_inaction:AgeGroup + disappointment + elation + disappointment:AgeGroup + elation:AgeGroup + (1 | subjectID)")

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
	
	current_convergence <- summarySE(current_model_df, measurevar="convergence", groupvars="subjectID")$convergence
		
		
	current_model_df$prev_outcome <- factor(current_model_df$prev_outcome)
	
	current_glm <- glmer(model_list[[model]], data=current_model_df, family=binomial("logit"))
	current_z <- data.frame(t(summary(current_glm)@coefs[,3]))
	current_df <- data.frame(model=model, decay_type=decay_type, 
		domain=domain,AIC=summary(current_glm)@AICtab[1], nonconvergent=sum(current_convergence > 0),
		n_param=length(current_z))
	current_df <- cbind(current_df , current_z)
	aic_z_df <- rbind.fill(aic_z_df, current_df)

	
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
	aic_z_excld_nonconverg_df <- rbind.fill(aic_z_excld_nonconverg_df, current_df)
	
	

} # for domain
} # for decay_type
} # for model
out_file_data_name <- "../analysis/_analysis_emot/model_comparisons_AIC_z.R"
save(aic_z_df, aic_z_excld_nonconverg_df,  file=out_file_data_name) # current_model_df














##############################################################################
##############################################################################
#								PLOTTING RESULTS





load(out_file_data_name)
aic_z_df$decay_type <- factor(aic_z_df$decay_type)
aic_z_df$model <- factor(aic_z_df$model)
aic_z_excld_nonconverg_df$decay_type <- factor(aic_z_excld_nonconverg_df$decay_type)
aic_z_excld_nonconverg_df$model <- factor(aic_z_excld_nonconverg_df$model)

# CREATE ADJUSTED AIC BY DOMAIN
aic_z_df <- ddply(aic_z_df, .(domain), transform, AIC_adjusted = AIC + n_param - min(AIC) - min(n_param))
aic_z_excld_nonconverg_df <- ddply(aic_z_excld_nonconverg_df, .(domain), transform, AIC_adjusted = AIC + n_param - min(AIC) - min(n_param))
aic_z_excld_nonconverg_df <- ddply(aic_z_excld_nonconverg_df, .(domain), transform, AIC_adjusted_excld = AIC.1 + n_param - min(AIC.1) - min(n_param))




# COMPARE ADJUSTED AICS FOR NONEXCLUSIONARY MODEL
dev.new(height=4,width=4)
ggplot(aic_z_df, aes(x=as.numeric(model), y= AIC_adjusted)) +
	geom_bar(stat="identity", aes(fill= decay_type), position="dodge") +
	facet_wrap( ~ domain, ncol=1)

# COMPARE ADJUSTED AICS FOR EXCLUSIONARY MODELS - ONLY MODEL/DECAY/DOMAIN SPECIFIC SUBJECTS EXCLUDED
dev.new(height=4,width=4)
ggplot(aic_z_excld_nonconverg_df, aes(x=as.numeric(model), y= AIC_adjusted)) +
	geom_bar(stat="identity", aes(fill= decay_type), position="dodge") +
	facet_wrap( ~ domain, ncol=1)

# COMPARE ADJUSTED AICS FOR EXCLUSIONARY MODELS - MAX MODEL/DECAY/DOMAIN SPECIFIC SUBJECTS EXCLUDED
dev.new(height=4,width=4)
ggplot(aic_z_excld_nonconverg_df, aes(x=as.numeric(model), y= AIC_adjusted_excld)) +
	geom_bar(stat="identity", aes(fill= decay_type), position="dodge") +
	facet_wrap( ~ domain, ncol=1)


uninteresting_vars <- c("nonconvergent","AIC", "AIC_adjusted", "X.Intercept.", "cv1", "cv1.AgeGroupchild","prev_outcome1", "prev_outcome2", "prev_outcome0.AgeGroupchild", 
	"prev_outcome1.AgeGroupchild", "prev_outcome2.AgeGroupchild","n_param", "AgeGroupchild.prev_outcome1", "AgeGroupchild.prev_outcome2")

aic_ordered_vars <- rev(c("regret","AgeGroupchild.regret","relief","AgeGroupchild.relief",
	"regret_inaction","AgeGroupchild.regret_inaction","relief_inaction","AgeGroupchild.relief_inaction","disappointment",
	"AgeGroupchild.disappointment","elation","AgeGroupchild.elation","cv1","cv1.AgeGroupchild","prev_outcome1", "prev_outcome2", "prev_outcome0.AgeGroupchild", 
	"prev_outcome1.AgeGroupchild", "prev_outcome2.AgeGroupchild","X.Intercept.","AIC","AIC_adjusted","nonconvergent", "n_param"))

aic_excld_ordered_vars <- rev(c("regret","AgeGroupchild.regret","relief","AgeGroupchild.relief",
	"regret_inaction","AgeGroupchild.regret_inaction","relief_inaction","AgeGroupchild.relief_inaction","disappointment",
	"AgeGroupchild.disappointment","elation","AgeGroupchild.elation","cv1","cv1.AgeGroupchild","prev_outcome1", "prev_outcome2", "prev_outcome0.AgeGroupchild", 
	"AgeGroupchild.prev_outcome1", "AgeGroupchild.prev_outcome2","X.Intercept.","AIC","AIC_adjusted","nonconvergent", "n_param"))


aic_z_df_long <- melt(aic_z_df, id=c("model", "domain", "decay_type"))
# REORDER VARIABLES FOR PLOTTING
aic_z_df_long$variable <- factor(aic_z_df_long$variable, 
	levels= aic_ordered_vars)

# ALL SUBJECTS (INCLUDING NONCONVERGENT SUBJECTS)
# COMPARE MODELS AND DECAY TYPE FOR GAIN DOMAIN
dev.new(height=4,width=9)
ggplot(subset(aic_z_df_long, domain=="Gain" & ! variable %in% uninteresting_vars), aes(x= variable, y=value, fill= decay_type)) +
	geom_bar(stat="identity", position="dodge") +
	facet_wrap(~ model, ncol=4) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip()


aic_z_excld_nonconverg_df_long <- melt(aic_z_excld_nonconverg_df, id=c("model", "domain", "decay_type"))
# REORDER VARIABLES FOR PLOTTING
aic_z_excld_nonconverg_df_long$variable <- factor(aic_z_excld_nonconverg_df_long$variable, 
	levels= aic_excld_ordered_vars)




##############################################################################
#								GAIN


# EXCLUDE NONCONVERGENT SUBJECTS
# COMPARE MODELS AND DECAY TYPE FOR GAIN DOMAIN - DECAY TYPES 1-3
dev.new(height=4,width=9)
ggplot(subset(aic_z_excld_nonconverg_df_long, domain=="Gain" & ! variable %in% uninteresting_vars & decay_type != 0), aes(x= variable, y=value, fill= decay_type)) +
	geom_bar(stat="identity", position="dodge") +
	facet_wrap(~ model, ncol=4) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip()
	
	
# COMPARE MODELS 2 AND 3 FOR GAIN DOMAIN	
dev.new(height=4,width=9)
ggplot(subset(aic_z_excld_nonconverg_df_long, domain=="Gain" & ! variable %in% uninteresting_vars & decay_type == 3), aes(x= variable, y=value)) +
	geom_bar(stat="identity", position="dodge") +
	facet_wrap(~ model, ncol=4) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip()


##############################################################################
#								LOSS


# COMPARE MODELS AND DECAY TYPE FOR LOSS DOMAIN - DECAY TYPES 1-3
dev.new(height=4,width=9)
ggplot(subset(aic_z_excld_nonconverg_df_long, domain=="Loss" & ! variable %in% uninteresting_vars & decay_type != 0), aes(x= variable, y=value, fill= decay_type)) +
	geom_bar(stat="identity", position="dodge") +
	facet_wrap(~ model, ncol=4) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip()
	

# COMPARE MODELS 2 AND 3 FOR LOSS DOMAIN	
dev.new(height=4,width=9)
ggplot(subset(aic_z_excld_nonconverg_df_long, domain=="Loss" & ! variable %in% uninteresting_vars & decay_type == 3), aes(x= variable, y=value)) +
	geom_bar(stat="identity", position="dodge") +
	facet_wrap(~ model, ncol=4) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip()




##############################################################################
#								GAIN VS. LOSS


# COMPARE GAIN TO LOSS FOR MODEL 3, DECAY TYPE 3	
myColors <- brewer.pal(3,"Set1")[c(3,1)]
names(myColors) <- levels(aic_z_excld_nonconverg_df_long$domain)
colScale <- scale_fill_manual(name = "domain", values = myColors)

dev.new(height=4,width=9)
ggplot(subset(aic_z_excld_nonconverg_df_long, model %in% c(2,3) & !variable %in% uninteresting_vars & decay_type == 3), aes(x= variable, y=value, fill=domain)) +
	geom_bar(stat="identity", position="dodge") +
	colScale +
	facet_wrap(model ~ domain, ncol=2) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip()




