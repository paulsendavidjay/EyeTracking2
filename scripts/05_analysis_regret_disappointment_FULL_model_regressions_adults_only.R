library(lme4)
library(ggplot2)
library(RColorBrewer)
library(reshape)
source("~/R-stuff/functions/summarySE.R")

setwd("~/Documents/Academics/Projects/EyeTrackingGaze2/scripts/")




##############################################################################
##############################################################################
#								COLLECTING STATISTICS


model_list <- list()

model_list[["Base"]] <- as.formula("gamble ~ cv1 + prev_outcome + (1 | subjectID)")

model_list[["Choice"]] <- as.formula("gamble ~ cv1 + prev_outcome + sure2_prop.decision + mag1_prop.decision + (1 | subjectID)")

model_list[["Outcome"]] <- as.formula("gamble ~ cv1 + prev_outcome + relief + regret + relief_inaction + regret_inaction + (1 | subjectID)")

#model_list[["Model_3"]] <- as.formula("gamble ~ cv1 + prev_outcome + relief + regret + relief_inaction + regret_inaction + disappointment + elation + (1 | subjectID)")

model_list[["Full"]] <- as.formula("gamble ~ cv1 + prev_outcome + relief + regret + relief_inaction + regret_inaction + sure2_prop.decision + mag1_prop.decision + (1 | subjectID)")




max_nonconvergent <- list()
max_nonconvergent[["Gain"]] <- c(60, 61, 66, 509, 512, 516, 530)
max_nonconvergent[["Loss"]] <- c(55, 66, 67, 512, 521)


model = 2
domain = "Gain"
decay_type = 3
	

model_comparison_df <- data.frame()
for (domain in c("Gain","Loss")) {
for (model in names(model_list)) {

	# WE USE MODEL 2 FOR ALL THESE ANALYSES
	out_file_data_name <- paste(c("../analysis/_analysis_emot/model2_", domain,"_decayType", decay_type, "_data.R"), sep="", collapse="")
	load(out_file_data_name) # current_model_df
	current_model_df <- subset(current_model_df, AgeGroup == "adult")


	current_model_df$prev_outcome <- factor(current_model_df$prev_outcome)
	current_convergence <- summarySE(current_model_df, measurevar="convergence", groupvars="subjectID")$convergence

	
	current_glm <- glmer(model_list[[model]], data=current_model_df, subset=convergence==0, family=binomial("logit"))
	exclude_list <- max_nonconvergent[[domain]]
	# TO COMPARE AICs WE NEED AN EQUAL NUMBER OF DATA POINTS - THIS EXCLUDES MAX NUM SUBJECTS FOR MODEL 3, DECAY 3, BY DOMAIN
	AIC <- summary(current_glm)@AICtab[1]
	current_z <- data.frame(t(summary(current_glm)@coefs[,3]))
	current_df <- data.frame(model=model, decay_type=decay_type, 
		domain=domain, AIC=AIC, nonconvergent=sum(current_convergence > 0),
		n_param=length(current_z))
	current_df <- cbind(current_df , current_z)
	model_comparison_df <- rbind.fill(model_comparison_df, current_df)

} # for model
} # for domain


out_file_data_name <- "../analysis/_analysis_emot/model_comparisons_Base_to_Full_adult.R"
save(model_comparison_df,  file=out_file_data_name) # current_model_df



##############################################################################
##############################################################################
#								PLOTTING RESULTS




load(out_file_data_name)

# CREATE ADJUSTED AIC BY DOMAIN
model_comparison_df <- ddply(model_comparison_df, .(domain), transform, AIC_adjusted = AIC - min(AIC))
model_comparison_df$model <- factor(model_comparison_df$model, levels=c("Full","Outcome","Choice","Base"))

uninteresting_vars <- c("nonconvergent","AIC", "decay_type", "AIC_adjusted", "X.Intercept.","n_param","nonconvergent")

aic_ordered_vars <- rev(c("cv1", "prev_outcome1", "prev_outcome2", "sure2_prop.decision", "mag1_prop.decision", "relief", "regret", "relief_inaction", "regret_inaction", "AIC", "AIC_adjusted", "nonconvergent", "n_param", "X.Intercept."))

aic_ordered_labels <- rev(c("cv", "prev. outcome (sure bet)", "prev. outcome\n(Gain: Win, Loss: Lose)", "prop. dwell on sure bet\nduring decision", "prop. dwell on large mag\nduring decision", "relief", "regret", "relief_inaction", "regret_inaction", "AIC", "AIC_adjusted", "nonconvergent", "n_param", "X.Intercept."))



model_comparison_df$model <- factor(model_comparison_df$model, levels=rev(c("Full","Outcome","Choice","Base")))


model_comparison_df_long <- melt(model_comparison_df, id=c("model", "domain", "decay_type"))
# REORDER VARIABLES FOR PLOTTING
model_comparison_df_long$variable <- factor(model_comparison_df_long$variable, 
	levels= aic_ordered_vars,
	labels= aic_ordered_labels)

myColors <- brewer.pal(3,"Set1")[c(3,1)]
names(myColors) <- levels(model_comparison_df_long$domain)

# SET TEXT SIZING FOR ALL PLOTS
text_sizing <- 	theme(axis.text.x = element_text(size=6),
		axis.text.y = element_text(size=6),
		axis.title.x = element_text(size=8),
		axis.title.y = element_text(size=8),
		legend.text = element_text(size=8),
		legend.title = element_text(size=8),
		strip.text.x = element_text(size=9),
		title = element_text(size=12)) 

# COMPARE ADJUSTED AICS FOR EXCLUSIONARY MODELS - MAX MODEL/DECAY/DOMAIN SPECIFIC SUBJECTS EXCLUDED
#dev.new(height=4,width=4)

pdf(file="~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/AIC_comparison_base_vs_FULL_adults.pdf", height=4, width=4)
ggplot(model_comparison_df, aes(x=model, y= AIC_adjusted, fill=domain)) +
	geom_bar(stat="identity", position="dodge") +
	facet_wrap( ~ domain, ncol=1) +
	scale_fill_manual(name = "Domain", values = myColors) +
	text_sizing +
	labs(title="Model AIC Comparison", x="", y="AIC (adusted)") +
	theme(legend.position="none") + # remove legend
	coord_flip()
dev.off()



##############################################################################
#								GAIN


# COMPARE MODELS AND DECAY TYPE FOR GAIN DOMAIN
#dev.new(height=4,width=9)
pdf(file="~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/analysis_model_comparison_base_vs_FULL_PEs_Gain_adults.pdf", height=4,width=6)
ggplot(subset(model_comparison_df_long, domain=="Gain" & ! variable %in% uninteresting_vars), aes(x= variable, y=value)) +
	geom_bar(stat="identity", position="dodge", fill=myColors[1]) +
	facet_wrap(~ model, ncol=4) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip() +
	theme(panel.background=element_blank()) + 
	text_sizing +
	labs(title="Gain Domain", x="", y="z value")

dev.off()

##############################################################################
#								LOSS


# COMPARE MODELS AND DECAY TYPE FOR LOSS DOMAIN
#dev.new(height=4,width=9)
pdf(file="~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/analysis_model_comparison_base_vs_FULL_PEs_Loss_adults.pdf", height=4,width=6)
ggplot(subset(model_comparison_df_long, domain=="Loss" & ! variable %in% uninteresting_vars), aes(x= variable, y=value)) +
	geom_bar(stat="identity", position="dodge", fill=myColors[2]) +
	facet_wrap(~ model, ncol=4) +
	geom_hline(yintercept=c(-1.96,0,1.96), linetype=c("dotted","solid","dotted")) +
	coord_flip() +
	theme(panel.background=element_blank()) + 
	text_sizing +
	labs(title="Loss Domain", x="", y="z value")

dev.off()





