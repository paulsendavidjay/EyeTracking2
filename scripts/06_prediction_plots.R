library(lme4)
library(ggplot2)
library(RColorBrewer)
library(reshape)
library(arm)
source("~/R-stuff/functions/summarySE.R")

setwd("~/Documents/Academics/Projects/EyeTrackingGaze2/scripts/")






##############################################################################
##############################################################################
#								COLLECTING STATISTICS


model_list <- list()

model_list[["Base"]] <- as.formula("gamble ~ cv1 + prev_outcome + (1 | subjectID)")

model_list[["Choice"]] <- as.formula("gamble ~ cv1 + prev_outcome + sure2_prop.decision + mag1_prop.decision + (1 | subjectID)")

model_list[["Outcome"]] <- as.formula("gamble ~ cv1 + prev_outcome + relief + regret + relief_inaction + regret_inaction + (1 | subjectID)")

model_list[["Full"]] <- as.formula("gamble ~ cv1 + prev_outcome + relief + regret + relief_inaction + regret_inaction + sure2_prop.decision + mag1_prop.decision + (1 | subjectID)")

# SUBJECTS THAT SHOULD BE EXCLUDED FOR NONCONVERGENCE
max_nonconvergent <- list()
max_nonconvergent[["Gain"]] <- c(60, 61, 66, 509, 512, 516, 530)
max_nonconvergent[["Loss"]] <- c(55, 66, 67, 512, 521)


# STAR MODEL - USE THESE SETTINGS TO GET DATA
model = 2
domain = "Gain"
decay_type = 3
	
out_file_data_name <- paste(c("../analysis/_analysis_emot/model2_", domain,"_decayType", decay_type, "_data.R"), sep="", collapse="")
load(out_file_data_name) # current_model_df
current_model_df <- subset(current_model_df, AgeGroup == "adult")

# SET PREVIOUS TRIAL OUTCOME TO FACTOR
current_model_df$prev_outcome <- factor(current_model_df$prev_outcome)
current_model_df$gamble <- as.numeric(as.character(current_model_df$gamble))
current_glm <- glmer(model_list[["Outcome"]], data=current_model_df, subset=convergence==0, family=binomial("logit"))
coeff <- current_glm@fixef





png(filename="~/Documents/Academics/Projects/EyeTrackingGaze2/analysis/prediction.png", height=4, width=4, units="in", res=900)
plot.new()


###### BASIC PLOTTING PARAMETERS
main_cex = 1.4

axis_cex=0.8
label_cex = 0.8

plot_cex = 1.5
point_linewidth = 0.5
linewidth=3

legend_cex = 1.2
legend_ptcex = plot_cex
legend_linewidth = 1

axis_mgp <- c(1.6, 0.5, 0) 	#mgp=c(3,1,0) # specifies lines at which label, axis values, are positioned
par(mgp=axis_mgp)

par(mar=c(2.6, 2.6, 1.6, 0.6)) # c(5.1, 4.1, 4.1, 2.1)



###### COEFFICIENT PARAMETERS FOR PREDICTION PLOTS

x <- seq(0, 1, 0.01)
prev_outcome1 = 1 # safe
prev_outcome2 = 0 # win
regret <- 0
relief <- 0
relief_inaction <- 0
regret_inaction <- 0



cv <- .7
curve(invlogit (cbind(1, cv, prev_outcome1, prev_outcome2, x, regret, relief_inaction, regret_inaction) %*% coeff), add=F, xaxt='n',
	col='black',
	ylim=c(0,1),
	main="Emotion's Effect on Risk Taking", ylab="Probability of Choosing Gamble", xlab="Emotion Value",
	cex=plot_cex, lwd=linewidth, mgp= axis_mgp, cex.axis = axis_cex, cex.main=main_cex, cex.lab=label_cex)
axis(1, at = c(0,0.5,1), labels=c("None","Medium","Full"), cex.axis=axis_cex)
curve(invlogit (cbind(1, cv, prev_outcome1, prev_outcome2, relief, x, relief_inaction, regret_inaction) %*% coeff), add=T, 
	col='blue', lwd=linewidth)
curve(invlogit (cbind(1, cv, prev_outcome1, prev_outcome2, relief, regret, relief_inaction, x) %*% coeff), add=T, 
	col='green3', lwd=linewidth)
legend(0.0, 0.28,
	title="Experienced Emotion", title.adj=0.15,
	c("Regret for not gambling", "Regret after losing", "Relief after winning"),
	lty=c(1,1,1), 
	cex=1,
	bty="n",
	col=c('green3','blue','black'))


text(0.32, 0.75, "30% increase", cex=legend_cex, col="black")
text(0.6, 0.55, "12% decrease", cex=legend_cex, col="black")
text(0.83, 0.395, "26% decrease", cex=legend_cex, col="black")


# CALCULATE DIFFERENCE IN PROBABILITY OF GAMBLING DUE TO EMOTION

x <- c(0,1)

delta_relief <- invlogit (cbind(1, cv, prev_outcome1, prev_outcome2, x, regret, relief_inaction, regret_inaction) %*% coeff)
delta_relief  <- 100*abs(delta_relief[2] - delta_relief[1])
delta_relief <- round(delta_relief, 1)

delta_regret <- invlogit (cbind(1, cv, prev_outcome1, prev_outcome2, relief, x, relief_inaction, regret_inaction) %*% coeff)
delta_regret <- 100*abs(delta_regret[2] - delta_regret[1])
delta_regret <- round(delta_regret, 1)

delta_regret_inaction <- invlogit (cbind(1, cv, prev_outcome1, prev_outcome2, relief, regret, relief_inaction, x) %*% coeff)
delta_regret_inaction <- 100*abs(delta_regret_inaction[2] - delta_regret_inaction[1])
delta_regret_inaction <- round(delta_regret_inaction, 1)

dev.off()

