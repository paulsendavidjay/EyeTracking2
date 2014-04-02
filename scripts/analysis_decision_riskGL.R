##########################################################################################
##########################################################################################
# RISK DATA


# NOTE: ALL FIGURES USE GROUP MEANS CONSTRUCTED FROM SUBJECT MEANS FIRST, INCLUDING THE FFO AND LFO
# CHI SQ STATISTICS FOR FFO AND LFO USE GROUP COUNTS, NOT MEANS - THESE ARE NOT WEIGHTED
# THIS SHOULD MAKE THE REGRESSION MORE CONSISTENT WITH THE FIGURE SINCE EACH SUBJECT CAN ONLY CONTRIBUTE
# AS MUCH TO THE REGRESSION AS THEY HAVE DATA FOR



library(RColorBrewer)
library(zoo)
library(arm)
library(MASS)
library(reshape)
library(ggplot2)
library(lme4)
source("~/R-stuff/functions/summarySE.R")

error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
	if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
	stop("vectors must be same length")
	arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}



# DEFINE COLORS AND TRANSPARENCY VALUES
risk_scale_loss <- brewer.pal(5,"OrRd")[c(2:5)]
risk_scale_gain <- brewer.pal(5,"PuBu")[c(2:5)]
risk_scale_grey <- brewer.pal(9,"Greys")[c(6:9)]

decision_gain_col <- c(brewer.pal(9,"RdYlBu")[c(3,7)], risk_scale_grey[1])
decision_loss_col <- c(brewer.pal(9,"RdYlBu")[c(1,9)], risk_scale_grey[1])
outcome_gain_col <- c(brewer.pal(9,"BrBG")[c(3,7)], risk_scale_grey[1])
outcome_loss_col <- c(brewer.pal(9,"BrBG")[c(1,9)], risk_scale_grey[1])

trans_value <- 20
decision_gain_col.trans <- apply(as.matrix(decision_gain_col), 1, function(ch) paste(c(ch, trans_value), sep="", collapse=""))
decision_loss_col.trans <- apply(as.matrix(decision_loss_col), 1, function(ch) paste(c(ch, trans_value), sep="", collapse=""))
outcome_gain_col.trans <- apply(as.matrix(outcome_gain_col), 1, function(ch) paste(c(ch, trans_value), sep="", collapse=""))
outcome_loss_col.trans <- apply(as.matrix(outcome_loss_col), 1, function(ch) paste(c(ch, trans_value), sep="", collapse=""))



study_dir <- c("/Users/Shared/EyeTrackingGaze", 
	"/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze")

	
if (file.exists(study_dir[1])) { 
	study_dir <- study_dir[1] 
} else if (file.exists(study_dir[2])) { 
	study_dir <- study_dir[2]
} else {
	return('study directory not found')
}



setwd("/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze/scripts")
smoothing_kernel <- kernel("daniell", 5) # smoothing kernel five time points



#source("./combine_beh_data.R") # yields 'cmbd_riskData', 'cmbd_riskData', cmbd_outcome_pupil[[1]] = raw, cmbd_outcome_pupil[[2]] = normalized by subject, cmbd_outcome_pupil[[3]] = counts , 'cmbd_outcome_pupil_trial_count', 'cmbd_outcome_pupil.norm.means'


cmbd_outcome_pupil_file_name <- paste(c(study_dir, "/data/processed_data/pupil_data_cmbd_outcome_pupil.R"), collapse="")
load(cmbd_outcome_pupil_file_name)

cmbd_riskData_file_name <- paste(c(study_dir, "/data/processed_data/beh_data_cmbd_riskData.R"), collapse="")
#save(cmbd_riskData, file=cmbd_riskData_file_name)
load(cmbd_riskData_file_name)



n_children = sum(as.numeric(levels(as.factor(cmbd_riskData$subjectID))) > 100) # 21
n_adults = sum(as.numeric(levels(as.factor(cmbd_riskData$subjectID))) < 100) # 17

rejection_thresh <- 0.75

# DECISION
# STANDARD BEHAVIOR GOOD SUBJECTS

# COUNT OF REJECTED TRIALS DUE TO NO FIXATIONS
cmbd_riskData.riskOnly <- subset(cmbd_riskData, trialType > 1 & trialType < 6)
missing_fixations.riskOnly <- subset(cmbd_riskData.riskOnly, mag1_samp.decision == 0 & mag2_samp.decision == 0 & sure2_samp.decision == 0)
nrow(missing_fixations.riskOnly) # 10


exclude_low_fix_count_trials <- subset(cmbd_riskData, mag1_samp.decision > 0 & mag2_samp.decision > 0 & sure2_samp.decision > 0 & trialType > 1 & trialType < 6)
good_fix.decision.table_cnt <- tapply(exclude_low_fix_count_trials$good_decision, 
	list(exclude_low_fix_count_trials$subjectID, exclude_low_fix_count_trials$gamble, exclude_low_fix_count_trials$condition), length)
# replace NA with 0
good_fix.decision.table_cnt[is.na(good_fix.decision.table_cnt)] <- 0

good_fix.decision.table_cnt.behav <- as.data.frame(good_fix.decision.table_cnt)
names(good_fix.decision.table_cnt.behav) <- c("Loss_Safe", "Loss_Gamble", "Gain_Safe", "Gain_Gamble")

nrow(good_fix.decision.table_cnt.behav) # 38, initial number of subjects
good_fix.decision.table_cnt.behav <- good_fix.decision.table_cnt.behav[good_fix.decision.table_cnt.behav[,1] > 2,] # more than 1 trials safe
good_fix.decision.table_cnt.behav <- good_fix.decision.table_cnt.behav[good_fix.decision.table_cnt.behav[,2] > 2,] # more than 1 trials gambled
good_fix.decision.table_cnt.behav <- good_fix.decision.table_cnt.behav[good_fix.decision.table_cnt.behav[,3] > 2,] # more than 1 trials safe
good_fix.decision.table_cnt.behav <- good_fix.decision.table_cnt.behav[good_fix.decision.table_cnt.behav[,4] > 2,] # more than 1 trials gambled
nrow(good_fix.decision.table_cnt.behav) # 35, number of subjects after exclusion

good_subjects.decision.behav <- rownames(good_fix.decision.table_cnt.behav) # 
good_subjects.decision.behav <- good_subjects.decision.behav[good_subjects.decision.behav != "534"] # 534 gambled 100% on Gain trials




# EYE-TRACKING & BEHAVIOR GOOD SUBJECTS
exclude_low_fix_count_trials <- subset(cmbd_riskData, mag1_samp.decision > 0 & mag2_samp.decision > 0 & sure2_samp.decision > 0 & trialType > 1 & trialType < 6)
bad_low_fix_count_trials.decision <- subset(exclude_low_fix_count_trials, good_decision <= rejection_thresh)
nrow(exclude_low_fix_count_trials) # 3609
exclude_low_fix_count_trials <- subset(exclude_low_fix_count_trials, good_decision > rejection_thresh)
nrow(exclude_low_fix_count_trials) # 3202

summarySE(bad_low_fix_count_trials.decision, measurevar="cv1", groupvars=c("AgeGroup","condition"))
  # AgeGroup condition   N       cv1        sd         se         ci
# 1    adult         0  26 0.8974817 0.4269750 0.08373670 0.17245895
# 2    adult         1  26 1.0198655 0.3650606 0.07159427 0.14745116
# 3    child         0 189 0.9502917 0.3846990 0.02798274 0.05520050
# 4    child         1 166 0.9413891 0.3824843 0.02968654 0.05861446


exclude_low_fix_count_trials <- subset(exclude_low_fix_count_trials, outlier == 0)

good_fix.decision.table_cnt <- tapply(exclude_low_fix_count_trials$good_decision, 
	list(exclude_low_fix_count_trials$subjectID, exclude_low_fix_count_trials$gamble), length)
# replace NA with 0
good_fix.decision.table_cnt[is.na(good_fix.decision.table_cnt)] <- 0
good_fix.decision.table_cnt <- as.data.frame(good_fix.decision.table_cnt)
good_fix.decision.table_cnt <- good_fix.decision.table_cnt[good_fix.decision.table_cnt[1] > 2,] # more than 2 trials safe
good_fix.decision.table_cnt <- good_fix.decision.table_cnt[good_fix.decision.table_cnt[2] > 2,] # more than 2 trials gambled

good_subjects.decision <- rownames(good_fix.decision.table_cnt) # 
good_subjects.decision <- good_subjects.decision[good_subjects.decision != "534"] # 534 gambled 100% on Gain trials

# ARRANGING DATA
subset_riskData.decision <- subset(cmbd_riskData, mag1_samp.decision > 0 & mag2_samp.decision > 0 & sure2_samp.decision > 0 & trialType > 1 & trialType < 6)
subset_riskData.decision.beh <- subset(subset_riskData.decision, is.element(subjectID, good_subjects.decision.behav))

subset_riskData.decision <- subset(subset_riskData.decision, outlier == 0)
subset_riskData.decision <- subset(subset_riskData.decision, good_decision > rejection_thresh)
subset_riskData.decision <- subset(subset_riskData.decision, is.element(subjectID, good_subjects.decision))


n_children.eye <- sum(as.numeric(as.character(good_subjects.decision)) > 100) # 
n_adults.eye <- sum(as.numeric(as.character(good_subjects.decision)) < 100) # 


# subset_riskData.decision.beh does not remove outliers or trials with < 0.75 proportion of samples
# subset_riskData.decision removes outliers and trials with < 0.75 proportion of samples

#################################################################################
# counts for number of outliers by subject and trialType
tapply(cmbd_riskData$outlier, list(cmbd_riskData$subjectID, cmbd_riskData$trialType), sum)
    # 1 2 3 4 5
# 51  2 1 0 0 2
# 52  0 2 2 4 2
# 54  1 2 1 2 2
# 55  0 0 1 0 1
# 56  1 1 1 1 2
# 57  1 3 1 3 3
# 58  0 1 0 1 1
# 59  0 0 0 0 0
# 60  1 3 1 2 2
# 61  2 4 2 4 6
# 62  3 3 0 2 1
# 63  0 3 3 0 1
# 64  0 1 1 1 0
# 65  0 4 1 2 1
# 66  0 1 0 2 3
# 67  0 0 0 0 0
# 68  1 1 2 0 2
# 502 2 3 4 5 4
# 505 0 1 0 0 0
# 507 2 4 2 3 4
# 508 1 1 1 1 3
# 509 0 2 1 3 2
# 511 1 2 0 3 3
# 512 0 1 0 2 3
# 516 1 4 5 6 3
# 517 0 3 0 1 3
# 518 1 2 1 0 0
# 519 1 6 1 1 2
# 520 0 0 0 0 2
# 521 0 1 0 0 1
# 523 2 4 5 2 6
# 525 0 2 0 1 2
# 526 0 1 1 3 1
# 527 0 1 0 0 0
# 529 0 2 2 2 0
# 530 1 2 2 0 0
# 534 1 0 1 1 2
# 535 1 0 1 1 3


# good_subjects.decision
 # [1] "51"  "52"  "54"  "55"  "56"  "57"  "58"  "59"  "60"  "61"  "62"  "63"  "64"  "65"  "66" 
# [16] "67"  "68"  "502" "505" "507" "508" "509" "511" "512" "516" "517" "518" "519" "520" "521"
# [31] "523" "525" "526" "527" "529" "530" "535"



subset_riskData.decision <- subset(subset_riskData.decision, is.element(subset_riskData.decision$subjectID, good_subjects.decision))


#save(subset_riskData.decision, file="../data/subset_riskData.decision.R")

load("../data/subset_riskData.decision.R")
summarySE(subset_riskData.decision, measurevar="cv1", groupvars=c("AgeGroup","condition"))
  # AgeGroup condition   N       cv1        sd         se         ci
# 1    adult         0 750 0.8819979 0.3930043 0.01435049 0.02817196
# 2    adult         1 733 0.8783366 0.3948675 0.01458476 0.02863296
# 3    child         0 752 0.8660177 0.3968249 0.01447072 0.02840786
# 4    child         1 738 0.8685532 0.3922400 0.01443855 0.02834559


##########################################################################################
# REGRESSIONS

if (F) {

sink(file = "/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze/analysis/analysis_decision_risk_regressions.txt", append=F, split=T)

date()

print("BEHAVIOR ONLY: GAINS")
glm_beh1 <- glmer( gamble ~ AgeGroup + cv1 + AgeGroup:cv1 + (cv1|subjectID), data=subset_riskData.decision, subset=condition==1, family=binomial('logit'))
glm_beh1

print("BEHAVIOR ONLY: LOSSES")
glm_beh2 <- glmer( gamble ~ AgeGroup + cv1 + AgeGroup:cv1 + (cv1|subjectID), data=subset_riskData.decision, subset=condition==0, family=binomial('logit'))
glm_beh2

print("ADULTS ONLY GAINS")
glmer( gamble ~ cv1 + (cv1|subjectID), data=subset_riskData.decision, subset=AgeGroup=='adult' & condition==1, family=binomial('logit'))

print("ADULTS ONLY LOSSES")
glmer( gamble ~ cv1 + (cv1|subjectID), data=subset_riskData.decision, subset=AgeGroup=='adult' & condition==0, family=binomial('logit'))

print("CHILDREN ONLY GAINS")
glmer( gamble ~ cv1 + (1|subjectID), data=subset_riskData.decision, subset=AgeGroup=='child' & condition==1, family=binomial('logit'))

print("CHILDREN ONLY LOSSES")
glmer( gamble ~ cv1 + (1|subjectID), data=subset_riskData.decision, subset=AgeGroup=='child' & condition==0, family=binomial('logit'))


print("Mag1 Reference")

glm_full_intxn1.loss <- glmer( gamble ~ ffo.decision + lfo.decision + sure2_prop.decision + mag2_prop.decision + cv1 + AgeGroup + ffo.decision:AgeGroup + lfo.decision:AgeGroup + sure2_prop.decision:AgeGroup + mag2_prop.decision:AgeGroup + cv1:AgeGroup + (cv1|subjectID), 
	data= subset_riskData.decision, subset=condition==0, family=binomial("logit") ) 

glm_full_intxn1.gain <- glmer( gamble ~ ffo.decision + lfo.decision + sure2_prop.decision + mag2_prop.decision + cv1 + AgeGroup + ffo.decision:AgeGroup + lfo.decision:AgeGroup + sure2_prop.decision:AgeGroup + mag2_prop.decision:AgeGroup + cv1:AgeGroup + (cv1|subjectID), 
	data= subset_riskData.decision, subset=condition==1, family=binomial("logit") ) 

glm_intxn1.loss <- glmer( gamble ~ lfo.decision + sure2_prop.decision + mag2_prop.decision + cv1 + AgeGroup + lfo.decision:AgeGroup + sure2_prop.decision:AgeGroup + mag2_prop.decision:AgeGroup + cv1:AgeGroup + (cv1|subjectID), 
	data= subset_riskData.decision, subset=condition==0, family=binomial("logit") ) 

glm_intxn1.gain <- glmer( gamble ~ lfo.decision + sure2_prop.decision + mag2_prop.decision + cv1 + AgeGroup + lfo.decision:AgeGroup + sure2_prop.decision:AgeGroup + mag2_prop.decision:AgeGroup + cv1:AgeGroup + (cv1|subjectID), 
	data= subset_riskData.decision, subset=condition==1, family=binomial("logit") ) 

print("NO FFO")
glm_intxn1.gain
glm_intxn1.loss

print("INCLUDES FFO")
glm_full_intxn1.gain
glm_full_intxn1.loss


glm_full_intxn2L <- glmer( gamble ~ ffo.decision + lfo.decision + mag1_prop.decision + mag2_prop.decision + cv1 + AgeGroup + ffo.decision:AgeGroup + lfo.decision:AgeGroup + mag1_prop.decision:AgeGroup + mag2_prop.decision:AgeGroup + cv1:AgeGroup + (cv1|subjectID), 
    data= subset_riskData.decision, subset = condition == 0, family=binomial("logit") ) 



glm_full_intxn2G <- glmer( gamble ~ ffo.decision + lfo.decision + mag1_prop.decision + mag2_prop.decision + cv1 + AgeGroup + ffo.decision:AgeGroup + lfo.decision:AgeGroup + mag1_prop.decision:AgeGroup + mag2_prop.decision:AgeGroup + cv1:AgeGroup + (cv1|subjectID), 
    data= subset_riskData.decision, subset = condition == 1, family=binomial("logit") ) 

#glm_full_intxn2L # 
#glm_full_intxn2G

sink()
}


# Fixed effects:
                                 # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      -1.10571    1.51147  -0.732 0.464448    
# ffo.decisionmag2                 -0.72531    0.52664  -1.377 0.168438    
# ffo.decisionsure2                -0.02179    0.46805  -0.047 0.962863    
# lfo.decisionmag2                 -0.04822    0.57053  -0.085 0.932646    
# lfo.decisionsure2                -2.71704    0.45655  -5.951 2.66e-09 ***
# mag1_prop.decision                4.84543    2.11469   2.291 0.021945 *  
# mag2_prop.decision                8.71074    2.54106   3.428 0.000608 ***
# cv1                              -2.27117    0.69937  -3.247 0.001164 ** 
# AgeGroupchild                    -1.54739    2.11641  -0.731 0.464693    
# ffo.decisionmag2:AgeGroupchild    1.48988    0.75767   1.966 0.049253 *  
# ffo.decisionsure2:AgeGroupchild   0.07838    0.65340   0.120 0.904523    
# lfo.decisionmag2:AgeGroupchild   -0.22437    0.90067  -0.249 0.803272    
# lfo.decisionsure2:AgeGroupchild   0.17072    0.61887   0.276 0.782664    
# mag1_prop.decision:AgeGroupchild  2.57599    2.91147   0.885 0.376278    
# mag2_prop.decision:AgeGroupchild -5.22178    3.36499  -1.552 0.120711    
# cv1:AgeGroupchild                 2.85960    0.92928   3.077 0.002089 ** 







################### t-tests

if (F) {
sink(file = "/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze/analysis/analysis_decision_risk_ttests.txt", split=T)


# bring relevant columns together into long format
subset_riskData.decision <- subset(subset_riskData.decision, good_decision > rejection_thresh) # subset data are not identical
ttest_data.frame.decision <- melt(subset_riskData.decision[, c("mag1_prop.decision","mag2_prop.decision","sure2_prop.decision","gamble","AgeGroup", "condition","subjectID")], id=c("subjectID", "AgeGroup", "gamble","condition"))
names(ttest_data.frame.decision) <- c("subjectID", "AgeGroup", "gamble", "condition", "object","prop")

ttest_data.frame.decision$gamble <- as.factor(as.character(ttest_data.frame.decision$gamble))
ttest_data.frame.decision$object  <- sub("_prop.decision", "", ttest_data.frame.decision$object) # cut out "_prop.decision"


# collapse into subject means
ttest_data.frame <- melt(tapply(ttest_data.frame.decision$prop, list(ttest_data.frame.decision$subjectID, ttest_data.frame.decision$object, ttest_data.frame.decision$condition , ttest_data.frame.decision$gamble), mean))
names(ttest_data.frame) <- c("subjectID", "object", "condition", "gamble", "prop")
ttest_data.frame$AgeGroup <- 'adult'; ttest_data.frame$AgeGroup[ttest_data.frame$subjectID > 100] <- 'child'



ttest_data.frameG <- subset(ttest_data.frame, condition=="gain_cond")
ttest_data.frameL <- subset(ttest_data.frame, condition=="loss_cond")
tapply(ttest_data.frameG$prop, list(ttest_data.frameG$object, ttest_data.frameG$gamble, ttest_data.frameG$AgeGroup), mean)
tapply(ttest_data.frameL$prop, list(ttest_data.frameL$object, ttest_data.frameL$gamble, ttest_data.frameL$AgeGroup), mean)


# GAINS
print("GAINS: ADULT VS CHILD GAMBLES")
t.test(ttest_data.frameG$prop[ttest_data.frameG$gamble == 1 & ttest_data.frameG$AgeGroup == 'adult' & ttest_data.frameG$object == 'mag1'],
	 ttest_data.frameG$prop[ttest_data.frameG$gamble == 1 & ttest_data.frameG$AgeGroup == 'child' & ttest_data.frameG$object == 'mag1'], paired=F)

t.test(ttest_data.frameG$prop[ttest_data.frameG$gamble == 1 & ttest_data.frameG$AgeGroup == 'adult' & ttest_data.frameG$object == 'mag2'],
	 ttest_data.frameG$prop[ttest_data.frameG$gamble == 1 & ttest_data.frameG$AgeGroup == 'child' & ttest_data.frameG$object == 'mag2'], paired=F)

t.test(ttest_data.frameG$prop[ttest_data.frameG$gamble == 1 & ttest_data.frameG$AgeGroup == 'adult' & ttest_data.frameG$object == 'sure2'],
	 ttest_data.frameG$prop[ttest_data.frameG$gamble == 1 & ttest_data.frameG$AgeGroup == 'child' & ttest_data.frameG$object == 'sure2'], paired=F)

print("GAINS: ADULT VS CHILD SURE BET")
t.test(ttest_data.frameG$prop[ttest_data.frameG$gamble == 0 & ttest_data.frameG$AgeGroup == 'adult' & ttest_data.frameG$object == 'mag1'],
	 ttest_data.frameG$prop[ttest_data.frameG$gamble == 0 & ttest_data.frameG$AgeGroup == 'child' & ttest_data.frameG$object == 'mag1'], paired=F)

t.test(ttest_data.frameG$prop[ttest_data.frameG$gamble == 0 & ttest_data.frameG$AgeGroup == 'adult' & ttest_data.frameG$object == 'mag2'],
	 ttest_data.frameG$prop[ttest_data.frameG$gamble == 0 & ttest_data.frameG$AgeGroup == 'child' & ttest_data.frameG$object == 'mag2'], paired=F)

t.test(ttest_data.frameG$prop[ttest_data.frameG$gamble == 0 & ttest_data.frameG$AgeGroup == 'adult' & ttest_data.frameG$object == 'sure2'],
	 ttest_data.frameG$prop[ttest_data.frameG$gamble == 0 & ttest_data.frameG$AgeGroup == 'child' & ttest_data.frameG$object == 'sure2'], paired=F)

# LOSSES
print("LOSSES: ADULT VS CHILD GAMBLES")
t.test(ttest_data.frameL$prop[ttest_data.frameL$gamble == 1 & ttest_data.frameL$AgeGroup == 'adult' & ttest_data.frameL$object == 'mag1'],
	 ttest_data.frameL$prop[ttest_data.frameL$gamble == 1 & ttest_data.frameL$AgeGroup == 'child' & ttest_data.frameL$object == 'mag1'], paired=F)

t.test(ttest_data.frameL$prop[ttest_data.frameL$gamble == 1 & ttest_data.frameL$AgeGroup == 'adult' & ttest_data.frameL$object == 'mag2'],
	 ttest_data.frameL$prop[ttest_data.frameL$gamble == 1 & ttest_data.frameL$AgeGroup == 'child' & ttest_data.frameL$object == 'mag2'], paired=F)

t.test(ttest_data.frameL$prop[ttest_data.frameL$gamble == 1 & ttest_data.frameL$AgeGroup == 'adult' & ttest_data.frameL$object == 'sure2'],
	 ttest_data.frameL$prop[ttest_data.frameL$gamble == 1 & ttest_data.frameL$AgeGroup == 'child' & ttest_data.frameL$object == 'sure2'], paired=F)

print("LOSSES: ADULT VS CHILD SURE BET")
t.test(ttest_data.frameL$prop[ttest_data.frameL$gamble == 0 & ttest_data.frameL$AgeGroup == 'adult' & ttest_data.frameL$object == 'mag1'],
	 ttest_data.frameL$prop[ttest_data.frameL$gamble == 0 & ttest_data.frameL$AgeGroup == 'child' & ttest_data.frameL$object == 'mag1'], paired=F)

t.test(ttest_data.frameL$prop[ttest_data.frameL$gamble == 0 & ttest_data.frameL$AgeGroup == 'adult' & ttest_data.frameL$object == 'mag2'],
	 ttest_data.frameL$prop[ttest_data.frameL$gamble == 0 & ttest_data.frameL$AgeGroup == 'child' & ttest_data.frameL$object == 'mag2'], paired=F)

t.test(ttest_data.frameL$prop[ttest_data.frameL$gamble == 0 & ttest_data.frameL$AgeGroup == 'adult' & ttest_data.frameL$object == 'sure2'],
	 ttest_data.frameL$prop[ttest_data.frameL$gamble == 0 & ttest_data.frameL$AgeGroup == 'child' & ttest_data.frameL$object == 'sure2'], paired=F)



attach(ttest_data.frame)
t.test(prop[gamble == 1 & AgeGroup == 'child' & object == 'mag1' & condition==1],
	 prop[gamble == 1 & AgeGroup == 'child' & object == 'mag1' & condition==0], paired=T)
detach(ttest_data.frame)






sink()
ttest_data.frameG


}

##########################################################################################
# CALCULATE MEAN AND SE OF BEHAVIOR


subset_riskData.decision$gamble <- as.numeric(subset_riskData.decision$gamble)

# SAME DATA THAT GOES INTO ANALYSIS OF EYE-TRACKING
# gambles were coded 1,2, convert to 0,1 if necessary
if (sum(subset_riskData.decision$gamble==2) > 0) {
	subset_riskData.decision$gamble <- as.numeric(subset_riskData.decision$gamble)-1
} else {
	subset_riskData.decision$gamble <- as.numeric(subset_riskData.decision$gamble)
}

# select Gain trials
risk_trials.tableG <- subset(subset_riskData.decision, condition==1)
# collect subject means
risk_trials.tableG <- as.data.frame(tapply(risk_trials.tableG$gamble, list(risk_trials.tableG$subjectID, risk_trials.tableG$trialType), mean))

risk_trials.tableG <- cbind(rownames(risk_trials.tableG), risk_trials.tableG)
risk_trials.data.frameG <- melt(risk_trials.tableG)
names(risk_trials.data.frameG) <- c("subjectID", "trialType", "gamble")
risk_trials.data.frameG$subjectID <- as.numeric(as.character(risk_trials.data.frameG$subjectID))

# set age groups
risk_trials.data.frameG$AgeGroup <- 0
risk_trials.data.frameG$AgeGroup[risk_trials.data.frameG$subjectID > 100] <- 1

n_children.behG <- sum(as.numeric(rownames(risk_trials.tableG)) > 100) # 20
n_adult.behG <- sum(as.numeric(rownames(risk_trials.tableG)) < 100) # 17

source("~/R-stuff/functions/summarySE.R")
summarySE(risk_trials.data.frameG, "gamble", groupvars=c("trialType","AgeGroup"))

# calculate group means
risk_trials.table.meanG <- as.data.frame(tapply(risk_trials.data.frameG$gamble, list(risk_trials.data.frameG$AgeGroup, risk_trials.data.frameG$trialType), mean))
# calculate group SD (for SE)
risk_trials.table.sdG <- as.data.frame(tapply(risk_trials.data.frameG$gamble, list(risk_trials.data.frameG$AgeGroup, risk_trials.data.frameG$trialType), sd))
# calculate group SE
risk_trials.table.seG <- rbind(risk_trials.table.sdG[1,] / sqrt(n_adult.behG), risk_trials.table.sdG[2,] / sqrt(n_children.behG) )








######################################################################################
#	PLOT BEHAVIORAL DATA

#	DEFINE COLORS
# FOR CNS POSTER & MANUSCRIPT
cv0col <-  rgb(186, 228, 179, maxColorValue=255)#  "#0xBAE4B3"
cv1col <-  rgb(116, 196, 118, maxColorValue=255)#  "#0x74C476"
cv2col <- rgb(49, 163, 84, maxColorValue=255)# "#0x31A354"
cv3col <- rgb(0, 109, 44, maxColorValue=255)#  "#0x006D2C"

#for single figures
#pdf("/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze/analysis/Fig_Exp2_beh_line_plot.pdf", height=2.5, width=5)
#pdf("/Users/dpaulsen/Dropbox/Research/SfN_2012/Beh_line_plot_gains.pdf", height=3, width=3)
pdf("~/Documents/Academics/Projects/EyeTrackingGaze/docs/manuscript/Figures/Fig1_beh.pdf", height=2.5, width=5)



	par(mar=c(2.6, 2.6, 2, 0.6)) # c(5.1, 4.1, 4.1, 2.1)
	
	par(mfrow=c(1,2))
	
	risk_col <- c(cv0col, cv1col, cv2col,cv3col)
	axis_cex=0.8
	label_cex = 0.8
	main_cex = 1
	plot_cex = 1
	axis_mgp <- c(1.5, 0.6, 0) 	#mgp=c(3,1,0) # specifies lines at which label, axis values, are positioned
	legend_cex = 0.7
	legend_ptcex = 0.8
	legend_linewidth = 1
	err.width = 0.025

	mtext_line = 1.1
	mtext_offset = 0.8	
	# atPos <- xrange[1] - mtext_offset*sd(xrange)	
	atPos <- 1 - mtext_offset*sd(c(1,2,3,4))

	
	plot(c(1,2,3,4), risk_trials.table.meanG[1,], type='l', lty='solid', lwd=2, xlim=c(0.8,4.2),ylim=c(0,1),xaxt='n',xlab="CV",ylab="Prop. Chose Gamble", main="Age Group by CV: Gain", mgp= axis_mgp, cex.axis = axis_cex, cex.main=main_cex, cex.lab=label_cex, font.main=1)
	axis(1, at = c(1,2,3,4), labels = c("0.35","0.71","1.06","1.41"), mgp=axis_mgp, cex.axis=axis_cex, cex.lab=label_cex)
	points(c(1,2,3,4), risk_trials.table.meanG[1,], pch=21, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(risk_trials.table.meanG[1,] + risk_trials.table.seG[1,]), c(1,2,3,4), as.matrix(risk_trials.table.meanG[1,] - risk_trials.table.seG[1,]), angle=90, code=3, length= err.width)
	
	
	lines(c(1,2,3,4), risk_trials.table.meanG[2,],lty="dotted",lwd=2)
	points(c(1,2,3,4), risk_trials.table.meanG[2,], pch=22, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(risk_trials.table.meanG[2,] + risk_trials.table.seG[2,]), c(1,2,3,4), as.matrix(risk_trials.table.meanG[2,] - risk_trials.table.seG[2,]), angle=90, code=3, length= err.width)	
	
	
		legend(0.8, 0.25, 
		c("Adults", "Children"),
		lty=c('solid','dotted'), 
		cex= legend_cex,
		pt.cex=legend_ptcex,
		bty="n",
		col=c("black", "black"), 
		pch=c(21, 22),
		pt.bg=c(cv1col, cv1col),
		lwd=c(legend_linewidth, legend_linewidth))	
	box()


mtext("A", side=3, line=mtext_line, at=atPos, cex=1)

#dev.off()


#pdf("/Users/dpaulsen/Dropbox/Research/SfN_2012/Beh_line_plot_losses.pdf", height=3, width=3)


#par(mar=c(2.6, 2.6, 2, 0.6)) # c(5.1, 4.1, 4.1, 2.1)



# DATA THAT DOES NOT EXCLUDE BASED ON RT OUTLIERS OR GREATER THAN 3 TRIALS PER CHOICE (NEED ONLY 1 TRIAL)
#risk_trials.tableL <- as.data.frame(tapply(subset_riskData.decision.behL$gamble, list(subset_riskData.decision.behL$subjectID, subset_riskData.decision.behL$trialType), mean))
# DATA THAT EXCLUDES BASED ON RT OUTLIERS AND GREATER THAN 3 TRIALS PER CHOICE (NEED ONLY 1 TRIAL)
risk_trials.tableL <- subset(subset_riskData.decision, condition==0)
risk_trials.tableL <- as.data.frame(tapply(risk_trials.tableL$gamble, list(risk_trials.tableL$subjectID, risk_trials.tableL$trialType), mean))



risk_trials.tableL <- cbind(rownames(risk_trials.tableL), risk_trials.tableL)
risk_trials.data.frameL <- melt(risk_trials.tableL)
names(risk_trials.data.frameL) <- c("subjectID", "trialType", "gamble")
risk_trials.data.frameL$subjectID <- as.numeric(as.character(risk_trials.data.frameL$subjectID))

risk_trials.data.frameL$AgeGroup <- 0
risk_trials.data.frameL$AgeGroup[risk_trials.data.frameL$subjectID > 100] <- 1

n_children.behL <- sum(as.numeric(rownames(risk_trials.tableL)) > 100) # 16
n_adult.behL <- sum(as.numeric(rownames(risk_trials.tableL)) < 100) # 14


risk_trials.table.meanL <- as.data.frame(tapply(risk_trials.data.frameL$gamble, list(risk_trials.data.frameL$AgeGroup, risk_trials.data.frameL$trialType), mean))


risk_trials.table.sdL <- as.data.frame(tapply(risk_trials.data.frameL$gamble, list(risk_trials.data.frameL$AgeGroup, risk_trials.data.frameL$trialType), sd))
risk_trials.table.seL <- rbind(risk_trials.table.sdL[1,] / sqrt(n_adult.behL), risk_trials.table.sdL[2,] / sqrt(n_children.behL) )


# USED WHEN MAKING SEPARATE PDF PLOTS
#	par(mar=c(2.6, 2.6, 2, 0.6)) # c(5.1, 4.1, 4.1, 2.1)

	# risk_col <- c(cv0col, cv1col, cv2col,cv3col)
	# axis_cex=0.8
	# label_cex = 0.8
	# main_cex = 1
	# plot_cex = 1
	# axis_mgp <- c(1.5, 0.6, 0) 	#mgp=c(3,1,0) # specifies lines at which label, axis values, are positioned
	# legend_cex = 0.7
	# legend_ptcex = 0.8
	# legend_linewidth = 1
	# err.width = 0.025
	
	plot(c(1,2,3,4), risk_trials.table.meanL[1,], type='l', lty='solid', lwd=2, xlim=c(0.8,4.2),ylim=c(0,1),xaxt='n',xlab="CV",ylab="Prop. Chose Gamble", main="Age Group by CV: Loss", mgp= axis_mgp, cex.axis = axis_cex, cex.main=main_cex, cex.lab=label_cex, font.main=1)
	axis(1, at = c(1,2,3,4), labels = c("0.35","0.71","1.06","1.41"), mgp=axis_mgp, cex.axis=axis_cex, cex.lab=label_cex)
	points(c(1,2,3,4), risk_trials.table.meanL[1,], pch=21, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(risk_trials.table.meanL[1,] + risk_trials.table.seL[1,]), c(1,2,3,4), as.matrix(risk_trials.table.meanL[1,] - risk_trials.table.seL[1,]), angle=90, code=3, length= err.width)
	
	
	lines(c(1,2,3,4), risk_trials.table.meanL[2,],lty="dotted",lwd=2)
	points(c(1,2,3,4), risk_trials.table.meanL[2,], pch=22, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(risk_trials.table.meanL[2,] + risk_trials.table.seL[2,]), c(1,2,3,4), as.matrix(risk_trials.table.meanL[2,] - risk_trials.table.seL[2,]), angle=90, code=3, length= err.width)	
	
	
		legend(0.8, 0.25, 
		c("Adults", "Children"),
		lty=c('solid','dotted'), 
		cex= legend_cex,
		pt.cex=legend_ptcex,
		bty="n",
		col=c("black", "black"), 
		pch=c(21, 22),
		pt.bg=c(cv1col, cv1col),
		lwd=c(legend_linewidth, legend_linewidth))	
	box()


mtext("B", side=3, line=mtext_line, at=atPos, cex=1)



dev.off()



pdf("~/Documents/Academics/Projects/EyeTrackingGaze/docs/manuscript/Figures/Fig1_beh_adults.pdf", height=2.5, width=2.5)

	par(mar=c(2.6, 2.6, 2, 0.6)) # c(5.1, 4.1, 4.1, 2.1)

	risk_col <- c(cv0col, cv1col, cv2col,cv3col)
	axis_cex=0.8
	label_cex = 0.8
	main_cex = 1
	plot_cex = 1
	axis_mgp <- c(1.5, 0.6, 0) 	#mgp=c(3,1,0) # specifies lines at which label, axis values, are positioned
	legend_cex = 0.7
	legend_ptcex = 0.8
	legend_linewidth = 1
	err.width = 0.025

	mtext_line = 1.1
	mtext_offset = 0.8	
	# atPos <- xrange[1] - mtext_offset*sd(xrange)	
	atPos <- 1 - mtext_offset*sd(c(1,2,3,4))
	
	plot(c(1,2,3,4), risk_trials.table.meanG[1,], type='l', lty='solid', lwd=2, xlim=c(0.8,4.2),ylim=c(0,1),xaxt='n',xlab="CV",ylab="Prop. Chose Gamble", main="Gain vs Loss by CV", mgp= axis_mgp, cex.axis = axis_cex, cex.main=main_cex, cex.lab=label_cex, font.main=1)
	axis(1, at = c(1,2,3,4), labels = c("0.35","0.71","1.06","1.41"), mgp=axis_mgp, cex.axis=axis_cex, cex.lab=label_cex)
	points(c(1,2,3,4), risk_trials.table.meanG[1,], pch=21, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(risk_trials.table.meanG[1,] + risk_trials.table.seG[1,]), c(1,2,3,4), as.matrix(risk_trials.table.meanG[1,] - risk_trials.table.seG[1,]), angle=90, code=3, length= err.width)
	
	
	lines(c(1,2,3,4), risk_trials.table.meanL[1,],lty="dotted",lwd=2)
	points(c(1,2,3,4), risk_trials.table.meanL[1,], pch=22, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(risk_trials.table.meanL[1,] + risk_trials.table.seL[2,]), c(1,2,3,4), as.matrix(risk_trials.table.meanL[1,] - risk_trials.table.seL[1,]), angle=90, code=3, length= err.width)	
	
		legend(0.8, 0.25, 
		c("Gain", "Loss"),
		lty=c('solid','dotted'), 
		cex= legend_cex,
		pt.cex=legend_ptcex,
		bty="n",
		col=c("black", "black"), 
		pch=c(21, 22),
		pt.bg=c(cv1col, cv1col),
		lwd=c(legend_linewidth, legend_linewidth))	
	box()

dev.off()













pdf("~/Documents/Academics/Projects/EyeTrackingGaze/docs/manuscript/Figures/Fig1_beh_w_rflct.pdf", height=2, width=5)



	par(mar=c(2.6, 2.6, 2, 0.6)) # c(5.1, 4.1, 4.1, 2.1)
	
	par(mfrow=c(1,3))
	
	risk_col <- c(cv0col, cv1col, cv2col,cv3col)
	axis_cex=0.8
	label_cex = 0.8
	main_cex = 1
	plot_cex = 1
	axis_mgp <- c(1.5, 0.6, 0) 	#mgp=c(3,1,0) # specifies lines at which label, axis values, are positioned
	legend_cex = 0.7
	legend_ptcex = 0.8
	legend_linewidth = 1
	err.width = 0.025

	mtext_line = 0.8
	mtext_offset = 0.8
	# atPos <- xrange[1] - mtext_offset*sd(xrange)	
	atPos <- 1 - mtext_offset*sd(c(1,2,3,4))

	
	plot(c(1,2,3,4), risk_trials.table.meanG[1,], type='l', lty='solid', lwd=2, xlim=c(0.8,4.2),ylim=c(0,1),xaxt='n',xlab="CV",ylab="Prop. Chose Gamble", main="Age Group by CV: Gain", mgp= axis_mgp, cex.axis = axis_cex, cex.main=main_cex, cex.lab=label_cex, font.main=1)
	axis(1, at = c(1,2,3,4), labels = c("0.35","0.71","1.06","1.41"), mgp=axis_mgp, cex.axis=axis_cex, cex.lab=label_cex)
	points(c(1,2,3,4), risk_trials.table.meanG[1,], pch=21, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(risk_trials.table.meanG[1,] + risk_trials.table.seG[1,]), c(1,2,3,4), as.matrix(risk_trials.table.meanG[1,] - risk_trials.table.seG[1,]), angle=90, code=3, length= err.width)
	
	
	lines(c(1,2,3,4), risk_trials.table.meanG[2,],lty="dotted",lwd=2)
	points(c(1,2,3,4), risk_trials.table.meanG[2,], pch=22, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(risk_trials.table.meanG[2,] + risk_trials.table.seG[2,]), c(1,2,3,4), as.matrix(risk_trials.table.meanG[2,] - risk_trials.table.seG[2,]), angle=90, code=3, length= err.width)	
	
	
		legend(0.8, 0.2, 
		c("Adults", "Children"),
		lty=c('solid','dotted'), 
		cex= legend_cex,
		pt.cex=legend_ptcex,
		bty="n",
		col=c("black", "black"), 
		pch=c(21, 22),
		pt.bg=c(cv1col, cv1col),
		lwd=c(legend_linewidth, legend_linewidth))	
	box()


mtext("A", side=3, line=mtext_line, at=atPos, cex=1)

#dev.off()


#pdf("/Users/dpaulsen/Dropbox/Research/SfN_2012/Beh_line_plot_losses.pdf", height=3, width=3)


#par(mar=c(2.6, 2.6, 2, 0.6)) # c(5.1, 4.1, 4.1, 2.1)



# DATA THAT DOES NOT EXCLUDE BASED ON RT OUTLIERS OR GREATER THAN 3 TRIALS PER CHOICE (NEED ONLY 1 TRIAL)
#risk_trials.tableL <- as.data.frame(tapply(subset_riskData.decision.behL$gamble, list(subset_riskData.decision.behL$subjectID, subset_riskData.decision.behL$trialType), mean))
# DATA THAT EXCLUDES BASED ON RT OUTLIERS AND GREATER THAN 3 TRIALS PER CHOICE (NEED ONLY 1 TRIAL)
risk_trials.tableL <- subset(subset_riskData.decision, condition==0)
risk_trials.tableL <- as.data.frame(tapply(risk_trials.tableL$gamble, list(risk_trials.tableL$subjectID, risk_trials.tableL$trialType), mean))



risk_trials.tableL <- cbind(rownames(risk_trials.tableL), risk_trials.tableL)
risk_trials.data.frameL <- melt(risk_trials.tableL)
names(risk_trials.data.frameL) <- c("subjectID", "trialType", "gamble")
risk_trials.data.frameL$subjectID <- as.numeric(as.character(risk_trials.data.frameL$subjectID))

risk_trials.data.frameL$AgeGroup <- 0
risk_trials.data.frameL$AgeGroup[risk_trials.data.frameL$subjectID > 100] <- 1

n_children.behL <- sum(as.numeric(rownames(risk_trials.tableL)) > 100) # 16
n_adult.behL <- sum(as.numeric(rownames(risk_trials.tableL)) < 100) # 14


risk_trials.table.meanL <- as.data.frame(tapply(risk_trials.data.frameL$gamble, list(risk_trials.data.frameL$AgeGroup, risk_trials.data.frameL$trialType), mean))


risk_trials.table.sdL <- as.data.frame(tapply(risk_trials.data.frameL$gamble, list(risk_trials.data.frameL$AgeGroup, risk_trials.data.frameL$trialType), sd))
risk_trials.table.seL <- rbind(risk_trials.table.sdL[1,] / sqrt(n_adult.behL), risk_trials.table.sdL[2,] / sqrt(n_children.behL) )

	
	plot(c(1,2,3,4), risk_trials.table.meanL[1,], type='l', lty='solid', lwd=2, xlim=c(0.8,4.2),ylim=c(0,1),xaxt='n',xlab="CV",ylab="Prop. Chose Gamble", main="Age Group by CV: Loss", mgp= axis_mgp, cex.axis = axis_cex, cex.main=main_cex, cex.lab=label_cex, font.main=1)
	axis(1, at = c(1,2,3,4), labels = c("0.35","0.71","1.06","1.41"), mgp=axis_mgp, cex.axis=axis_cex, cex.lab=label_cex)
	points(c(1,2,3,4), risk_trials.table.meanL[1,], pch=21, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(risk_trials.table.meanL[1,] + risk_trials.table.seL[1,]), c(1,2,3,4), as.matrix(risk_trials.table.meanL[1,] - risk_trials.table.seL[1,]), angle=90, code=3, length= err.width)
	
	
	lines(c(1,2,3,4), risk_trials.table.meanL[2,],lty="dotted",lwd=2)
	points(c(1,2,3,4), risk_trials.table.meanL[2,], pch=22, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(risk_trials.table.meanL[2,] + risk_trials.table.seL[2,]), c(1,2,3,4), as.matrix(risk_trials.table.meanL[2,] - risk_trials.table.seL[2,]), angle=90, code=3, length= err.width)	
	
	
		legend(0.8, 0.2, 
		c("Adults", "Children"),
		lty=c('solid','dotted'), 
		cex= legend_cex,
		pt.cex=legend_ptcex,
		bty="n",
		col=c("black", "black"), 
		pch=c(21, 22),
		pt.bg=c(cv1col, cv1col),
		lwd=c(legend_linewidth, legend_linewidth))	
	box()


mtext("B", side=3, line=mtext_line, at=atPos, cex=1)

	source("analysis_decision_riskGL_reflection_by_cv.R")



	axis_mgp <- c(1.25,0.6,0)
	par(mar=c(2.6, 3, 2, 0.2)) # c(5.1, 4.1, 4.1, 2.1)


	plot(c(1,2,3,4), subset(reflection_table, AgeGroup=="adult")$diff, type='l', lty='solid', lwd=2, xlim=c(0.8,4.2), ylim=c(-0.5,0.5),xaxt='n',xlab="CV",ylab="Prop. Chose Gamble\n(Gain-Loss)", main="Age Group by CV:\nReflection Effect", mgp= axis_mgp, cex.axis = axis_cex, cex.main=main_cex, cex.lab=label_cex, font.main=1)
	axis(1, at = c(1,2,3,4), labels = c("0.35","0.71","1.06","1.41"), mgp=axis_mgp, cex.axis=axis_cex, cex.lab=label_cex)
	points(c(1,2,3,4), subset(reflection_table, AgeGroup=="adult")$diff, pch=21, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(subset(reflection_table, AgeGroup=="adult")$diff + subset(reflection_table, AgeGroup=="adult")$se), c(1,2,3,4), as.matrix(subset(reflection_table, AgeGroup=="adult")$diff - subset(reflection_table, AgeGroup=="adult")$se), angle=90, code=3, length= err.width)
	
	
	lines(c(1,2,3,4), subset(reflection_table, AgeGroup=="child")$diff, lty="dotted",lwd=2)
	points(c(1,2,3,4), subset(reflection_table, AgeGroup=="child")$diff, pch=22, cex= plot_cex, col="black", bg=risk_col)
	arrows(c(1,2,3,4), as.matrix(subset(reflection_table, AgeGroup=="child")$diff + subset(reflection_table, AgeGroup=="child")$se), c(1,2,3,4), as.matrix(subset(reflection_table, AgeGroup=="child")$diff - subset(reflection_table, AgeGroup=="child")$se), angle=90, code=3, length= err.width)	
	abline(h=0, lty="dotted")


		legend(0.8, -0.3, 
		c("Adults", "Children"),
		lty=c('solid','dotted'), 
		cex= legend_cex,
		pt.cex=legend_ptcex,
		bty="n",
		col=c("black", "black"), 
		pch=c(21, 22),
		pt.bg=c(cv1col, cv1col),
		lwd=c(legend_linewidth, legend_linewidth))	
	box()


	mtext("C", side=3, line=mtext_line, at=atPos, cex=1)




dev.off()






















########################################################################################################
# 							FIXATION DATA DURING DECISION EPOCH
########################################################################################################


######################################################################################
# DWELL TIME DATA
# GET MEANS BY SUBJECT
decision_prop.frame <- melt(subset_riskData.decision[, c("mag1_prop.decision",  "mag2_prop.decision" , "sure2_prop.decision", "gamble", "condition","AgeGroup", "subjectID")], 
	id=c("AgeGroup", "gamble", "condition", "subjectID"))
decision_prop.table <- tapply(decision_prop.frame$value, list(decision_prop.frame$subjectID, decision_prop.frame$variable, decision_prop.frame$gamble, decision_prop.frame$condition), mean)

# CONVERT TO DATAFRAME
decision_prop.table <- melt(decision_prop.table)
names(decision_prop.table) <- c("subjectID","object","gamble","condition","prop")
# MAKE SURE THAT 99 (SURE) GAMBLES AREN'T INCLUDED.
decision_prop.table <- subset(decision_prop.table, gamble < 2)

# REMOVE TEXT FROM OBJECT VARIABLE
decision_prop.table$object  <- sub("_prop.decision", "", decision_prop.table$object) # cut out "_prop.decision"

# RECREATE GROUP VARIABLE
decision_prop.table$AgeGroup <- "adult"
decision_prop.table$AgeGroup[decision_prop.table$subjectID > 100] <- "child"

n_children <- sum(unique(decision_prop.table$subjectID) > 100) # 20
n_adult <- sum(unique(decision_prop.table$subjectID) < 100) # 17

# GET MEANS BY GROUP
decision_prop.tableMN <- tapply(decision_prop.table$prop, list(decision_prop.table$AgeGroup, decision_prop.table$object, decision_prop.table$gamble, decision_prop.table$condition), mean)
# CONVERT TO DATAFRAME
decision_prop.tableMN <- melt(decision_prop.tableMN)
names(decision_prop.tableMN) <- c("group","object","gamble","condition","prop")


# CONVERT TO FACTORS AND RELABEL
decision_prop.tableMN$gamble <- factor(decision_prop.tableMN$gamble, levels=c(0,1), labels=c("sure","gamble"))
decision_prop.tableMN$object <- factor(decision_prop.tableMN$object, levels=c("mag1","mag2","sure2"), labels=c("Lg","Sm","Sure"))
decision_prop.tableMN$condition <- factor(decision_prop.tableMN$condition, levels=c(0,1), labels=c("Loss","Gain"))




# GET SD BY GROUP
decision_prop.tableSDSE <- tapply(decision_prop.table$prop, list(decision_prop.table$AgeGroup, decision_prop.table$object, decision_prop.table$gamble, decision_prop.table$condition), sd)
# CONVERT TO DATAFRAME
decision_prop.tableSDSE <- melt(decision_prop.tableSDSE)
names(decision_prop.tableSDSE) <- c("group","object","gamble","condition","propSD")

# ADULT SE
decision_prop.tableSDSE$propSE <- decision_prop.tableSDSE$propSD / sqrt(n_adult)
# CHILD SE
decision_prop.tableSDSE$propSE[decision_prop.tableSDSE$group == 1] <- decision_prop.tableSDSE$propSD[decision_prop.tableSDSE$group == 1] / sqrt(n_children)





######################################################################################
# FIRST AND LAST FIXATION OBJECTS



# collect data
table_data_sure <- subset(subset_riskData.decision, gamble==0)
table_data_gamble <- subset(subset_riskData.decision, gamble==1)

# NUMBER OF FIRST FIXATIONS BY OBJECT, SEPARATELY BY GAMBLE VS. SURE DECISIONS
# ffo_table.sure <- tapply(table_data_sure$AgeGroup, list(table_data_sure$AgeGroup, table_data_sure$ffo.decision, table_data_sure$condition), length)
# ffo_table.gamble <- tapply(table_data_gamble$AgeGroup, list(table_data_gamble$AgeGroup, table_data_gamble$ffo.decision, table_data_gamble$condition), length)
# # RENAME FOR EASY FIGURE LABELING
# rownames(ffo_table.sure) <- c("adult\nsafe","child\nsafe")
# rownames(ffo_table.gamble) <- c("adult\ngamble","child\ngamble")

subset_riskData.decision$condition <- factor( subset_riskData.decision$condition,
	levels =  c(0,1),
	labels = c("loss_cond","gain_cond"))

subset_riskData.decision$topRight <- factor( subset_riskData.decision$topRight,
	levels =  c(0,1),
	labels = c("top_left","top_right"))

subset_riskData.decision$vert <- factor( subset_riskData.decision$vert,
	levels =  c(0,1),
	labels = c("gamble_bottom","gamble_top"))

subset_riskData.decision$sideMag <- factor( subset_riskData.decision$sideMag,
	levels =  c(0,1),
	labels = c("large_right","large_left"))


# NUMBER OF LAST FIXATIONS BY OBJECT, SEPARATELY BY GAMBLE VS. SURE DECISIONS
lfo_table.sure <- tapply(table_data_sure$AgeGroup, list(table_data_sure$AgeGroup, table_data_sure$lfo.decision, table_data_sure$condition), length)
lfo_table.gamble <- tapply(table_data_gamble$AgeGroup, list(table_data_gamble$AgeGroup, table_data_gamble$lfo.decision, table_data_gamble$condition), length)
# RENAME FOR EASY FIGURE LABELING
rownames(lfo_table.sure) <- c("adult\nsafe","child\nsafe")
rownames(lfo_table.gamble) <- c("adult\ngamble","child\ngamble")

lfo_table.sure <- lfo_table.sure[,c(2,3,5),] 
lfo_table.gamble <- lfo_table.gamble[,c(2,3,5),] 

lfo_table.sure[1,,1] <- lfo_table.sure[1,,1] / sum(lfo_table.sure[1,,1])
lfo_table.sure[2,,1] <- lfo_table.sure[2,,1] / sum(lfo_table.sure[2,,1])
lfo_table.sure[1,,2] <- lfo_table.sure[1,,2] / sum(lfo_table.sure[1,,2])
lfo_table.sure[2,,2] <- lfo_table.sure[2,,2] / sum(lfo_table.sure[2,,2])

lfo_table.gamble[1,,1] <- lfo_table.gamble[1,,1] / sum(lfo_table.gamble[1,,1])
lfo_table.gamble[2,,1] <- lfo_table.gamble[2,,1] / sum(lfo_table.gamble[2,,1])
lfo_table.gamble[1,,2] <- lfo_table.gamble[1,,2] / sum(lfo_table.gamble[1,,2])
lfo_table.gamble[2,,2] <- lfo_table.gamble[2,,2] / sum(lfo_table.gamble[2,,2])




##############################################################################
# 	FFO FOR GAIN AND LOSS CONDITIONS - COMBINED CHOICES

# NUMBER OF FIRST FIXATIONS BY OBJECT, GAMBLE VS. SURE DECISIONS TOGETHER
ffo.table <- tapply(subset_riskData.decision$AgeGroup, list(subset_riskData.decision$AgeGroup, subset_riskData.decision$ffo.decision, subset_riskData.decision$condition), length)
rownames(ffo.table) <- c("adult","child")

ffo.table.gain <- ffo.table[,c(2,3,5),2]
ffo.table.loss <- ffo.table[,c(2,3,5),1]



ffo.table.vert.sideMag <- tapply(subset_riskData.decision$vert, list(subset_riskData.decision$vert, subset_riskData.decision$sideMag , subset_riskData.decision$AgeGroup, subset_riskData.decision$condition), length)


sink(file = "/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze/analysis/analysis_decision_risk_chisq.txt", append=F, split=T)

date()

cat("\n\n\n
   BECAUSE LUMINANCE WAS CONTROLLED FOR DURING PRESENTATION OF BOXES, WE CAN BE SURE THAT VISUAL PREFERENCE IS NOT DUE TO THAT. BECAUSE THE SURE BET BAR TAKES UP ONE HALF OF THE SPACE ALLOTTED TO THE WIN LOSE AND SURE BET BARS, WE EXPECT THERE TO BE ABOUT A 50% CHANCE OF LOOKING DOWN OR UP, WITH PERHAPS A BIAS TOWARD THE UPPER VISUAL FIELD. WE EXPECT A 50% CHANCE OF LOOKING TO EITHER RIGHT OR LEFT HEMIFIELD, AND ASSUME THAT VISUAL PREFERENCES BEYOND THESE RATIOS INDICATE SEEKING BEHAVIOR FOR DECISION-RELATED INFORMATION. 
\n\n")


cat("\n\n\n
Adults and children looked more often to sure bet first (13.5% and 8.7% change, respectively), but adults looked disproportionately more often first to the large magnitude object than to the smaller magnitude object with a -5.6% vs -11.4% change, respectively.
")
ffo.table.gain[5:6,] <- ffo.table.gain[5:6,] - 1
ffo.table.gain
                  # mag1        mag2      sure2
# adult      173.0000000 144.0000000 416.000000
# child      168.0000000 169.0000000 401.000000
# adult_exp  183.2500000 183.2500000 366.500000
# child_exp  184.5000000 184.5000000 369.000000
# adult_pcnt  -0.05593452  -0.21418827   0.13506139
# child_pcnt  -0.08943089  -0.08401084   0.08672087


ffo.table.gain <- rbind(ffo.table.gain,
	chisq.test(ffo.table.gain[1,], p = c(25,25,50), rescale.p=T)$expected,
	chisq.test(ffo.table.gain[2,], p = c(25,25,50), rescale.p=T)$expected)
row.names(ffo.table.gain) <- c("adult", "child", "adult_exp", "child_exp")
prcnt_change.gain <- ffo.table.gain[1:2,]/ffo.table.gain[3:4,]
row.names(prcnt_change.gain) <- c("adult_pcnt", "child_pcnt")
ffo.table.gain
prcnt_change.gain
ffo.table.gain <- rbind(ffo.table.gain, prcnt_change.gain)



ffo.table.loss <- rbind(ffo.table.loss,
	chisq.test(ffo.table.loss[1,], p = c(25,25,50), rescale.p=T)$expected,
	chisq.test(ffo.table.loss[2,], p = c(25,25,50), rescale.p=T)$expected)
row.names(ffo.table.loss) <- c("adult", "child", "adult_exp", "child_exp")

prcnt_change.loss <- ffo.table.loss[1:2,]/ffo.table.loss[3:4,]
row.names(prcnt_change.loss) <- c("adult_pcnt", "child_pcnt")
ffo.table.loss <- rbind(ffo.table.loss, prcnt_change.loss)

cat("\n\n\n
Adults and children looked again more often to sure bet first (10.6%% and 9.3% change, respectively). Adults looked disproportionately more often first to the small (winning) magnitude object than to the larger (losing) magnitude object with only a -7.7% vs -13.6% change, respectively.")


ffo.table.loss[5:6,] <- ffo.table.loss[5:6,] - 1
ffo.table.loss 
                 # mag1         mag2        sure2
# adult      162.000000 173.00000000 415.00000000
# child      168.000000 173.00000000 411.00000000
# adult_exp  187.500000 187.50000000 375.00000000
# child_exp  188.000000 188.00000000 376.00000000
# adult_pcnt  -0.136000  -0.07733333   0.10666667
# child_pcnt  -0.106383  -0.07978723   0.09308511



cat("\n\n\n
   SHOWS THAT THERE ARE NO SIGNIFICANT DIFFERENCES IN FFO BETWEEN GROUPS
\n\n")
chisq.test(ffo.table.gain)
# data:  ffo.table.gain
# X-squared = 2.3285, df = 2, p-value = 0.3121
chisq.test(ffo.table.loss)
# data:  ffo.table.loss
# X-squared = 0.1258, df = 2, p-value = 0.939





cat("ADULT CHI SQUARE FOR GAINS: 25%, 25%, 50%")
chisq.test(ffo.table.gain[1,], p = c(25,25,50), rescale.p=T)
# X-squared = 15.6658, df = 2, 
# p-value = 0.0003965
chisq.test(ffo.table.gain[1,1:2], p = c(25,25), rescale.p=T)
# X-squared = 2.653, df = 1, p-value = 0.1034

cat("
CHILD CHI SQUARE FOR GAINS: 25%, 25%, 50%
")
chisq.test(ffo.table.gain[2,], p = c(25,25,50), rescale.p=T)
# X-squared = 5.5528, df = 2, 
# p-value = 0.06226
chisq.test(ffo.table.gain[2,1:2], p = c(50,50), rescale.p=T)
# X-squared = 0.003, df = 1, p-value = 0.9566

cat("\n
ADULT CHI SQUARE FOR LOSSES: 25%, 25%, 50%
")
chisq.test(ffo.table.loss[1,], p = c(25,25,50), rescale.p=T)
# X-squared = 8.856, df = 2, 
# p-value = 0.01194
chisq.test(ffo.table.loss[1,1:2], p = c(50,50), rescale.p=T)
# X-squared = 0.3612, df = 1, p-value = 0.5478

cat("
CHILD CHI SQUARE FOR LOSSES: 25%, 25%, 50%
")
chisq.test(ffo.table.loss[2,], p = c(25,25,50), rescale.p=T)
# X-squared = 6.5824, df = 2, 
# p-value = 0.03721
chisq.test(ffo.table.loss[2,1:2], p = c(50,50), rescale.p=T)
# X-squared = 0.0733, df = 1, p-value = 0.7866



cat("\n\n
TESTS FOR SIGNIFICANT DIFFERENCES IN COUNTERBALANCING TOPRIGHT AND SIDEMAG
\n\n")
cat("adult loss cond")
chisq.test(ffo.table.vert.sideMag[,,1,1])
# X-squared = 0.0504, df = 1, p-value = 0.8223
cat("child loss cond")
chisq.test(ffo.table.vert.sideMag[,,2,1])
# X-squared = 0.133, df = 1, p-value = 0.7154

cat("adult gain cond")
chisq.test(ffo.table.vert.sideMag[,,1,2])
# X-squared = 0.0666, df = 1, p-value = 0.7963
cat("child gain cond")
chisq.test(ffo.table.vert.sideMag[,,2,2])
# X-squared = 0.0172, df = 1, p-value = 0.8956

sink()




# 	FFO FOR GAIN AND LOSS CONDITIONS - COMBINED CHOICES

# FFO TABLES BY SUBJECT MEANS
a <- summarySE(table_data_sure, measurevar="subjectID", groupvars=c("subjectID","ffo.decision","condition", "AgeGroup"))
a <- a[,c("subjectID", "ffo.decision", "condition", "AgeGroup", "N")]
ffo_table.sureSub <- tapply(a$N, list(a$AgeGroup, a$ffo.decision, a$condition), mean)
rm(a)
a <- summarySE(table_data_gamble, measurevar="subjectID", groupvars=c("subjectID","ffo.decision","condition", "AgeGroup"))
a <- a[,c("subjectID", "ffo.decision", "condition", "AgeGroup", "N")]
ffo_table.gambleSub <- tapply(a$N, list(a$AgeGroup, a$ffo.decision, a$condition), mean)
rm(a)
# RENAME FOR EASY FIGURE LABELING
rownames(ffo_table.sureSub) <- c("adult\nsafe","child\nsafe")
rownames(ffo_table.gambleSub) <- c("adult\ngamble","child\ngamble")


lfo_table.sure <- lfo_table.sure[,c(2,3,5),] 
lfo_table.gamble <- lfo_table.gamble[,c(2,3,5),] 

lfo_table.sure[1,,1] <- lfo_table.sure[1,,1] / sum(lfo_table.sure[1,,1])
lfo_table.sure[2,,1] <- lfo_table.sure[2,,1] / sum(lfo_table.sure[2,,1])
lfo_table.sure[1,,2] <- lfo_table.sure[1,,2] / sum(lfo_table.sure[1,,2])
lfo_table.sure[2,,2] <- lfo_table.sure[2,,2] / sum(lfo_table.sure[2,,2])

lfo_table.gamble[1,,1] <- lfo_table.gamble[1,,1] / sum(lfo_table.gamble[1,,1])
lfo_table.gamble[2,,1] <- lfo_table.gamble[2,,1] / sum(lfo_table.gamble[2,,1])
lfo_table.gamble[1,,2] <- lfo_table.gamble[1,,2] / sum(lfo_table.gamble[1,,2])
lfo_table.gamble[2,,2] <- lfo_table.gamble[2,,2] / sum(lfo_table.gamble[2,,2])




########################################################################


# NUMBER OF LAST FIXATIONS BY OBJECT, SEPARATELY BY GAMBLE VS. SURE DECISIONS
a <- summarySE(table_data_sure, measurevar="subjectID", groupvars=c("subjectID","lfo.decision","condition", "AgeGroup"))
a <- a[,c("subjectID", "lfo.decision", "condition", "AgeGroup", "N")]
lfo_table.sureSub <- tapply(a$N, list(a$AgeGroup, a$lfo.decision, a$condition), mean)
rm(a)
a <- summarySE(table_data_gamble, measurevar="subjectID", groupvars=c("subjectID","lfo.decision","condition", "AgeGroup"))
a <- a[,c("subjectID", "lfo.decision", "condition", "AgeGroup", "N")]
lfo_table.gambleSub <- tapply(a$N, list(a$AgeGroup, a$lfo.decision, a$condition), mean)
rm(a)
# RENAME FOR EASY FIGURE LABELING
rownames(lfo_table.sureSub) <- c("adult\nsafe","child\nsafe")
rownames(lfo_table.gambleSub) <- c("adult\ngamble","child\ngamble")



lfo_table.sureSub <- lfo_table.sureSub[,c(2,3,5),] 
lfo_table.gambleSub <- lfo_table.gambleSub[,c(2,3,5),] 

lfo_table.sureSub[1,,1] <- lfo_table.sureSub[1,,1] / sum(lfo_table.sureSub[1,,1])
lfo_table.sureSub[2,,1] <- lfo_table.sureSub[2,,1] / sum(lfo_table.sureSub[2,,1])
lfo_table.sureSub[1,,2] <- lfo_table.sureSub[1,,2] / sum(lfo_table.sureSub[1,,2])
lfo_table.sureSub[2,,2] <- lfo_table.sureSub[2,,2] / sum(lfo_table.sureSub[2,,2])

lfo_table.gambleSub[1,,1] <- lfo_table.gambleSub[1,,1] / sum(lfo_table.gambleSub[1,,1])
lfo_table.gambleSub[2,,1] <- lfo_table.gambleSub[2,,1] / sum(lfo_table.gambleSub[2,,1])
lfo_table.gambleSub[1,,2] <- lfo_table.gambleSub[1,,2] / sum(lfo_table.gambleSub[1,,2])
lfo_table.gambleSub[2,,2] <- lfo_table.gambleSub[2,,2] / sum(lfo_table.gambleSub[2,,2])




lfo_tableG <- rbind(lfo_table.sureSub[,,2], lfo_table.gambleSub[,,2])
lfo_tableL <- rbind(lfo_table.sureSub[,,1], lfo_table.gambleSub[,,1])
# RENAME FOR EASY FIGURE LABELING
rownames(lfo_tableL) <- c("safe\nadult","safe\nchild","gamble\nadult","gamble\nchild")
rownames(lfo_tableG) <- c("safe\nadult","safe\nchild","gamble\nadult","gamble\nchild")





######################################################################################
#	PLOT FFO AND LFO DATA
#pdf("/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze/analysis/Fig2_Eye_barplot_fo_fix_cmbdGAIN.pdf", height=5, width=5)

#pdf("/Users/dpaulsen/Dropbox/Research/SfN_2012/Fig2_Eye_barplot_FFO_cmbdGAIN.pdf", height=3, width=3)
pdf("~/Documents/Academics/Projects/EyeTrackingGaze/docs/manuscript/Figures/Fig3_LFO.pdf", height=2.5, width=5)


	par(mfrow=c(1,2))
	par(mar=c(2.6, 2.6, 2, 0.1)) # c(5.1, 4.1, 4.1, 2.1)

	risk_col <- c(cv0col, cv1col, cv2col,cv3col)
	axis_cex=0.7
	label_cex = 0.8
	names_cex = 0.7
	plot_cex = 1.5
	axis_mgp <- c(1.5, 0.6, 0) 	#mgp=c(3,1,0) # specifies lines at which label, axis values, are positioned
	
	
	legend_cex = 0.4
	legend_ptcex = 0.8
	legend_linewidth = 1
	main_cex = 0.9
	
	mtext_line = 1
	mtext_offset = 0.6
	atPos <- 0 - mtext_offset
	
	bar_text_offset <- 0.05
	bar_cex <- 0.5
	
	lfoplot <- barplot(t(lfo_tableG), beside=F, legend=F, ylab="Prop. Trials",#yaxt='none',
		main="LFO Gain Domain:\nAge Group by Choice", 
		col= outcome_gain_col, 
		xlab="Age Group by Choice", ylim=c(0,1.1),
		mgp=axis_mgp, cex.axis=axis_cex, cex.main=main_cex, 
		cex.lab=label_cex, cex.names=names_cex, font.main=1)
	
	means <- round(t(lfo_tableG), digits=3)

	current_means <- means[1,]
	text(lfoplot, current_means/2 , paste(means[1,]*100, "%", sep=""), xpd = TRUE, cex= bar_cex)
	for (i in 2:3) {
		if (i == 3) {bar_col <- "white"} else { bar_col <- "black"}
		text(lfoplot, current_means + (means[i,])/2, paste(means[i,]*100, "%", sep=""), col=bar_col, xpd = TRUE, cex= bar_cex)
		current_means <- current_means + means[i,]
	}		
	box()
	mtext("A", side=3, line=mtext_line, at=atPos, cex=1)
	legend("top", c("Lg Mag", "Sm Mag", "Sure Bet"), cex= legend_cex, 
	   bty="n", fill= outcome_gain_col, horiz=T)	





	barplot(t(lfo_tableL), beside=F, legend=F, #yaxt='none', 
		ylab="Prop. Trials",
		xlab="Age Group by Choice", ylim=c(0,1.1),
		main="LFO Loss Domian:\nAge Group by Choice", col= outcome_loss_col, 
		mgp=axis_mgp, cex.axis=axis_cex, cex.main=main_cex, 
		cex.lab=label_cex, cex.names=names_cex, font.main=1)

	means <- round(t(lfo_tableL), digits=3)
	current_means <- means[1,]
	bar_col <- "white"
	text(lfoplot, current_means/2 , paste(means[1,]*100, "%", sep=""), col=bar_col, xpd = TRUE, cex= bar_cex)
	for (i in 2:3) {
		bar_col <- "white"
		text(lfoplot, current_means + (means[i,])/2, paste(means[i,]*100, "%", sep=""), col=bar_col, xpd = TRUE, cex= bar_cex)
		current_means <- current_means + means[i,]

	}
	box()
	mtext("B", side=3, line=mtext_line, at=atPos, cex=1)

	legend("top", c("Lg Mag", "Sm Mag", "Sure Bet"), cex= legend_cex, 
	   bty="n", fill= outcome_loss_col, horiz=T)


dev.off()



	# mtext("C", side=3, line=mtext_line, at=atPos, cex=1)



	# mtext("D", side=3, line=mtext_line, at=atPos, cex=1)

	# barplot(t(100*ffo.table.gain[5:6,]), beside=T, legend=F,
		# names.arg=c("Adult", "Child"),
		# main="FFO Gain Domain:\nPercent Change from Expected", col= outcome_gain_col, 
		# xlab="Age Group", #xaxt='none',
		# ylab="% Change from Expected",
		# ylim=c(-25,25),
		# mgp=axis_mgp, cex.axis=axis_cex, cex.main=main_cex, 
		# cex.lab=label_cex, cex.names=names_cex, font.main=1)
	# abline(h=0)
	# box()
	# legend("topleft", c("Lg Mag Object", "Sm Mag Object", "Sure Bet Object"), cex= legend_cex, 
	   # bty="n", fill= outcome_gain_col, horiz=FALSE)

	# barplot(t(100*ffo.table.loss[5:6,]), beside=T, legend=F,
		# names.arg=c("Adult", "Child"),
		# main="FFO Loss Domain:\nPercent Change from Expected", col= outcome_loss_col, 
		# xlab="Age Group", #xaxt='none',
		# ylab="% Change from Expected",
		# ylim=c(-20,20),
		# mgp=axis_mgp, cex.axis=axis_cex, cex.main=main_cex, 
		# cex.lab=label_cex, cex.names=names_cex, font.main=1)
	# abline(h=0)
	# box()
	# legend("topleft", c("Lg Mag Object", "Sm Mag Object", "Sure Bet Object"), cex= legend_cex, 
	   # bty="n", fill=outcome_loss_col, horiz=F)







######################################################################################
#	PLOT DWELL TIME DATA			





#pdf("/Users/dpaulsen/Dropbox/Research/SfN_2012/Fig2_Eye_barplot_DwellGamble_GAIN.pdf", height=3, width=3)
pdf("~/Documents/Academics/Projects/EyeTrackingGaze/docs/manuscript/Figures/Fig3_DwellTime_sig.pdf", height=5, width=5)

	par(mar=c(2.6, 2.6, 2.1, 0.1)) # c(5.1, 4.1, 4.1, 2.1)
	par(mfrow=c(2,2))
	axis_mgp <- c(1, 0.4, 0) 	#mgp=c(3,1,0) # specifies lines at which label, axis values, are positioned
	names_cex = 0.7
	
	
	anova_tick_len <- 0.025
	anova_lwd <- 1.5
	t_sig_ht <- 0.025


	lim_y <- c(0,0.8)
	
	
	
	
	gain_gamble_means <- cast(decision_prop.tableMN, object ~ group ~ gamble, mean, subset=condition=="Gain", value="prop")[,,2]
	gain_gamble_se <- cast(decision_prop.tableSDSE, object ~ group ~ gamble, mean, subset=condition==1, value="propSE")[,,2]
	b1 <- barplot( gain_gamble_means, 
		beside=T,col= outcome_gain_col, ylim=lim_y,
		ylab="Prop. Dwell Time",
		#ylab="", yaxt='none',
		xlab="Age Group",
		main="Dwell Time Before Gamble: Gain",
		mgp=axis_mgp, cex.axis=axis_cex, cex.main=main_cex, cex.lab=label_cex, cex.names=names_cex, font.main=1)
	abline(h=0.33, lty="dashed")
	error.bar(b1,gain_gamble_means, gain_gamble_se)
	legend("topleft", c("Lg Mag Object", "Sm Mag Object", "Sure Bet Object"), cex= legend_cex, 
	   bty="n", fill= outcome_gain_col, horiz=FALSE)
	box()
	mtext("A", side=3, line=mtext_line, at=atPos, cex=1)

	# DRAW ANOVA TICKS
	anova_tick_ht <- 0.58
	segments(b1[1,1], anova_tick_ht, b1[1,2], anova_tick_ht, col= outcome_gain_col[1], lwd=anova_lwd)
	for (ht in c(1,2)) {
		segments(b1[1,ht], anova_tick_ht - anova_tick_len , b1[1,ht], anova_tick_ht,
			col= outcome_gain_col[1],
			lwd=anova_lwd)
	}
	text(mean(c(b1[1,1], b1[1,2])) , anova_tick_ht-t_sig_ht,"*")

	anova_tick_ht <- 0.5
	segments(b1[3,1], anova_tick_ht, b1[3,2], anova_tick_ht, col= outcome_gain_col[3], lwd=anova_lwd)
	for (ht in c(1,2)) {
		segments(b1[3,ht], anova_tick_ht - anova_tick_len , b1[3,ht], anova_tick_ht,
			col= outcome_gain_col[3],
			lwd=anova_lwd)
	}
	text(mean(c(b1[3,1], b1[3,2])) , anova_tick_ht-t_sig_ht,"*")






	atPos <- 0 - mtext_offset/2
	gain_safe_means <- cast(decision_prop.tableMN, object ~ group ~ gamble, mean, subset=condition=="Gain", value="prop")[,,1]
	gain_safe_se <- cast(decision_prop.tableSDSE, object ~ group ~ gamble, mean, subset=condition==1, value="propSE")[,,1]
	b2 <- barplot( gain_safe_means, 
		beside=T,col= outcome_gain_col, ylim=lim_y,
		ylab="Prop. Dwell Time",
		xlab="Age Group",
		main="Dwell Time Before Safe: Gain",
		mgp=axis_mgp, cex.axis=axis_cex, cex.main=main_cex, cex.lab=label_cex, cex.names=names_cex, font.main=1)
	abline(h=0.33, lty="dashed")
	error.bar(b2, gain_safe_means, gain_safe_se)
	
	box()

	mtext("B", side=3, line=mtext_line, at=atPos, cex=1)

	anova_tick_ht <- 0.52
	# text(b1[3,1] , anova_tick_ht,"*")	
	# text(b1[3,2] , anova_tick_ht,"*")	


	
	loss_gamble_means <- cast(decision_prop.tableMN, object ~ group ~ gamble, mean, subset=condition=="Loss", value="prop")[,,2]
	loss_gamble_se <- cast(decision_prop.tableSDSE, object ~ group ~ gamble, mean, subset=condition==0, value="propSE")[,,2]

	b3 <- barplot( loss_gamble_means, 
		beside=T,col= outcome_loss_col, ylim=lim_y,
		ylab="Prop. Dwell Time", #yaxt='none',
		xlab="Age Group",
		main="Dwell Time Before Gamble: Loss",
		mgp=axis_mgp, cex.axis=axis_cex, cex.main=main_cex, cex.lab=label_cex, cex.names=names_cex, font.main=1)	
	abline(h=0.33, lty="dashed")
	error.bar(b3, loss_gamble_means, loss_gamble_se)


	box()
	legend("topleft", c("Lg Mag Object", "Sm Mag Object", "Sure Bet Object"), cex= legend_cex, 
	   bty="n", fill= outcome_loss_col, horiz=FALSE)
	mtext("C", side=3, line=mtext_line, at=atPos, cex=1)
	
	
	# DRAW ANOVA TICKS
	anova_tick_ht <- 0.58
	segments(b3[2,1], anova_tick_ht, b3[2,2], anova_tick_ht, col= outcome_loss_col[2], lwd=anova_lwd)
	for (ht in c(1,2)) {
		segments(b3[2,ht], anova_tick_ht - anova_tick_len , b3[2,ht], anova_tick_ht,
			col= outcome_loss_col[2],
			lwd=anova_lwd)
	}
	text(mean(c(b3[2,1], b3[2,2])) , anova_tick_ht-t_sig_ht,"*")


	anova_tick_ht <- 0.5
	segments(b3[3,1], anova_tick_ht, b3[3,2], anova_tick_ht, col= outcome_loss_col[3], lwd=anova_lwd)
	for (ht in c(1,2)) {
		segments(b3[3,ht], anova_tick_ht - anova_tick_len , b3[3,ht], anova_tick_ht,
			col= outcome_loss_col[3],
			lwd=anova_lwd)
	}
	text(mean(c(b3[3,1], b3[3,2])) , anova_tick_ht-t_sig_ht,"*")

	
	
	
	

	loss_safe_means <- cast(decision_prop.tableMN, object ~ group ~ gamble, mean, subset=condition=="Loss", value="prop")[,,1]
	loss_safe_se <- cast(decision_prop.tableSDSE, object ~ group ~ gamble, mean, subset=condition==0, value="propSE")[,,1]
	
	b4 <- barplot( loss_safe_means, 
		beside=T,col= outcome_loss_col, ylim=lim_y,
		ylab="Prop. Dwell Time",
		xlab="Age Group",
		main="Dwell Time Before Safe: Loss",
		mgp=axis_mgp, cex.axis=axis_cex, cex.main=main_cex, cex.lab=label_cex, cex.names=names_cex, font.main=1)
	abline(h=0.33, lty="dashed")
	error.bar(b4, loss_safe_means, loss_safe_se)
	box()
	mtext("D", side=3, line=mtext_line, at=atPos, cex=1)

	anova_tick_ht <- 0.52
	# text(b1[3,1] , anova_tick_ht,"*")	
	# text(b1[3,2] , anova_tick_ht,"*")	




dev.off()
















#######################################
# SEPEARATE PLOTS BY CV @ GAMBLE
decision_prop.frame2 <- melt(subset_riskData.decision[, c(51,52,54,8,98,100)],id=c("gamble","AgeGroup","cv1"))
decision_prop.table2 <- tapply(decision_prop.frame2$value, list(decision_prop.frame2$AgeGroup, decision_prop.frame2$variable, decision_prop.frame2$gamble, decision_prop.frame2$cv1), mean)

decision_prop.table2 <- melt(decision_prop.table2)
names(decision_prop.table2) <- c("group","object","gamble","cv","prop")
# somehow 99 gambles are in there.
decision_prop.table2 <- subset(decision_prop.table2, gamble < 2)
decision_prop.table2$cv <- as.character(round(decision_prop.table2$cv,digits=2))

decision_prop.table2$object  <- sub("_prop.decision", "", decision_prop.table2$object) # cut out "_prop.decision"
decision_prop.table2$gamble <- factor(decision_prop.table2$gamble, levels=c(0,1), labels=c("sure","gamble"))
decision_prop.table2$object <- factor(decision_prop.table2$object, levels=c("mag1","mag2","sure2"), labels=c("gain","loss","sure"))

quartz()
ggplot(decision_prop.table2[decision_prop.table2$gamble == "sure",], aes(object, prop)) +
 	geom_bar(aes(y = prop, fill = object), stat = "identity") + 
	scale_fill_brewer(palette="Paired") +
	facet_grid(cv ~ group) +
	geom_hline(aes(yintercept=0.33),color="gray75") + 
	opts(panel.background=theme_blank(), axis.line = theme_segment()) +
	opts(title = "Decision: Proportion of fixations on objects\n before choosing Sure Bet", plot.title = theme_text(size = 16)) +
	opts(legend.position='none')

quartz()
ggplot(decision_prop.table2[decision_prop.table2$gamble == "gamble",], aes(object, prop)) +
 	geom_bar(aes(y = prop, fill = object), stat = "identity") + 
	scale_fill_brewer(palette="Paired") +
	facet_grid(cv ~ group) +
	geom_hline(aes(yintercept=0.33),color="gray75") + 
	opts(panel.background=theme_blank(), axis.line = theme_segment()) +
	opts(title = "Decision: Proportion of fixations on objects\n before choosing Gamble", plot.title = theme_text(size = 16)) +
	opts(legend.position='none')



quartz()
ggplot(decision_prop.table2[decision_prop.table2$gamble == "sure",], aes(object, prop)) +
 	geom_bar(aes(y = prop, fill = object), stat = "identity") + 
	scale_fill_brewer(palette="Paired") +
	facet_grid(~ group) +
	geom_hline(aes(yintercept=0.33),color="gray75") + 
	opts(panel.background=theme_blank(), axis.line = theme_segment()) +
	opts(title = "Decision: Proportion of fixations on objects\n before choosing Sure Bet", plot.title = theme_text(size = 16)) +
	opts(legend.position='none')

quartz()
ggplot(decision_prop.table2[decision_prop.table2$gamble == "gamble",], aes(object, prop)) +
 	geom_bar(aes(y = prop, fill = object), stat = "identity") + 
	scale_fill_brewer(palette="Paired") +
	facet_grid(~ group) +
	geom_hline(aes(yintercept=0.33),color="gray75") + 
	opts(panel.background=theme_blank(), axis.line = theme_segment()) +
	opts(title = "Decision: Proportion of fixations on objects\n before choosing Gamble", plot.title = theme_text(size = 16)) +
	opts(legend.position='none')





