

# subject 1 considered pilot - unanalyzable data (no variance), and no age information

subjectlist <- read.table("/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze/data/participant_info.txt", header=T, sep="\t")
birth_date <- strptime(subjectlist$DOB, format = "%m/%d/%y")
test_date <- strptime(subjectlist$test_date, format = "%m/%d/%y")
AgeYears <- (test_date - birth_date) / 365
AgeMonths <- AgeYears * 12



participant_ages <- as.data.frame( round( cbind(subjectlist$subjectID, AgeYears, AgeMonths, subjectlist$sex ), digits=2))
names(participant_ages) <- c("subjectID","AgeYears","AgeMonths", "sex")
participant_ages$sex <- participant_ages$sex - 1 # males = 1
subjectlist <- cbind(subjectlist, participant_ages)

rm(birth_date, test_date, AgeYears, AgeMonths)

adults <- subset(subjectlist, subjectID < 90) # n = 18
adults <- subset(adults, incl == 1) # n = 17 (tracker failed for 53, no data)
sum(adults$sex == "F") # 11
mean(adults$AgeYears) # 22.79

all_adults <- c(51:52,54:68) # n = 17     51? 58? low pupil counts
rejected_adults <- c(53)
# 53 (tracker fail)

pilot_adults <- (90:95) # controls

children <- subset(subjectlist, subjectID > 99) # n = 35
children_excl <- subset(children, incl == 0) # n = 14
children <- subset(children, incl == 1) # n = 21
sum(children$sex == "F") # 13
mean(children$AgeYears) # 7.69



all_children <- c(502,505,507,508,509,511,512,516,517,518,519,520,521,523,525,526,527,529,530,534,535) # same as 'children'
rejected_children <- c(501,503,504,506,510,513,514,515,522,524,528,531,532,533) # same subs as 'children_excl'
# 501 (bored),
# 503 (tracker fail) 
# 504 (tracker fail)
# 506
# 510 (tracker fail)
# ???513 tracker qual? 	??????????????????????????
# 514 (bored)
# 515 (bored, did not finish)
# ???	519 			??????????????????????????
# 522 (had no variance in behavior, gambled 100% gain, 0% loss)
# 524 (sick, did not finish)
# 528 (bored, did not want to finish)
# 529 super fidgity
# 530 
# 531 some problems tracking tracker fail
# 532 parent in room, child aware, tracker not following
# 533 tracker problems aug 23
# 534 tracker had a fussy day aug 23
# 535 great! aug 23
# 

all_subjects <- c(all_adults, all_children)
all_subjects_w_cntls <- c(all_adults, all_children, 90,91,92,93,94,95)
incomplete_subs <- c(rejected_adults,rejected_children)

controls <- subset(subjectlist, (subjectID > 89) & (subjectID < 100)) # n = 5
sum(controls$sex == "F") # 2
mean(controls$AgeYears) # 25.13


###############################################################


good_adults.decision <- c(51,52,54,55,57,58)
good_adults.choice <- c(51,52,54,55,57,58)
good_adults.outcome <- c(51,52,54,55,57,58)


good_children.decision <- c(502,505,507,508,509,511,516,517,518,520,521,523) 
good_children.choice <- c(502,505,507,508,509,511,516,517,518,520,521,523)
good_children.outcome <- c(502,505,507,508,509,511,516,517,518,520,521,523)




good_all.decision <- c(good_adults.decision, good_children.decision)
good_all.choice <- c(good_adults.choice, good_children.choice)
good_all.outcome <- c(good_adults.outcome, good_children.outcome)

good_all <- unique(good_all.decision, good_all.choice, good_all.outcome)





