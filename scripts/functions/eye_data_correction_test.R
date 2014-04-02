get_lost_fix_stats <- function(eye_data, trial_num) {

source("functions/cut_repeats.R")

#trial_num=1
a <- eye_data[eye_data$trial==trial_num,]

# b is second step: separate data by epoch
b.decision <- a[a$epoch=="decision",]
b.choice <- a[a$epoch=="choice",]
b.outcome <- a[a$epoch=="outcome",]

# c is third step: cut_repeats returns a list with [[1]] sequence of Found and [[2]] counts.
c.decision <- cut_repeats(b.decision$Found)
c.decision[[3]] <- rep("decision", times=length(c.decision[[1]]))

c.choice <- cut_repeats(b.choice$Found)
c.choice[[3]] <- rep("choice", times=length(c.choice[[1]]))

c.outcome <- cut_repeats(b.outcome$Found)
c.outcome[[3]] <-rep("outcome", times=length(c.outcome[[1]]))


#b.decision[,c("Found","GazepointX", "GazepointY","left_x", "left_y","right_x", "right_y","left_pup_diam")]

c.decision[[4]] <- 0* c.decision[[2]]
c.decision[[5]] <- 0* c.decision[[2]]
c.decision[[4]][1] <- 1 # start line
c.decision[[5]][1] <- c.decision[[2]][1] # end line
for (i in 2:length(c.decision[[2]])) {
	# c.decision[[2]] = duration of lost or found fixations
	c.decision[[4]][i] <- c.decision[[5]][i-1] + 1
	c.decision[[5]][i] <- c.decision[[4]][i] + c.decision[[2]][i] - 1# add current start and duration
}

c.choice[[4]] <- 0* c.choice[[2]]
c.choice[[5]] <- 0* c.choice[[2]]
c.choice[[4]][1] <- 1 # start line
c.choice[[5]][1] <- c.choice[[2]][1] # end line
for (i in 2:length(c.choice[[2]])) {
	# c.choice[[2]] = duration of lost or found fixations
	c.choice[[4]][i] <- c.choice[[5]][i-1] + 1
	c.choice[[5]][i] <- c.choice[[4]][i] + c.choice[[2]][i] - 1# add current start and duration
}


c.outcome[[4]] <- 0* c.outcome[[2]]
c.outcome[[5]] <- 0* c.outcome[[2]]
c.outcome[[4]][1] <- 1 # start line
c.outcome[[5]][1] <- c.outcome[[2]][1] # end line
for (i in 2:length(c.decision[[2]])) {
	# c.decision[[2]] = duration of lost or found fixations
	c.outcome[[4]][i] <- c.outcome[[5]][i-1] + 1
	c.outcome[[5]][i] <- c.outcome[[4]][i] + c.outcome[[2]][i] - 1# add current start and duration
}



#hist(c(c.decision[[2]],c.choice[[2]]), breaks=200, xlim=c(0,50))

d <- data.frame(found=c(c.decision[[1]],c.choice[[1]],c.outcome[[1]]),
				count=c(c.decision[[2]],c.choice[[2]],c.outcome[[2]]),
				startRow=c(c.decision[[4]],c.choice[[4]],c.outcome[[4]]),
				endRow=c(c.decision[[5]],c.choice[[5]],c.outcome[[5]]),
				epoch=c(c.decision[[3]],c.choice[[3]],c.outcome[[3]])  )


d



}


