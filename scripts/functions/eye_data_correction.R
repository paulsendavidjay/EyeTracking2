eye_data_correction <- function(eye_data, trial_num, max_interp_len = 10) {

	source("functions/cut_repeats.R")
	
	#trial_num=1
	a <- eye_data[eye_data$trial==trial_num,]
	
	# collect TRUE or FALSE wheter left or right eyes were found
	left <- a$left_y != -1
	right <- a$right_y != -1
	
	
	left <- cut_repeats(left) # returns two lists: one of the order of T's and F's, and one for the length of each, respectively
	right <- cut_repeats(right)
	
	

	if (length(left[[1]]) > 2) {
	
		
		left[[3]] <- 0* left[[2]]
		left[[4]] <- 0* left[[2]]
		left[[3]][1] <- 1 # start line
		left[[4]][1] <- left[[2]][1] # end line
		for (i in 2:length(left[[2]])) {
			# left[[2]] = duration of lost or found fixations
			left[[3]][i] <- left[[4]][i-1] + 1
			left[[4]][i] <- left[[3]][i] + left[[2]][i] - 1# add current start and duration
		}
		
		left <- data.frame(found=left[[1]],
						count=left[[2]],
						startRow=left[[3]],
						endRow=left[[4]])
		
		for (i in 2:(nrow(left) - 1)) { # should start on second row b/c if first row is both, do nothing, if first row is missing, cannot do.
			
			if (left[i,"found"] == TRUE) {
				# do nothing
			} else if (left$count[i] < (max_interp_len + 1) ) {
				startRow <- left$startRow[i] - 1
				endRow <- left$endRow[i] + 1
				
				interpolated_left_x <- approx( c(a$left_x[startRow], a$left_x[endRow]), n=left$count[i])
				interpolated_left_y <- approx( c(a$left_y[startRow], a$left_y[endRow]), n=left$count[i])
				interpolated_left_pup_diam <- approx( c(a$left_pup_diam[startRow], a$left_pup_diam[endRow]), n=left$count[i])		
	
				a$replaced_L[(startRow+1):(endRow-1)] <- TRUE
				a$left_x[(startRow+1):(endRow-1)] <- interpolated_left_x[[2]]
				a$left_y[(startRow+1):(endRow-1)] <- interpolated_left_y[[2]]
				a$left_pup_diam[(startRow+1):(endRow-1)] <- interpolated_left_pup_diam[[2]]
							
			}
		}

	}

	
	if (length(right[[1]]) > 2) { 

	
		right[[3]] <- 0* right[[2]]
		right[[4]] <- 0* right[[2]]
		right[[3]][1] <- 1 # start line
		right[[4]][1] <- right[[2]][1] # end line
		for (i in 2:length(right[[2]])) {
			# right[[2]] = duration of lost or found fixations
			right[[3]][i] <- right[[4]][i-1] + 1
			right[[4]][i] <- right[[3]][i] + right[[2]][i] - 1# add current start and duration
		}
		
		right <- data.frame(found=right[[1]],
						count=right[[2]],
						startRow=right[[3]],
						endRow=right[[4]])
		
	
	
		for (i in 2:(nrow(right) - 1)) { # should start on second row b/c if first row is both, do nothing, if first row is missing, cannot do.
			
			if (right[i,"found"] == TRUE) {
				# do nothing
			} else if (right$count[i] < (max_interp_len + 1) ) {
				startRow <- right$startRow[i] - 1
				endRow <- right$endRow[i] + 1
				
				interpolated_right_x <- approx( c(a$right_x[startRow], a$right_x[endRow]), n=right$count[i])
				interpolated_right_y <- approx( c(a$right_y[startRow], a$right_y[endRow]), n=right$count[i])
				interpolated_right_pup_diam <- approx( c(a$right_pup_diam[startRow], a$right_pup_diam[endRow]), n=right$count[i])
			
				a$replaced_R[(startRow+1):(endRow-1)] <- TRUE
				a$right_x[(startRow+1):(endRow-1)] <- interpolated_right_x[[2]]
				a$right_y[(startRow+1):(endRow-1)] <- interpolated_right_y[[2]]
				a$right_pup_diam[(startRow+1):(endRow-1)] <- interpolated_right_pup_diam[[2]]
	
				
			}
		}

	}
	
	a # return eye_data

}


