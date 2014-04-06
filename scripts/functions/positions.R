# FUNCTION TO DETERMINE BOUNDARY FRAME FOR AREAS OF INTEREST
current_frame_positions <- function(trialType, vert, sideMag, sideSure, boundary_ext=20) {

	# [trialType, vert, sideMag, sideSure] parameters passed in from beh_data file
	# boundary_ext: distance to extend area of interest perimeter, in pixels
	
	# [[1]] = top, [[2]] = bottom
	vertical <- list(c(263,442),c(582,761)) 
	
	# [[1]] = left, [[2]] = right, [[3]] = middle
	horizontal <- list(c(401,580), c(700,879), c(550,729)) 
	
	for (i in 1:length(vertical)) {
		vertical[[i]] <- vertical[[i]] + c(-boundary_ext, boundary_ext)
	}
	
	for (i in 1:length(horizontal)) {
		horizontal[[i]] <- horizontal[[i]] + c(-boundary_ext, boundary_ext)
	}
	
	
	if (trialType == 1) {
		
		# on sure trials, vert=1 means sure1 (larger) is on top, sure2 is on bottom
		if (vert == 1) {
	
			sure1 <- list(horizontal[[3]], vertical[[1]])
			sure2 <- list(horizontal[[3]], vertical[[2]])		
	
		} else {
	
			sure1 <- list(horizontal[[3]], vertical[[2]])
			sure2 <- list(horizontal[[3]], vertical[[1]])
	
		}
		list(sure1, sure2)
		
	# on gamble trials, vert=1 means mag1 and mag2 on top, sure2 on bottom
	#	side=1 means mag1 (larger) is on left, mag2 is on right
	} else if ( (trialType > 1) && (trialType < 6) ) {
	
		if ( (vert == 1) && (sideMag == 1)) {
		
			gain <- list(horizontal[[1]], vertical[[1]])
			loss <- list(horizontal[[2]], vertical[[1]])
			sure <- list(horizontal[[3]], vertical[[2]])
		
		} else if ( (vert == 1) && (sideMag == 0)) {
			
			gain <- list(horizontal[[2]], vertical[[1]])
			loss <- list(horizontal[[1]], vertical[[1]])
			sure <- list(horizontal[[3]], vertical[[2]])
			
		} else if ( (vert == 0) && (sideMag == 1)) {
			
			gain <- list(horizontal[[1]], vertical[[2]])
			loss <- list(horizontal[[2]], vertical[[2]])
			sure <- list(horizontal[[3]], vertical[[1]])
		
		} else if ( (vert == 0) && (sideMag == 0)) {
		
			gain <- list(horizontal[[2]], vertical[[2]])
			loss <- list(horizontal[[1]], vertical[[2]])
			sure <- list(horizontal[[3]], vertical[[1]])
		
		}
		list(gain, loss, sure)
	
	} else if (trialType == 6) {
	
		if ( (vert == 1) && (sideMag == 1)) {
		
			gainMag <- list(horizontal[[1]], vertical[[1]])
			lossMag <- list(horizontal[[2]], vertical[[1]])
		
			if (sideSure == 1) {

				gainSure <- list(horizontal[[1]], vertical[[2]])
				lossSure <- list(horizontal[[2]], vertical[[2]])
				
			} else {

				gainSure <- list(horizontal[[2]], vertical[[2]])
				lossSure <- list(horizontal[[1]], vertical[[2]])
				
			}
		
		} else if ( (vert == 1) && (sideMag == 0)) {
			
			gainMag <- list(horizontal[[2]], vertical[[1]])
			lossMag <- list(horizontal[[1]], vertical[[1]])

			if (sideSure == 1) {

				gainSure <- list(horizontal[[1]], vertical[[2]])
				lossSure <- list(horizontal[[2]], vertical[[2]])
				
			} else {

				gainSure <- list(horizontal[[2]], vertical[[2]])
				lossSure <- list(horizontal[[1]], vertical[[2]])
				
			}
			
		} else if ( (vert == 0) && (sideMag == 1)) {
			
			gainMag <- list(horizontal[[1]], vertical[[2]])
			lossMag <- list(horizontal[[2]], vertical[[2]])
		
			if (sideSure == 1) {

				gainSure <- list(horizontal[[1]], vertical[[1]])
				lossSure <- list(horizontal[[2]], vertical[[1]])
				
			} else {

				gainSure <- list(horizontal[[2]], vertical[[1]])
				lossSure <- list(horizontal[[1]], vertical[[1]])
				
			}
			
		} else if ( (vert == 0) && (sideMag == 0)) {
		
			gainMag <- list(horizontal[[2]], vertical[[2]])
			lossMag <- list(horizontal[[1]], vertical[[2]])

			if (sideSure == 1) {

				gainSure <- list(horizontal[[1]], vertical[[1]])
				lossSure <- list(horizontal[[2]], vertical[[1]])
				
			} else {

				gainSure <- list(horizontal[[2]], vertical[[1]])
				lossSure <- list(horizontal[[1]], vertical[[1]])
				
			}
		
		}
		list(gainMag, lossMag, gainSure, lossSure)
	
	}



}

