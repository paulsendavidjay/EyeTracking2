# THIS FUNCTION WILL RETURN A LOGICAL LIST OF LENGTH Rts INDICATING WHICH VALUES LAY OUTSIDE THE RANGE SET BY n_std_devs (standard deviations)

reject_outliers <- function(Rts, n_std_devs) {
	
	Rts[ Rts == Inf] <- NA
	Rts[ Rts == -Inf] <- NA
	
	limit <- n_std_devs
	outliers <- TRUE
	Rts_copy <- Rts
	while (outliers == TRUE) {

		max_value <- max(Rts_copy, na.rm=T)
		max_index <- which(Rts_copy == max(Rts_copy))
		min_value <- min(Rts_copy, na.rm=T)
		min_index <- which(Rts_copy == min(Rts_copy))
		Rts_copy <- Rts_copy[Rts_copy != max_value ]
		Rts_copy <- Rts_copy[Rts_copy != min_value ]
		
		upper_limit <- mean(Rts_copy, na.rm=T) + limit*sd(Rts_copy, na.rm=T)
		lower_limit <- mean(Rts_copy, na.rm=T) - limit*sd(Rts_copy, na.rm=T)
		
		if (max_value > upper_limit) {
			if (min_value > lower_limit) {
				# replace min values if within threshold, * however many there are (in index)
				Rts_copy <- c( Rts_copy, rep(min_value, length(min_index)) ) 
			}
		} else {
			# replace max values if within threshold, * however many there are (in index)
			Rts_copy <- c( Rts_copy, rep(max_value, length(max_index)) )
			if (min_value > lower_limit) {
				# replace min values if within threshold, * however many there are (in index)
				Rts_copy <- c( Rts_copy, rep(min_value, length(min_index)) ) 
				outliers <- FALSE
			}
		}
	}
	upper_limit = mean(Rts_copy, na.rm=T) + limit*sd(Rts_copy, na.rm=T)
	lower_limit = mean(Rts_copy, na.rm=T) - limit*sd(Rts_copy, na.rm=T)
	(Rts < lower_limit) | (Rts > upper_limit)
}