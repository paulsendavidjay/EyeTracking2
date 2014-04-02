seek_time_line <- function(seek_time, time_array) {
	if ((seek_time < (min(time_array) - 0.05)) || seek_time > (max(time_array) + 0.05)) {
		print("error in seek_time_line: array does not contain value")
		print(list(seekTime=seek_time, min=min(time_array), max=max(time_array)))
		return(0)
	} else {
		
		for (i in 1:length(time_array)) {
			
			if (seek_time < time_array[i]) {
				if (abs(seek_time - time_array[i]) > abs(seek_time - time_array[i-1]) ) {
					return(i-1)
				} else {
					return(i)
				} # end determining which of two time points is closest
			} # end found closest time points
		 } # end for i in length
	} # end if seek_time is not in array

} # end function
