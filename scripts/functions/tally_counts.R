tally_counts <- function(xpos_array, ypos_array, xrange, yrange) {
	
	if (length(xpos_array) != length(ypos_array)) {
		return(0)
	} else {
		count <- 0
		for (i in 1:length(xpos_array)) {
			if (is_bounded(xpos_array[i], ypos_array[i], xrange, yrange)) { count <- count + 1 }
		} # for i in length(xpos_array)
		return(count)
	} # end if length(xpos_array)
	
}


find_ffo <- function(xpos_array, ypos_array, vert, side) {
	if (length(xpos_array) != length(ypos_array)) {
		return(NULL)
	} else {
		ffo <- ""
		current_gain <- current_frame_positions(vert, side)[[1]]
		current_loss <- current_frame_positions(vert, side)[[2]]
		current_sure <- current_frame_positions(vert, side)[[3]]
		i <- 1
		for (i in 1:length(xpos_array)){
			if (is_bounded(xpos_array[i], ypos_array[i], current_gain[[1]], current_gain[[2]])) { ffo <- "gain" }
			else if (is_bounded(xpos_array[i], ypos_array[i], current_loss[[1]], current_loss[[2]])) { ffo <- "loss" }
			else if (is_bounded(xpos_array[i], ypos_array[i], current_sure[[1]], current_sure[[2]])) { ffo <- "sure" }
		}
		return(ffo)
	}

}


find_lfo <- function(xpos_array, ypos_array, vert, side) {
	if (length(xpos_array) != length(ypos_array)) {
		return(NULL)
	} else {
		lfo <- ""
		current_gain <- current_frame_positions(vert, side)[[1]]
		current_loss <- current_frame_positions(vert, side)[[2]]
		current_sure <- current_frame_positions(vert, side)[[3]]
		i <- 1
		for (i in length(xpos_array):1){
			if (is_bounded(xpos_array[i], ypos_array[i], current_gain[[1]], current_gain[[2]])) { lfo <- "gain" }
			else if (is_bounded(xpos_array[i], ypos_array[i], current_loss[[1]], current_loss[[2]])) { lfo <- "loss" }
			else if (is_bounded(xpos_array[i], ypos_array[i], current_sure[[1]], current_sure[[2]])) { lfo <- "sure" }
		}
		return(lfo)
	}

}
