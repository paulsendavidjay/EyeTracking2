is_bounded <- function(x, y, xrange, yrange) {
	
	
	outputVector <- x >= min(xrange) & x <= max(xrange) & y >= min(yrange) & y <= max(yrange)
	outputVector[is.na(outputVector)] <- 0 # replace NAs with 0
	outputVector
	# if (x >= min(xrange) & x <= max(xrange) & y >= min(yrange) & y <= max(yrange)) {
		# return(TRUE)
	# } else {
		# return(FALSE)
	# }
	
}