is_bounded <- function(x, y, xrange, yrange) {
	# x and y are lists of equal length
	# xrange, range, lists of length 2
	# output is logical vector
	outputVector <- x >= min(xrange) & x <= max(xrange) & y >= min(yrange) & y <= max(yrange)
	outputVector[is.na(outputVector)] <- 0 # replace NAs with 0
	outputVector
	
}