# FUNCTION TO DETERMINE IF VALUES
# x and y are vectors of equal length
# xrange, range, vectors of length 2
# output is logical vector

is_bounded <- function(x, y, xrange, yrange) {
	outputVector <- x >= min(xrange) & x <= max(xrange) & y >= min(yrange) & y <= max(yrange)
	outputVector[is.na(outputVector)] <- 0 # replace NAs with 0
	outputVector
}