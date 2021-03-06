# RETURNS LIST OF LENGTH 2:
	# [[1]] VECTOR OF ITEMS AS THEY APPEAR (OUTPUT CONTAINS NO ITEMS APPEARING BESIDE ANOTHER OF THE SAME VALUE)
	# [[2]] COUNTS OF ITEMS AS THEY APPEAR (I.E. NUMBER OF REPETITIONS)
# cut_repeats(c(1,1,1,1,2,3,3,3,2,2,2,1,2,3,4,4,4,5))
	# [[1]]
	# [1] 1 2 3 2 1 2 3 4 5

	# [[2]]
	# [1] 4 1 3 3 1 1 1 3 1

# values in the exclude array will be cut and not counted.

cut_repeats <- function(array, exclude=c()) {

	j = 1
	for (j in 1:length(array)) {
		if	(is.element(array[j], exclude)) {
			j <- j + 1
		} else {
			break
		}
	}
	
	if (j < length(array) ) {
	
		return_array <- array[j]
		return_counts <- c()
		count <- 1
		for (i in (j+1):length(array)) {
			if ( (array[i] != return_array[length(return_array)]) & (! is.element(array[i], exclude)) ){ 
				return_array <- c(return_array, array[i])
				return_counts <- c(return_counts, count) 
				count <- 1
			} else if (! is.element(array[i], exclude)) { count <- count + 1 }# end if
		} # end for
		return_counts <- c(return_counts, count)
	
		list(return_array, return_counts)

	} else if (j == length(array) ) { list(array[j], 1) # in case there is only one item, and it is the last item
	} else	{ list(c(),c()) }

}