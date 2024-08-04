pct_mult <- function(x){
	x <- sort(x, decreasing = T)
	temp <- x[[1]]
	for(i in 2:length(x)){
		temp <- temp + prod(x[1:i])
	}
	return(temp)
}
