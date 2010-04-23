# Robert Hijmans
# April 2010
# version 1
# License GPL3


distm <- function(x, fun=distCosine) {
	x <- .pointsToMatrix(x)
	n = nrow(x)
	dm = matrix(ncol=n, nrow=n)
	dm[cbind(1:n, 1:n)] = 0
	if (n == 1) {	return(dm) 	}
	for (i in 2:n) {
		j = 1:(i-1)
		dm[i,j] = fun(x[i,], x[j,])
	}
	return(dm)
}

