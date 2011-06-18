# Author: Robert J. Hijmans
# Date :  June 2008
# Licence GPL v3

# distance based on law of cosines
# http://en.wikipedia.org/wiki/Great_circle_distance

distCosine <- function(p1, p2, r=6378137) {
	p1 <- .pointsToMatrix(p1) 
	p2 <- .pointsToMatrix(p2) 
	p  <- cbind(p1[,1], p1[,2], p2[,1], p2[,2], as.vector(r))
	p[,1:4] <- p[,1:4] * pi / 180 
	as.vector( acos( sin(p[,2]) * sin(p[,4]) + cos(p[,2]) * cos(p[,4]) * cos(p[,1]-p[,3]) ) * p[,5]  )
}

#	n <- nrow(p)
#	d <- vector("double", n)
#	d <- .C('distance', as.integer(n), as.double(p[,1]), as.double(p[,2]), as.double(p[,3]), as.double(p[,4]), as.double(p[,5]), as.integer(1), d)[[8]]
#	return(d)
