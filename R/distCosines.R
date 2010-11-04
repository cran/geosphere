# Author: Robert J. Hijmans
# Date :  June 2008
# Licence GPL v3

# distance based on law of cosines
# http://en.wikipedia.org/wiki/Great_circle_distance

distCosine <- function(p1, p2, r=6378137) {
	toRad <- pi / 180 
	
	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad
	
	p = cbind(p1[,1], p1[,2], p2[,1], p2[,2], as.vector(r))
	
	z <- isTRUE(p1[,1] == p2[,1] &  p1[,2] == p2[,2]) # distance is zero

	lon1 <- p[,1]
	lat1 <- p[,2]
	lon2 <- p[,3]
	lat2 <- p[,4]
	r <- p[,5]

	cosd <- sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon1-lon2)
	dist <- matrix(nrow=nrow(p), ncol=1)
	dist[z] <- 0
	dist[!z] <- r[!z] * acos(cosd[!z]) 

	colnames(dist) <- 'distance'
	return(dist)
}

