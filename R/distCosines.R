# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.8 (taken from Raster package)
# Licence GPL v3

# distance based on law of cosines
# http://en.wikipedia.org/wiki/Great_circle_distance

distCosine <- function(p1, p2, r=6378137) {
	toRad <- pi / 180 
	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad
	.compareDim(p1, p2)
	lon1 <- p1[,1]
	lat1 <- p1[,2]
	lon2 <- p2[,1]
	lat2 <- p2[,2]

	cosd <- sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon1-lon2)
	dist <- r * acos(cosd) 

	if (is.vector(dist)) { dist <- matrix(dist) }
	colnames(dist) <- 'distance'
	
	return(dist)
}


