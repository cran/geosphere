# Author: Robert J. Hijmans
# Date :  June 2008
# Licence GPL v3

# distance based on law of cosines
# http://en.wikipedia.org/wiki/Great_circle_distance

distCosine <- function(p1, p2, r=6378137) {
	toRad <- pi / 180 
	
	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad
	p  <- cbind(p1[,1], p1[,2], p2[,1], p2[,2], as.vector(r))
	
#	cosd <- sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon1-lon2)
	cosd <- sin(p[,2]) * sin(p[,4]) + cos(p[,2]) * cos(p[,4]) * cos(p[,1]-p[,3])
	return ( as.vector( acos(cosd) * p[,5] ) )
}

