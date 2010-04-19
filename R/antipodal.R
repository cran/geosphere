# Author: Robert J. Hijmans
# October 2009
# version 0.1
# license GPL3


antipodal <- function(p1, p2) {
	toRad <- pi / 180 
	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad
	p <- cbind(p1[,1], p1[,2], p2[,1], p2[,2])	
	diflon = round(abs(p[,2] + p[,4]), 12)
	diflat = round(abs(p[,1] - p[,3]) - pi, 12)
	antipodal = (diflat == 0) & (diflon == 0)
	return(antipodal)
}


antipode <- function(p) {
	toRad <- pi / 180 
	p <- .pointsToMatrix(p) * toRad
	lon <- -pi + p[,1]
	lon <- (lon+pi)%%(2*pi) - pi
	lat <- -p[,2]
	return( cbind(lon,lat) / toRad )
}

