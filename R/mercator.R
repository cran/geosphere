# Author: Robert J. Hijmans
# April 2010
# version 0.1
# license GPL3

mercator <- function(p, inverse=FALSE, r=6378137) {
	toRad <- pi / 180 
	if (inverse) {
		p <- .pointsToMatrix(p, checkLonLat=FALSE) 
		lat <- pi/2 - 2 * atan(exp(-p[,2] / r))
		lon <- p[,1] / r 
		return(cbind(lon, lat) / toRad)
	} else {
		p <- .pointsToMatrix(p) * toRad
		y <- log( tan(p[,2]) + (1 / cos(p[,2]))) * r
		x <- p[,1] * r
		return(cbind(x,y))
	}
}


