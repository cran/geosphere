# Author: Robert J. Hijmans
# April 2010
# version 0.1
# license GPL3

mercator <- function(p, r=6378137) {
	toRad <- pi / 180 
	p <- .pointsToMatrix(p) * toRad
	y = log( tan(p[,2]) + (1 / cos(p[,2]))) * r
	x = p[,1] * r
	return(cbind(x,y))
}


