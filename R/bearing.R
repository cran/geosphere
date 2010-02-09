# author of original JavaScript code: Chris Vennes
# (c) 2002-2009 Chris Veness
# http://www.movable-type.co.uk/scripts/latlong.html
# Licence: LGPL, without any warranty express or implied

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# Licence: GPL3

bearing <- function(p1, p2) {
#calculate (initial) bearing between two points
	toRad <- pi / 180 
	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad
    .compareDim(p1, p2)
	lon1 <- p1[,1]
	lat1 <- p1[,2]
	lon2 <- p2[,1]
	lat2 <- p2[,2]

	dLon <- (lon2-lon1)
	y <- sin(dLon) * cos(lat2)
	x <- cos(lat1)*sin(lat2) - sin(lat1)*cos(lat2)*cos(dLon)
	b <- atan2(y, x) / toRad
	names(b) <- 'bearing'
	return( (b+360) %% 360 )
}
