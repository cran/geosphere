# author Robert Hijmans
# October 2009
# version 0.1
# license GPL


gcLat <- function(p1, p2, lon) {
# Intermediate points on a great circle
# source: http://williams.best.vwh.net/avform.htm
	toRad <- pi / 180 
	d <- distCosine(p1, p2)
	p1 <- .pointsToMatrix(p1)
	p2 <- .pointsToMatrix(p2)

	if (nrow(p1) > 1 | nrow(p2) > 1) {
		stop('provide single points')
	}

	if (antipodal(p1, p2)) {
		stop('you provided antipodal points; these have an infinite number of great circles')
	}

	lon1 <- p1[,1] * toRad
	lat1 <- p1[,2] * toRad
	lon2 <- p2[,1] * toRad
	lat2 <- p2[,2] * toRad
	lon <- lon * toRad
	
	meridians <- sin(lon1-lon2)==0
	if (! all(! meridians)) {
		stop("cannot compute this for a meridian")
	}

	lat <- atan((sin(lat1)*cos(lat2)*sin(lon-lon2) -sin(lat2)*cos(lat1)*sin(lon-lon1))/(cos(lat1)*cos(lat2)*sin(lon1-lon2)))
	return(lat / toRad)
}
