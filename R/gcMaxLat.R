# Based on formulae by Ed Williams
# http://williams.best.vwh.net/avform.htm

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# License GPL3

gcMaxLat <- function(p1, p2) {
	if (antipodal(p1, p2)) {
		stop('you provided antipodal points; these have an infinite number of great circles')
	}
	toRad <- pi / 180 
	b <- bearing(p1, p2) * toRad
	p1 <- .pointsToMatrix(p1) * toRad
	lat <- p1[,2] 
	lon <- p1[,1] * -1
	
# ‘Clairaut’s formula’ : the maximum latitude of a great circle path, given a bearing and latitude on the great circle
	maxlat <- acos(abs(sin(b) * cos(lat)))

	maxlon <- maxlat
	maxlon[] <- NA
	i <- maxlat==0
	j <- b < pi & !i
	k <- !j & !i
	maxlon[j] <- lon[j] - atan2(cos(b[j]), sin(b[j]) * sin(lat[j]))
	maxlon[k] <- lon[k] + pi - atan2(cos(b[k]), sin(b[k]) * sin(lat[k]))
	maxlon <- (maxlon+pi)%%(2*pi) - pi
 
	res <- cbind(-1 * maxlon, maxlat)/ toRad
	colnames(res) <- c('lon', 'lat')
	rownames(res) <- ''
	return(res)
}

