# author Robert Hijmans
# October 2009
# version 0.1
# license GPL

gcIntermediate <- function(p1, p2, n=50) {
# Intermediate points on a great circle
# source: http://williams.best.vwh.net/avform.htm
	toRad <- pi / 180 
	n = n[1]
	
	p1 <- .pointsToMatrix(p1)
	p2 <- .pointsToMatrix(p2)

	if (nrow(p1) > 1 | nrow(p2) > 1) {
		stop('provide single points')
	}

	if (antipodal(p1, p2)) {
		stop('you provided antipodal points; these have an infinite number of great circles')
	}

	d <- distCosine(p1, p2, r=1)

	lon1 <- p1[,1] * toRad
	lat1 <- p1[,2] * toRad
	lon2 <- p2[,1] * toRad
	lat2 <- p2[,2] * toRad

	n <- max(round(n), 1)
	f <- 1:n / (n+1)
	
    A <- sin(1-f)*d / sin(d)
    B <- sin(f*d) / sin(d)
    x <- A*cos(lat1)*cos(lon1) +  B*cos(lat2)*cos(lon2)
	y <- A*cos(lat1)*sin(lon1) +  B*cos(lat2)*sin(lon2)
	z <- A*sin(lat1)           +  B*sin(lat2)
    lat <- atan2(z,sqrt(x^2+y^2))
	lon <- atan2(y,x)
	
	gc <- cbind(lon,lat)/toRad
	return(gc)
}

