# author Robert Hijmans
# October 2009
# version 0.1
# license GPL


greatCircle <- function(p1, p2, n=360) {
	n <- max(round(n), 2)
	lon <- (1:n * 360 / n) - 180
	lat <- gcLat(p1, p2, lon) 
	return( cbind(lon,lat) )
}

