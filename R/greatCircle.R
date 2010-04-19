# author Robert Hijmans
# October 2009
# version 0.1
# license GPL


greatCircle <- function(p1, p2, n=360) {
	p1 <- .pointsToMatrix(p1)
	p2 <- .pointsToMatrix(p2)
	if (nrow(p1) > 1) stop('this function can only a single great circle. p1 should have a single point')
	if (nrow(p2) > 1) stop('this function can only a single great circle. p2 should have a single point')
	
	n <- max(round(n), 2)
	lon <- (1:n * 360 / n) - 180
	
	lat <- gcLat(p1, p2, lon) 
	return( cbind(lon,lat) )
}

