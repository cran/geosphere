# Author: Robert J. Hijmans
# February 2012
# version 1
# license GPL3


geomean <- function(xy, w=NULL) {

	if (inherits(xy, 'SpatialPolygons') | inherits(x, 'SpatialPoints')) {
		stopifnot(isLonLat(x)) 
		xy <- coordinates(xy)
	} 

	xy <- na.omit(xy) 
	xy[,1] <- xy[,1] + 180
	xy <- xy * pi / 180 
	if (is.null(w)) {
		w <- 1
	} else if (length(w) != nrow(xy)) {
			stop('length of weights not correct. It should be: ', nrow(xy))
	}
	w <- w / sum(w)
		
	Sx <- mean(sin(xy[,1]) * w)
	Cx <- mean(cos(xy[,1]) * w)
	x <- atan2(Sx, Cx)
	x <- x %% (2 * pi) - pi
		
	Sy <- mean(sin(xy[,2]) * w)
	Cy <- mean(cos(xy[,2]) * w)
	y <- atan2(Sy, Cy)
		
	cbind(x,y) * 180 / pi
}

