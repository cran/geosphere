# Author: Robert J. Hijmans
# April 2010
# version 1
# license GPL3


.normalizeLonDeg <- function(x) {
	x <- (x + 180) %% 360 - 180
}

.normalizeLonRad <- function(x) {
	x <- (x + pi) %% (2*pi) - pi 
}


.isPolygon <- function(x) {
	if (nrow(x) < 4) {
		stop('this is not a polygon (insufficent number of vertices)')
	}
	if (! isTRUE(all.equal(x[1,], x[nrow(x),]))) {
		stop('this is not a valid (closed) polygon (first vertex must be equal to the last vertex)')
	}
	if (! all(!(is.na(x))) ) {
		stop('polygon has NA values)')
	}
	return(TRUE)
}
