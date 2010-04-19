# author of original JavaScript code: Chris Vennes
# (c) 2002-2009 Chris Veness
# http://www.movable-type.co.uk/scripts/latlong.html
# Licence: LGPL, without any warranty express or implied

# Much of the above based on formulae by Ed Williams
# http://williams.best.vwh.net/avform.htm

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# License GPL3


midPoint <- function(p1, p2) {
#* calculate midpoint of great circle line between p1 & p2.
#*   see http:#//mathforum.org/library/drmath/view/51822.html for derivation
# source http://www.movable-type.co.uk/scripts/latlong.html
# (c) 2002-2009 Chris Veness
	toRad <- pi / 180 

	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad
	p <- cbind(p1[,1], p1[,2], p2[,1], p2[,2])	
	p1 <- p[, 1:2, drop=FALSE]
	p2 <- p[, 3:4, drop=FALSE]
	
	lon1 <- p1[,1]
	lat1 <- p1[,2]
	lon2 <- p2[,1]
	lat2 <- p2[,2]

	dLon <- (lon2-lon1)

	Bx <- cos(lat2) * cos(dLon)
	By <- cos(lat2) * sin(dLon)

	lat3 <- atan2(sin(lat1)+sin(lat2), sqrt((cos(lat1)+Bx)*(cos(lat1)+Bx) + By*By ) )
	lon3 <- lon1 + atan2(By, cos(lat1) + Bx)

	lon3[is.nan(lon3)] <- NA
	lat3[is.nan(lat3)] <- NA
	
	res <- cbind(lon3, lat3) / toRad
	colnames(res) <- c('lon', 'lat')
	
	return(res)
}
