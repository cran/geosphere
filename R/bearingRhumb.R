# author of original JavaScript code: Chris Vennes
# (c) 2002-2009 Chris Veness
# http://www.movable-type.co.uk/scripts/latlong.html
# Licence: LGPL, without any warranty express or implied

# see http://williams.best.vwh.net/avform.htm#Rhumb
# for the original formulae

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# license GPL3


bearingRhumb <- function(p1, p2) {

	toRad <- pi / 180 
	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad
    .compareDim(p1, p2)
  
	lon1 <- p1[,1]
	lat1 <- p1[,2]
	lon2 <- p2[,1]
	lat2 <- p2[,2]

	dLon <- (lon2-lon1)
	dPhi <- log(tan(lat2/2 + pi/4)/tan(lat1/2+pi/4))
	i <- (abs(dLon) > pi)
	j <- i && dLon > 0
	dLon[j] <- -(2*pi-dLon[j])
	j <- i && dLon <= 0
	dLon[j] <- dLon[j] <- (2*pi+dLon[j])
	
	b <- atan2(dLon, dPhi)
	b <- b / toRad
	b <- (b+360) %% 360
	names(b) <- NULL
	return(b)
}
