# author of original JavaScript code: Chris Vennes
# (c) 2002-2009 Chris Veness
# http://www.movable-type.co.uk/scripts/latlong.html
# Licence: LGPL, without any warranty express or implied

# Much of the above is based on formulae by Ed Williams
# http://williams.best.vwh.net/avform.htm

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# License GPL3


destPoint <- function(p, brng, d, r=6378137) {
# calculate destination point given start point, initial bearing (deg) and distance (km)
# see http:#//williams.best.vwh.net/avform.htm#LL
# source http://www.movable-type.co.uk/scripts/latlong.html
# (c) 2002-2009 Chris Veness
	toRad <- pi / 180 

	p <- .pointsToMatrix(p) * toRad
	lon1 <- p[,1] 
	lat1 <- p[,2]

	brng <- brng * toRad

	lat2 <- asin( sin(lat1)*cos(d/r) + cos(lat1)*sin(d/r)*cos(brng) )
	lon2 <- lon1 + atan2(sin(brng)*sin(d/r)*cos(lat1), cos(d/r)-sin(lat1)*sin(lat2))
	lon2 <- (lon2+pi)%%(2*pi) - pi  #// normalise to -180...+180
	lon2[is.nan(lon2)] <- NA
	lat2[is.nan(lat2)] <- NA

	res <- cbind(lon2, lat2) / toRad
	colnames(res) <- c('lon', 'lat')
	return(res)
}

