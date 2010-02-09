# author Robert Hijmans
# October 2009
# version 0.1
# license GPL3

# based on an alogrithm described by Ed Williams
# http://williams.best.vwh.net/intersect.htm


# Not used
#gete <- function(lon, lat) {
#	ex <- cos(lat)*cos(lon)
#	ey <- -cos(lat)*sin(lon)
#	ez <- sin(lat)
#	return(cbind(ex, ey, ez))
#}


gcIntersect <- function(p1, p2, p3, p4) {
#intersection of two great circles defined by pt1 to pt2 and pt3 to pt4.

	modlon <- function(lon) { ((lon + pi) %% (2*pi)) - pi  }


	einv <- function(e) {
		lat <- atan2(e[,3], sqrt(e[,1]^2 + e[,2]^2))
		lon <- atan2(-e[,2], e[,1]) 
		return(cbind(lon, lat))
	}

	eXe5 <- function(lon1, lat1, lon2, lat2) {
	    ex <- sin(lat1-lat2) *sin((lon1+lon2)/2) *cos((lon1-lon2)/2) - sin(lat1+lat2) *cos((lon1+lon2)/2) *sin((lon1-lon2)/2) 
		ey <- sin(lat1-lat2) *cos((lon1+lon2)/2) *cos((lon1-lon2)/2) + sin(lat1+lat2) *sin((lon1+lon2)/2) *sin((lon1-lon2)/2) 
		ez <- cos(lat1)*cos(lat2)*sin(lon1-lon2) 
		return( cbind(ex, ey, ez) )
	}

	eXe3 <- function(e1, e2) {
		x <- e1[,2] * e2[,3] -e2[,2] *e1[,3]
		y <- e1[,3] *e2[,1] -e2[,3] *e1[,1]
		z <- e1[,1] *e2[,2] -e1[,2] *e2[,1]
		return(cbind(x,y,z))
	}

	eSQRT <- function(e) {
		return(sqrt(e[,1]^2 + e[,2]^2 + e[,3]^2))
	}	

	
	p1 <- .pointsToMatrix(p1)
	p2 <- .pointsToMatrix(p2)
	p3 <- .pointsToMatrix(p3)
	p4 <- .pointsToMatrix(p4)

    .compareDim(p1, p2)
    .compareDim(p1, p3)
    .compareDim(p1, p4)
    .compareDim(p2, p3)
    .compareDim(p2, p4)
    .compareDim(p3, p4)

	anti <- antipodal(p1, p2)
	if (! all(! anti)) { stop('p1 and p2 are antipodal -- cannot define a Great Circle') }
	anti <- antipodal(p3, p4)
	if (! all(! anti)) { stop('p3 and p4 are antipodal -- cannot define a Great Circle') }

	toRad <- pi / 180 
	p1 <- p1 * toRad
	p2 <- p2 * toRad
	p3 <- p3 * toRad
	p4 <- p4 * toRad
	
	e1Xe2 <- eXe5(p1[,1], p1[,2], p2[,1], p2[,2])
	e3Xe4 <- eXe5(p3[,1], p3[,2], p4[,1], p4[,2])

	ea <- e1Xe2  / eSQRT(e1Xe2)
	eb <- e3Xe4  / eSQRT(e3Xe4)
	
	eaXeb <- eXe3(ea, eb)
	
	ll <- einv(eaXeb)
	ll2 <- cbind(ll[,1] + pi, -ll[,2])
	pts <- cbind(ll, ll2)
	pts[,1] <- modlon(pts[,1])
	pts[,3] <- modlon(pts[,3])
	
	pts <- pts / toRad
	colnames(pts) <- c('lon1', 'lat1', 'lon2', 'lat2')
	rownames(pts) <- NULL
	return(pts)
 }
 
 