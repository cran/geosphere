# based on code by Ed Williams
# Licence: GPL
# http://williams.best.vwh.net/avform.htm#XTE

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# license GPL3

crossTrackDistance <- function(p1, p2, p3, r=6378137) {
	toRad <- pi / 180 

	tc <- bearing(p1, p2) * toRad
	tcp <- bearing(p1, p3) * toRad
    dp <- distCosine(p1, p3, r=1)
	xtr <- asin(sin(tcp-tc) * sin(dp)) * r

	if (is.vector(xtr)) { xtr <- matrix(xtr) }
	colnames(xtr) <- 'distance'
	
	return(xtr)
}

