# based on code by Ed Williams
# licence GPL
# http://williams.best.vwh.net/avform.htm#XTE

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# license GPL3

alongTrackDistance <- function(p1, p2, p3, r=6378137) {
	toRad <- pi / 180 

	tc <- bearing(p1, p2) * toRad
	tcp <- bearing(p1, p3) * toRad
    dp <- distCosine(p1, p3, r=1)
	xtr <- asin(sin(tcp-tc) * sin(dp))

# +1/-1 for ahead/behind [lat1,lon1]
	direction <- sign(cos(tc - tcp))  
	dist <- direction * acos(cos(dp) / cos(xtr)) * r
	
	if (is.vector(dist)) { dist <- matrix(dist) }
	colnames(dist) <- 'distance'
	
	return(dist)
}
