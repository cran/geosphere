# author Robert Hijmans
# October 2009
# version 0.1
# license GPL3

# based on
#http://williams.best.vwh.net/avform.htm#Par

gcLon <- function(p1, p2, lat) {
# longitudes at which a given great circle crosses a given parallel
# source: http://williams.best.vwh.net/avform.htm

	modlon <- function(lon) { ((lon + pi) %% (2*pi)) - pi  }
	
	toRad <- pi / 180 
	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad

	.compareDim(p1, p2)
		
	lon1 <- p1[,1] * -1
	lat1 <- p1[,2] 
	lon2 <- p2[,1] * -1
	lat2 <- p2[,2]
	lat3 <- lat * toRad
	
	l12 = lon1-lon2
	A <- sin(lat1)*cos(lat2)*cos(lat3)*sin(l12)
	B <- sin(lat1)*cos(lat2)*cos(lat3)*cos(l12) - cos(lat1)*sin(lat2)*cos(lat3)
	C <-  cos(lat1)*cos(lat2)*sin(lat3)*sin(l12)
	lon <- atan2(B,A)   
	
	lon3 <- matrix(NA, nrow=length(lon1), ncol=2)
	
	i <- (abs(C) > sqrt(A^2 + B^2)) | (sqrt(A^2 + B^2) == 0)
	lon3[i,] <- NA
	i <- !i
	
	dlon <- A
	dlon[] <- NA
	dlon[i] = acos(C[i]/sqrt(A[i]^2+B[i]^2))
	lon3[i,1] <- modlon(lon1[i]+dlon[i]+lon[i])
	lon3[i,2] <- modlon(lon1[i]-dlon[i]+lon[i])
	
	lon3 <- lon3  / toRad
	colnames(lon3) <- c('lon1', 'lon2')
	return(lon3 * -1)
}

