# author Robert Hijmans
# October 2009
# version 0.1
# license GPL3

# based on formulae by Ed Willians at
# http://williams.best.vwh.net/avform.htm#Intersection

rhumbIntersect <- function(p1, brng1, p2, brng2) {
#crs13 true bearing from point 1 and the crs23 true bearing from point 2:
	toRad <- pi / 180 
	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad

	p <- cbind(p1[,1], p1[,2], p2[,1], p2[,2], as.vector(brng1), as.vector(brng2))
	lon1 <- p[,1]
	lat1 <- p[,2]
	lon2 <- p[,3]
	lat2 <- p[,4]
	lat1[lat1==90|lat1==-90] <- NA
	lat2[lat2==90|lat2==-90] <- NA

	crs13 <- p[,5] * toRad
	crs23 <- p[,6] * toRad

	dst12 <- 2*asin(sqrt((sin((lat1-lat2)/2))^2+ cos(lat1)*cos(lat2)*sin((lon1-lon2)/2)^2))
	g <- sin(lon2-lon1) < 0 
	crs12 <- vector(length=g)
	crs21 <- crs12
	
	crs12[g] <- acos((sin(lat2[g])-sin(lat1[g])*cos(dst12[g]))/(sin(dst12[g])*cos(lat1[g])))
	crs21[g] <- 2*pi - acos((sin(lat1[g])-sin(lat2[g])*cos(dst12[g]))/(sin(dst12[g])*cos(lat2[g])))
	crs12[!g] <- 2*pi-acos((sin(lat2[!g])-sin(lat1[!g])*cos(dst12[!g]))/(sin(dst12[!g])*cos(lat1[!g])))
	crs21[!g] <- acos((sin(lat1[!g])-sin(lat2[!g])*cos(dst12[!g]))/(sin(dst12[!g])*cos(lat2[!g])))
	
	ang1 <- (crs13-crs12+pi %% 2*pi) - pi
	ang2 <- (crs21-crs23+pi %% 2*pi) - pi

	lon3 <- vector(length=length(crs12))
	lat3 <- lon3
	g <- sin(ang1)==0 & sin(ang2)==0 
	h <- (sin(ang1) * sin(ang2)) < 0
	i <- !g & !h

	lon3[g] <- Inf
	lat3[g] <- Inf
	lon3[h] <- NA
	lat3[h] <- NA
	ang1 <- abs(ang1)
	ang2 <- abs(ang2)
	ang3 <- acos(-cos(ang1)*cos(ang2)+sin(ang1)*sin(ang2)*cos(dst12)) 
	dst13 <- atan2(sin(dst12)*sin(ang1)*sin(ang2),cos(ang2)+cos(ang1)*cos(ang3))
	lat3[i] <- asin(sin(lat1[i])*cos(dst13[i])+cos(lat1[i])*sin(dst13[i])*cos(crs13[i]))
	dlon <- atan2(sin(crs13)*sin(dst13)*cos(lat1),cos(dst13)-sin(lat1)*sin(lat3))
	lon3[i] <- lon1[i]-dlon[i]
	
	lon3 <- (lon3+pi)%%(2*pi) - pi  #// normalise to -180...+180
	
	ll <- cbind(lon3, lat3) / toRad
	colnames(ll) = c('lon', 'lat')
	return(ll)
}

