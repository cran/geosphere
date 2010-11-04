# author Robert Hijmans
# October 2009
# version 0.1
# license GPL

gcIntermediate <- function( p1, p2, n=50, breakAtDateLine=FALSE, addStartEnd=FALSE, sp=FALSE ) {
# Intermediate points on a great circle
# source: http://williams.best.vwh.net/avform.htm

	interm <- function(p1, p2, n) {
		if (antipodal(p1, p2)) {
			return(rep(Inf, nrow(p1)))
		}
		if (isTRUE(all.equal(p1, p2))) {
			return(cbind(rep(p1[,1], nrow(p1)), rep(p1[,2], nrow(p1)) ))
		}
	
		d <- distCosine(p1, p2, r=1)

		lon1 <- p1[,1] * toRad
		lat1 <- p1[,2] * toRad
		lon2 <- p2[,1] * toRad
		lat2 <- p2[,2] * toRad

		n <- max(round(n), 1)
		f <- 1:n / (n+1)
	
		A <- sin(1-f)*d / sin(d)
		B <- sin(f*d) / sin(d)
		x <- A*cos(lat1)*cos(lon1) +  B*cos(lat2)*cos(lon2)
		y <- A*cos(lat1)*sin(lon1) +  B*cos(lat2)*sin(lon2)
		z <- A*sin(lat1)           +  B*sin(lat2)
		lat <- atan2(z,sqrt(x^2+y^2))
		lon <- atan2(y,x)
	
		gc <- cbind(lon,lat)/toRad
		return(gc)
	}

	toRad <- pi / 180 
	p1 <- .pointsToMatrix(p1)
	p2 <- .pointsToMatrix(p2)
	p <- cbind(p1[,1], p1[,2], p2[,1], p2[,2], as.vector(n))
	p1 <- p[,1:2, drop=FALSE]
	p2 <- p[,3:4, drop=FALSE]
	n <- pmax(n, p[,5])
	res <- list()

	for (i in 1:nrow(p1)) {
		x <- interm(p1[i,,drop=FALSE], p2[i,,drop=FALSE], n[i])
		if (addStartEnd) {
			x <- rbind(p1[i,,drop=FALSE], x, p2[i,,drop=FALSE])
		}
		if (breakAtDateLine) {
	
			r <- range(x[,1]) 
			r <- r[2] - r[1]
			if (r > 200) {
				dif <- abs(x[-nrow(x),1] - x[-1,1])
				tr <- which(dif==max(dif))
				x1 <- x[1:tr, ,drop=FALSE]
				x2 <- x[(tr+1):nrow(x), ,drop=FALSE]
				if (x1[tr,1] < 0) { 
					x1[tr,1] <- -180 
					x2[1,1] <- 180
				} else { 
					x1[tr,1] <- 180 
					x2[1,1] <- -180 
				}
				res[[i]] <- list(x1, x2)
			} else {
				res[[i]] <- x
			}
		} else {
			res[[i]] <- x
		}
	}
	
	if (sp) {
		for (i in 1:length(res)) {
			if (! is.list(res[[i]])) {
				res[[i]] <- Lines( list( Line (res[[i]])), ID=as.character(i)) 	
			} else {
				res[[i]] <- Lines( list( Line (res[[i]][[1]]), Line(res[[i]][[2]])), ID=as.character(i))
			}
		}
		
		res <- SpatialLines(res, CRS("+proj=longlat +ellps=WGS84"))
		
	} else if (nrow(p1) == 1 ) {
		res <- res[[1]]
	}	
	
	return(res)
}


