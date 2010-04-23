# author Robert Hijmans
# April 2010
# version 0.1
# license GPL



.makeSinglePoly <- function(p, interval=10000, r=6378137, sp=FALSE) {
	res <- p[1,]
	for (i in 1:(nrow(p)-1)) {
		d <- distCosine(p[i,], p[i+1,], r=r)
		n <- floor(d / interval)
		if (n > 0) {
			pts <- gcIntermediate(p[i,],p[i+1,], n)
			pts <- rbind(p[i,], pts, p[i+1,])
			res <- rbind(res, pts, p[i+1,])
		}
	}
	return(res)
}
 
 
makePoly <- function(p, interval=10000, r=6378137, sp=FALSE) {
	if (inherits(p, 'SpatialPolygons')) {
		x = p@polygons
		n = length(x)
		polys = list()
		for (i in 1:n) {
			parts = length(x[[i]]@Polygons )
			sumarea = 0
			partlist = list()
			for (j in 1:parts) {
				crd = x[[i]]@Polygons[[j]]@coords
				crd = .makeSinglePoly(crd, interval=interval, r=r, sp=FALSE)
				partlist[[j]] = Polygon(crd)
			}
			polys[[i]] = Polygons(partlist, i)
		}
		polys <- SpatialPolygons(polys)
		if (inherits(p, 'SpatialPolygonsDataFrame')) {
			polys <- SpatialPolygonsDataFrame(polys, p@data)	
		}
		return(polys)
	} else {
		p <- .pointsToMatrix(p)
		if (nrow(p) < 3) {
			stop('cannot make a polygon (insufficent number of vertices)')
		}
		if (! isTRUE(all.equal(p[1,], p[nrow(p),]))) {
			p <- rbind(p, p[1,])
		}
		res <- .makeSinglePoly(p, interval=interval, r=r, sp=sp) 
		if (sp) {
			if (! require(sp) ) {
				stop("you need to install the 'sp' package to have the result returned as an sp object (or use sp=FALSE)")
			}
			res <- SpatialPolygons(list(Polygons(list(Polygon(res)), 1)))		
		}
		return(res)
	}
} 

