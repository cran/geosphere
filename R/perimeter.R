# Robert Hijmans
# April 2010
# version 1
# License GPL3

if (!isGeneric("perimeter")) {
	setGeneric("perimeter", function(x, ...)
		standardGeneric("perimeter"))
}	


setMethod("perimeter", signature(x='SpatialPolygons'), 
function(x, r=6378137, ...) {
	x = x@polygons
	n = length(x)
	res = vector(length=n)
	for (i in 1:n) {
		parts = length( x[[i]]@Polygons )
		perim = 0
		for (j in 1:parts) {
			if (! x[[i]]@Polygons[[j]]@hole) {
				crd = x[[i]]@Polygons[[j]]@coords
				perim = perim + perimeter(crd, r=r, ...)
			}
		}
		res[i] = perim
	}
	return(res)
} )


setMethod("perimeter", signature(x='SpatialLines'), 
function(x, r=6378137, ...) {
	x = x@lines
	n = length(x)
	res = vector(length=n)
	for (i in 1:n) {
		parts = length( x[[i]]@Lines )
		lng = 0
		for (j in 1:parts) {
			crd = x[[i]]@Lines[[j]]@coords
			lng = lng + perimeter(crd, r=r, ...)
		}
		res[i] = lng
	}
	return(res)
} )


setMethod("perimeter", signature(x='data.frame'), 
function(x, r=6378137, ...) {
	perimter(as.matrix(x), r, ...)
}  )


setMethod("perimeter", signature(x='matrix'), 
function(x, r=6378137, ...) {
	x <- x[,1:2]
	if (all.equal(x[1,], x[nrow(x),])) {
		x <- x[-nrow(x), ]
	}
	y = rbind(x[-1,], x[1,])
	d = distCosine(x, y, r=r)
	return(sum(d))
} )

