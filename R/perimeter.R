# Robert Hijmans
# October 2009
# version 0.1
# License GPL3


.perimeterFromSpatialPolygons <- function(xy, r) {
	p = xy@polygons
	n = length(p)
	res = vector(length=n)
	for (i in 1:n) {
		parts = length( p[[i]]@Polygons )
		perim = 0
		for (j in 1:parts) {
			if (p[[i]]@Polygons[[j]]@hole) {
				# do nothing
			} else {
				crd = p[[i]]@Polygons[[j]]@coords
				perim = perim + perimeter(crd, r)
			}
		}
		res[i] = perim
	}
	return(res)
}

.lenghtFromSpatialLines <- function(xy, r) {
	p = xy@lines
	n = length(p)
	res = vector(length=n)
	for (i in 1:n) {
		parts = length( p[[i]]@Lines )
		lng = 0
		for (j in 1:parts) {
			crd = p[[i]]@Lines[[j]]@coords
			lng = lng + perimeter(crd, r)
		}
		res[i] = lng
	}
	return(res)
}


perimeter <- function(xy, r=6378137) {

	if (inherits(xy, 'SpatialPolygons')) {
		return( .perimeterFromSpatialPolygons(xy, r))
	} else if (inherits(xy, 'SpatialLines')) {
		return( .lenghtFromSpatialLines(xy, r))
	}

	xy2 = rbind(xy[-1,], xy[1,])
	d = distCosine(xy, xy2, r=r)
	return(sum(d))
}

