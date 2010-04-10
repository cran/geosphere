# Based on code by Jason_Steven
# http://forum.worldwindcentral.com/showthread.php?p=69704

# R implementation by Robert Hijmans

.areaFromSpatial <- function(xy, r) {
	p = xy@polygons
	n = length(p)
	res = vector(length=n)
	for (i in 1:n) {
		parts = length( p[[i]]@Polygons )
		area = 0
		for (j in 1:parts) {
			crd = p[[i]]@Polygons[[j]]@coords
			ar = areaPolygon(crd, r)
			if (p[[i]]@Polygons[[j]]@hole) {
				area = area - ar
			} else {
				area = area + ar
			}
		}
		res[i] = area
	}
	return(res)
}


areaPolygon <- function(xy, r=6378137) {

	if (inherits(xy, 'SpatialPolygons')) {
		return(.areaFromSpatial(xy, r))
	}

	haversine <- function(x) (1-cos(x))/2

	toRad <- pi / 180 
	r = r[1]
	xy = xy * toRad
	lon=xy[,1]
	lat=xy[,2]
    
	lam1 = 0
	lam2 = 0
	beta1 =0
	beta2 = 0
	cosB1 =0
	cosB2 = 0
	hav = 0
    sum = 0

    for (j in 1:length(lat)) {
		if( j == 1 ) {
			lam1 = lon[j];
			beta1 = lat[j];
			lam2 = lon[j + 1];
			beta2 = lat[j + 1];
			cosB1 = cos( beta1 );
			cosB2 = cos( beta2 );
        } else {
			k = j %% length(lat) + 1;
			lam1 = lam2;
			beta1 = beta2;
			lam2 = lon[k];
            beta2 = lat[k];
			cosB1 = cosB2;
			cosB2 = cos( beta2 );
		}
		if ( lam1 != lam2 ) {
			hav = haversine( beta2 - beta1 ) + cosB1 * cosB2 * haversine( lam2 - lam1 );
			a = 2 * asin( sqrt( hav ) );
			b = pi / 2 - beta2;
			c = pi / 2 - beta1;
			s = 0.5 * ( a + b + c );
			t = tan( s / 2 ) * tan( ( s - a ) / 2 ) *  tan( ( s - b ) / 2 ) * tan( ( s - c ) / 2 );
			excess = abs( 4 * atan( sqrt( abs( t ) ) ) );
			if( lam2 < lam1 ) {
				excess = -excess;
			}
			sum = sum + excess;
		}
    }
    return(abs( sum ) * r * r);
}

