# Author: Robert J. Hijmans and Jacob van Etten
# October 2009
# version 0.1
# license GPL3


.compareDim <- function(p1, p2, p3) {
	if(dim(p1)[1] != dim(p2)[1]) {
		if(dim(p1)[1] > 1 & dim(p2)[1] > 1) {
			stop('p1 and p2 do not have the same number of points and neither has only a single point')
		}
	}
	if (! missing(p3)) {
		if(dim(p1)[1] != dim(p3)[1]) {
			if(dim(p1)[1] > 1 & dim(p3)[1] > 1) {
				stop('p1 and p3 do not have the same number of points and neither has only a single point')
			}
		}
		if(dim(p2)[1] != dim(p3)[1]) {
			if(dim(p2)[1] > 1 & dim(p3)[1] > 1) {
				stop('p2 and p3 do not have the same number of points and neither has only a single point')
			}
		}
	}
	return(invisible(TRUE))
}

