# Robert Hijmans
# October 2009
# version 0.1
# Licence: GPL3

finalBearing <- function(p1, p2) {
	b <- bearing(p2, p1)
	return ( (b+180) %% 360 )
}	
