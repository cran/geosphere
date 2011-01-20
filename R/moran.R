# Robert Hijmans
# January 2011
# version 1
# License GPL3


.GearyC <- function(p, v) {
	p <- geosphere:::.pointsToMatrix(p)
	v <- as.vector(v)
	stopifnot(length(v) > 1)
	stopifnot(length(v) == nrow(p))
	n = length(v)

	# (Xi - Xj)^2
	x = matrix(0, ncol=n, nrow=n)
	for (i in 2:n) {
		j = 1:(i-1)
		x[i,j] = (v[i] - v[j])^2
	}
	x <- as.vector(as.dist(x))
	
	# spatial weights
	w <- as.vector(as.dist(1 / (distm(p)/1000)))
	x <- sum( w * x ) * (n-1)
	svm <- sum( (v - mean(v))^2 )
	y <- 2 * sum(w) * svm

	x/y
}


.MoranI <- function(p, v, rsa=FALSE) {
	p <- geosphere:::.pointsToMatrix(p)
	v <- as.vector(v)
	stopifnot(length(v) > 1)
	stopifnot(length(v) == nrow(p))

	# spatial weights
	w <- 1 / (distm(p)/1000)
	Xd <- v - mean(v)
	X <- (Xd %o% Xd)

	if (rsa) {
		# as in ape::Moran.I 
		diag(w) <- 0
		rs <- rowSums(w)
		rs[rs == 0] <- 1
		w <- w/rs

	} else {
		w <- as.vector(as.dist(w))
		X <- as.vector(as.dist(X))
	}
	
	X <- sum( w * X ) 
	Xd <- sum( Xd^2 )
	( nrow(p) / sum(w) ) * ( X / Xd )
}

