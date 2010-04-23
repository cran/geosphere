\name{makePoly}

\alias{makePoly}
  
\title{make a polygon}

\description{
Make a polygon by adding intermedate points (vertices) inbetween the points supplied. This is relevant when vertices are relativly far apart. It can assure the shape of the polygon to be accurate, when plotted on a plane, is accurate.
}

\usage{
makePoly(p, interval=10000, r=6378137, sp=FALSE)
}

\arguments{
\item{p}{a 2-column matrix (longitude/latitude)}
\item{interval}{maximum interval of points, in units of r}
\item{r}{radius of the earth; default = 6378137}  
\item{sp}{Logical. If \code{TRUE}, a SpatialPolygons object is retunred (depends on the 'sp' package)}
}


\value{
A matrix 
}

\author{Robert J. Hijmans }

\examples{
pol <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
plot(pol)
lines(pol, col='red', lwd=3)
pol2 = makePoly(pol, interval=100000)
lines(pol2, col='blue', lwd=2)
}

\keyword{methods}
\keyword{spatial}
