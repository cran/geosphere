\name{distm}
\Rdversion{1.1}

\alias{distm}

\title{
Distance matrix
}

\description{
Distance matrix of a set of points
}

\usage{
distm(x, fun=distCosine)
}

\arguments{
  \item{x}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{fun}{A function to compute distances}  
}


\value{
Matrix of distances
}

\references{
\url{http://en.wikipedia.org/wiki/Great_circle_distance}
}

\author{
Robert Hijmans
}

\seealso{
\code{\link[geosphere]{distCosine}, \link[geosphere]{distHaversine}, \link[geosphere]{distVincentySphere}, \link[geosphere]{distVincentyEllipsoid}}
}

\examples{
distm(rbind(c(0,0),c(90,90),c(10,10),c(-120,-45)))
}

\keyword{ spatial }
