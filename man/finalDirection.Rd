\name{finalBearing}
\Rdversion{1.1}

\alias{finalBearing}

\title{
Final direction
}

\description{
Get the final direction (bearing) when arriving at \code{p2} after starting from \code{p1} and following the shortest path (a great circle).
}

\usage{
finalBearing(p1, p2)
}

\arguments{
  \item{p1}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{p2}{as above}
}


\value{
A direction (bearing) in degrees
}


\author{
Robert Hijmans
}


\seealso{
\code{ \link[geosphere]{bearing}  }
}

\examples{
bearing(c(10,10),c(20,20))
finalBearing(c(10,10),c(20,20))
}

\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}

\url{http://williams.best.vwh.net/ftp/avsig/avform.txt}
}


\keyword{ spatial }

