\name{bearing}
\Rdversion{1.1}

\alias{bearing}

\title{
bearing of travel
}

\description{
Get the initial bearing (direction; azimuth) to go from \code{p1} to \code{p2} following the shortest path (a Great Circle). Note that the bearing of travel changes continuously while going along a great circle. A route with constant bearing is a rhumb line (see  \code{\link[geosphere]{bearingRhumb}}).
}

\usage{
bearing(p1, p2)
}

\arguments{
  \item{p1}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{p2}{as above}
}


\value{
A bearing in degrees
}


\author{
Robert Hijmans
}


\seealso{
\code{ \link[geosphere]{bearingRhumb}  }
}

\examples{
bearing(c(10,10),c(20,20))
}

\references{
\url{http://williams.best.vwh.net/ftp/avsig/avform.txt}

\url{http://www.movable-type.co.uk/scripts/latlong.html}
}


\keyword{ spatial }

