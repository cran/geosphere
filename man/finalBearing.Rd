\name{finalBearing}
\Rdversion{1.1}

\alias{finalBearing}

\title{
Final bearing
}

\description{
Get the final bearing when arriving at point2 after starting from p1 and following the shortest path (a great circle).
}

\usage{
finalBearing(p1, p2)
}

\arguments{
  \item{p1}{longitude/latitude of point(s), in degrees; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{p2}{as above. Should have same length as p1, or a single point (or vice versa when p1 is a single point}
}


\value{
A bearing in degrees
}


\author{
Robert Hijmans
}


\seealso{
\code{ \link[geosphere]{bearing}  }
}

\examples{
bearing(c(0,0),c(90,90))
finalBearing(c(0,0),c(90,90))
}

\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}

\url{http://williams.best.vwh.net/ftp/avsig/avform.txt}
}


\keyword{ spatial }

