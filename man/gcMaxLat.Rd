\name{gcMaxLat}
\Rdversion{1.1}

\alias{gcMaxLat}


\title{
Highest latitude on a great circle
}

\description{
What is northern most point that will be reached when following a great circle? Computed with Clairaut's formula. The southern most point is the \code{\link[geosphere]{antipode}} of the northern-most point.
}

\usage{
gcMaxLat(p1, p2)
}

\arguments{
  \item{p1}{longitude/latitude of point(s), in degrees; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{p2}{as above. Should have same length as p1, or a single point (or vice versa when p1 is a single point}
}


\value{
A matrix with coordinates (longitude/latitude)
}

\references{
\url{http://williams.best.vwh.net/ftp/avsig/avform.txt}

\url{http://www.movable-type.co.uk/scripts/latlong.html}
}

\author{
Ed Williams, Chris Veness, Robert Hijmans
}

\seealso{
\code{\link[geosphere]{gcLat}, \link[geosphere]{gcLon}}
}


\examples{
gcMaxLat(c(5,52), c(-120,37))
}

\keyword{ spatial  }

