\name{destPoint}

\Rdversion{1.1}

\alias{destPoint}

\title{
Destination given bearing (direction) and distance, when following a Great Circle
}

\description{
Calculate the destination point travelling along a (shortest distance) Great Circle, given a start point, initial direction (bearing), and distance.
}

\usage{
destPoint(p, b, d, r = 6378137)
}

\arguments{
  \item{p}{Longitude and Latitude of point(s), in degrees. Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{b}{Numeric. Bearing (direction) in degrees}
  \item{d}{Numeric. Distance in the same unit as \code{r} (default is meters)}
  \item{r}{Numeric. Radius of the earth; default = 6378137 m}  
}


\note{
Direction changes continuously when traveling along a Great Circle. Therefore, thbe final direction is not the same as the initial direction. You can compute the final direction with \code{\link[geosphere]{finalBearing}} (see examples, below)
}


\value{
A pair of coordinates (longitude/latitude)
}

\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}

\url{http://williams.best.vwh.net/ftp/avsig/avform.txt}
}

\author{
Robert Hijmans, based on code by Chris Veness
}


\examples{
p <- c(5,52)
d <- destPoint(p,30,10000)

#final direction, when arriving at endpoint: 
finalBearing(d, p)
}

\keyword{ spatial  }

