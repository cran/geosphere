\name{destPoint}
\Rdversion{1.1}
\alias{destPoint}

\title{
Destination given bearing and distance, when following a great circle
}

\description{
Calculate the destination point travelling along a (shortest distance) great circle arc, given a start point, initial bearing, and distance.
}

\usage{
destPoint(p, brng, d, r = 6378137)
}

\arguments{
  \item{p}{longitude/latitude of point(s), in degrees; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{brng}{bearing}
  \item{d}{distance}
  \item{r}{radius of the earth; default = 6378137 m}  
}


\note{
The bearing changes continuously when traveling along a great circle line. Therefore, thbe final bearing is not the same as the initial bearing. You can comute the final bearing with \code{finalBearing} (see examples, below)
}


\value{
A pair of coordinates (longitude/latitude)
}

\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}

\url{http://williams.best.vwh.net/ftp/avsig/avform.txt}
}

\author{
Chris Veness; ported to R by Robert Hijmans
}


\examples{
p <- c(5,52)
d <- destPoint(p,30,10000)

#final bearing, when arriving at endpoint: 
finalBearing(d, p)

}

\keyword{ spatial  }

