\name{midPoint}

\Rdversion{1.1}

\alias{midPoint}

\title{Mid-point}

\description{
Mid-point between two points along a great circle
}

\usage{
midPoint(p1, p2)
}


\arguments{
  \item{p1}{longitude/latitude of point(s), in degrees; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{p2}{as above. Should be of same length of p1, or a single point (or vice versa when p1 is a single point}
}

\details{
Just as the initial bearing may vary from the final bearing, the midpoint may not be located half-way between latitudes/longitudes; the midpoint between 35N,45E and 35N,135E is around 45N,90E.}


\value{
A pair of coordinates (longitude/latitude)
}

\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}

\url{http://en.wikipedia.org/wiki/Great_circle_distance}
}

\author{
Chris Veness; ported to R by Robert Hijmans
}

\examples{
midPoint(c(0,0),c(90,90))  
}

\keyword{ spatial  }

